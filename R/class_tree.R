#' Cohort logic tree node
#'
#' An n-ary tree representing a single cohort's definition. Each node is one of:
#'
#'   * `"base"`   - the seed population (all patients of the dataset). No children.
#'   * `"filter"` - a leaf clause selecting from the base population. No children.
#'                  Carries the clause fields (`rdv`, `column`, `query_type`,
#'                  `inclusion`, `window`, and `value` = the query value).
#'   * `"and"`    - intersection of its children (>= 1 children).
#'   * `"or"`     - union of its children (>= 1 children).
#'   * `"not"`    - complement (against the base) of its single child.
#'
#' Evaluating the tree (`$evaluate(df_rdvs)`) resolves it to a `patient_list`
#' tibble (`project_id`, `entry_date`, `exit_date`) - the same object the linear
#' tibble pipeline in `utils_segment_cohorts.R` produces. The filter leaves reuse
#' the existing package closures (`str_regex_closure`, `ff_concurrent`,
#' `ff_crop_*`, `merge_contiguous`) so query/temporal semantics are identical.
#'
#' @export
Node <- R6::R6Class("Node",
  public = list(

    #' @field value_type One of "base", "filter", "and", "or", "not".
    value_type = NULL,
    #' @field value For "filter" leaves, the query value(s). NULL otherwise.
    value = NULL,
    #' @field rdv For "filter" leaves, the RDV code (e.g. "dia_conditions").
    rdv = NULL,
    #' @field column For "filter" leaves, the RDV column to query.
    column = NULL,
    #' @field query_type For "filter" leaves: str_starts / str_matches /
    #'   str_contains / date_between / age_between / numeric_between.
    query_type = NULL,
    #' @field inclusion For "filter" leaves: temporal rule (fully_concurrent,
    #'   after_first, on_first, ever, never).
    inclusion = NULL,
    #' @field window For "filter" leaves, the crop window c(start_days, end_days).
    window = NULL,
    #' @field children list() of child Nodes (operators only).
    children = NULL,
    #' @field patient_list Cached resolved membership (set by $evaluate()).
    patient_list = NULL,
    #' @field n Cached distinct patient count (set by $evaluate()).
    n = NULL,
    #' @field sig Content signature of the last evaluation; lets $evaluate()
    #'   skip recomputation when the (sub)tree is unchanged.
    sig = NULL,
    #' @field id Session-unique node id used by the drag-and-drop UI to address
    #'   nodes. Assigned once at construction; stable for the node's lifetime.
    #'   Not domain data - regenerate (do not persist) on (de)serialization.
    id = NULL,

    #' @description Create a node.
    #' @param value_type One of "base", "filter", "and", "or", "not".
    #' @param value Query value(s) for "filter" leaves.
    #' @param rdv,column,query_type,inclusion,window Clause fields for leaves.
    #' @param children list() of child Nodes for operator nodes.
    initialize = function(value_type = NULL, value = NULL, rdv = NULL,
                          column = NULL, query_type = NULL, inclusion = NULL,
                          window = c(0L, 0L), children = NULL) {
      self$value_type <- value_type
      self$value <- value
      self$rdv <- rdv
      self$column <- column
      self$query_type <- query_type
      self$inclusion <- inclusion
      self$window <- window
      self$children <- children %||% list()
      self$id <- private$gen_id()
    },

    #' @description Resolve this subtree to a patient_list tibble. Caches the
    #'   result (and `$n`) on the node.
    #' @param df_rdvs Named list of RDV tibbles (must include `df_pde`).
    #' @param base_list The seed population; computed from `df_pde` at the root
    #'   and threaded down to descendants. Leave NULL when calling on the root.
    #' @return A tibble with columns project_id, entry_date, exit_date.
    evaluate = function(df_rdvs, base_list = NULL) {

      # RDV data is fixed for the session, so an unchanged signature means an
      # unchanged result: reuse the cache. An AND<->OR swap then only re-combines
      # its (already-computed) children instead of re-running their queries.
      sig <- self$signature()
      if (!is.null(self$patient_list) && identical(self$sig, sig))
        return(self$patient_list)

      # Compute the base population once at the root and thread it down so that
      # every filter leaf / NOT selects from the same seed.
      if (is.null(base_list)) base_list <- private$seed_base(df_rdvs)

      pl <- switch(self$value_type,
        "base"   = base_list,
        "filter" = private$eval_filter(df_rdvs, base_list),
        "and"    = private$combine(df_rdvs, base_list, "and"),
        "or"     = private$combine(df_rdvs, base_list, "or"),
        "not"    = private$eval_not(df_rdvs, base_list),
        stop(sprintf("Unknown node value_type: '%s'", self$value_type))
      )

      pl <- private$merge_periods(pl)

      self$patient_list <- pl
      self$n <- self$patient_count()
      self$sig <- sig
      pl
    },

    #' @description Content signature of this subtree (its type and clause
    #'   fields, plus its children's signatures). Equal signatures evaluate to
    #'   the same patient_list, since RDV data is fixed per session.
    #' @return A hash string.
    signature = function() {
      if (self$value_type %in% c("and", "or", "not"))
        digest::digest(list(self$value_type,
                            lapply(self$children, function(child) child$signature())))
      else
        digest::digest(list(self$value_type, self$rdv, self$column,
                            self$query_type, self$inclusion, self$window, self$value))
    },

    #' @description Distinct patient count of the cached patient_list.
    #' @return Integer count, or NULL if not yet evaluated.
    patient_count = function() {
      if (is.null(self$patient_list)) return(NULL)
      dplyr::n_distinct(self$patient_list$project_id)
    },

    #' @description Depth-first search for a node by id within this subtree.
    #' @param id The node id to find.
    #' @return The matching `Node` (possibly `self`), or NULL if not found.
    find_by_id = function(id) {
      if (identical(self$id, id)) return(self)
      for (child in self$children) {
        hit <- child$find_by_id(id)
        if (!is.null(hit)) return(hit)
      }
      NULL
    },

    #' @description TRUE if `id` is this node or any descendant. Used to reject
    #'   drags that would move a node into its own subtree (cycle guard).
    #' @param id The node id to test for.
    #' @return Logical scalar.
    contains_id = function(id) {
      !is.null(self$find_by_id(id))
    },

    #' @description Detach the node with the given id from its parent's children
    #'   (searched within this subtree). Mutates the parent in place.
    #' @param id The node id to remove.
    #' @return The removed `Node`, or NULL if not found (or if id is `self`,
    #'   which has no parent here).
    remove_by_id = function(id) {
      for (i in seq_along(self$children)) {
        if (identical(self$children[[i]]$id, id)) {
          removed <- self$children[[i]]
          self$children[[i]] <- NULL
          return(removed)
        }
      }
      for (child in self$children) {
        removed <- child$remove_by_id(id)
        if (!is.null(removed)) return(removed)
      }
      NULL
    },

    #' @description Insert a child Node into this node's children at a 1-based
    #'   index (clamped to a valid position). Mutates in place.
    #' @param child The `Node` to insert.
    #' @param index 1-based target position. Defaults to the end.
    #' @return Invisibly, self.
    insert_at = function(child, index = NULL) {
      # Reject inserts that would create a cycle: placing a node inside itself or
      # its own descendant makes recursive traversals (find/remove/evaluate/
      # render) infinitely recurse and crash the session.
      if (identical(child, self)) {
        stop("Cannot insert a node into itself.")
      }
      if (child$contains_id(self$id)) {
        stop("Cannot insert a node into its own descendant (would create a cycle).")
      }
      n <- length(self$children)
      if (is.null(index)) index <- n + 1L
      index <- max(1L, min(as.integer(index), n + 1L))
      self$children <- append(self$children, list(child), after = index - 1L)
      invisible(self)
    },

    #' @description Whether this node can currently be evaluated. Operator nodes
    #'   need children (`not` needs exactly one); base/filter leaves are always
    #'   evaluable. Lets the UI hold empty operator boxes as a valid building
    #'   state without erroring in `$evaluate()`.
    #' @return Logical scalar.
    is_evaluable = function() {
      switch(self$value_type %||% "",
        "base"   = TRUE,
        "filter" = TRUE,
        "not"    = length(self$children) == 1 &&
                   self$children[[1]]$is_evaluable(),
        "and"    = ,
        "or"     = length(self$children) >= 1 &&
                   all(vapply(self$children, function(c) c$is_evaluable(), logical(1))),
        FALSE
      )
    }
  ),

  private = list(

    # Generate a session-unique node id (monotonic counter + random suffix).
    # The counter alone is unique within a session; the random suffix keeps ids
    # from colliding across R6 instances built in quick succession.
    gen_id = function() {
      .node_id_state$counter <- .node_id_state$counter + 1L
      sprintf("n%d_%s", .node_id_state$counter,
              paste(sample(c(letters, 0:9), 6, replace = TRUE), collapse = ""))
    },

    # Seed population: one period per patient, birth -> death (or today).
    seed_base = function(df_rdvs) {
      df_rdvs$df_pde %>%
        dplyr::transmute(
          project_id,
          entry_date = lubridate::as_date(birth_date),
          exit_date  = lubridate::as_date(
            dplyr::if_else(is.na(death_date), Sys.Date(), death_date)
          )
        )
    },

    # Evaluate a single filter leaf against the base population. Mirrors the
    # per-clause logic in utils_segment_cohorts::update_patient_lists().
    eval_filter = function(df_rdvs, base_list) {

      rdv_name   <- paste0("df_", stringr::str_to_lower(self$rdv))
      col        <- self$column
      inclusion  <- self$inclusion
      cropwindow <- unlist(self$window %||% c(0L, 0L))
      query_type <- self$query_type
      query_val  <- self$value

      # Build the query-matching closure
      if (stringr::str_starts(query_type, "str_")) {
        qv <- stringr::str_replace_all(query_val, c("\\(" = "\\\\(", "\\)" = "\\\\)"))
        regex <- paste0("(", paste(qv, collapse = "|"), ")")
        if (query_type == "str_starts") regex <- paste0("^", regex)
        else if (query_type == "str_matches") regex <- paste0("^", regex, "$")
        else if (query_type == "str_contains") regex <- paste0("(?i)", regex)
        f <- str_regex_closure(regex, col)
      } else if (query_type == "date_between") {
        f <- val_between_closure(query_val, col)
      } else if (query_type == "age_between") {
        f <- age_between_closure(query_val, col)
      } else if (query_type == "numeric_between") {
        f <- val_between_closure(query_val, col)
      } else {
        stop(sprintf("Unknown query_type: '%s'", query_type))
      }
      filter_functions <- c(f)

      # Temporal / inclusion crop closures
      if (inclusion == "fully_concurrent")
        filter_functions <- c(filter_functions, ff_concurrent, ff_crop_entry_exit_closure(cropwindow))
      else if (inclusion == "after_first")
        filter_functions <- c(filter_functions, ff_concurrent, ff_crop_entry_closure(cropwindow))
      else if (inclusion == "on_first")
        filter_functions <- c(filter_functions, ff_concurrent, ff_crop_first_closure(cropwindow))

      # Apply the filters against the RDV, scoped to the base population
      rdv_to_filter <- df_rdvs[[rdv_name]] %>%
        dplyr::right_join(base_list, by = "project_id")
      for (ff in filter_functions) rdv_to_filter <- rdv_to_filter %>% ff()

      new_patient_list <- rdv_to_filter %>%
        dplyr::select(project_id, entry_date, exit_date)

      # "never" / "ever" exclusion: keep the base patients who did NOT match
      if (inclusion == "never") {
        base_list %>%
          dplyr::filter(!project_id %in% unique(new_patient_list$project_id))
      } else {
        new_patient_list
      }
    },

    # Evaluate all children, then set-combine their patient lists.
    combine = function(df_rdvs, base_list, op) {
      if (length(self$children) == 0)
        stop(sprintf("'%s' node has no children", op))

      child_lists <- lapply(self$children, function(child)
        child$evaluate(df_rdvs, base_list))

      if (op == "and") {
        Reduce(private$intersect_periods, child_lists)
      } else { # "or"
        dplyr::bind_rows(child_lists)
      }
    },

    # NOT: base patients not present in the (single) child's patient list.
    eval_not = function(df_rdvs, base_list) {
      if (length(self$children) != 1)
        stop("'not' node must have exactly one child")
      child_pl <- self$children[[1]]$evaluate(df_rdvs, base_list)
      base_list %>%
        dplyr::filter(!project_id %in% unique(child_pl$project_id))
    },

    # Intersect two period sets per patient: a patient survives only where the
    # periods overlap, cropped to the overlapping window.
    intersect_periods = function(a, b) {
      dplyr::inner_join(
        a, b,
        by = "project_id",
        suffix = c("_a", "_b"),
        relationship = "many-to-many"
      ) %>%
        dplyr::mutate(
          entry_date = pmax(entry_date_a, entry_date_b),
          exit_date  = pmin(exit_date_a, exit_date_b)
        ) %>%
        dplyr::filter(entry_date <= exit_date) %>%
        dplyr::select(project_id, entry_date, exit_date)
    },

    # Merge overlapping / contiguous periods per patient (reuses package helper).
    merge_periods = function(patient_list) {
      if (is.null(patient_list) || nrow(patient_list) == 0) return(patient_list)
      patient_list %>%
        dplyr::mutate(remove = FALSE) %>%
        dplyr::group_by(project_id) %>%
        dplyr::group_modify(merge_contiguous) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!remove) %>%
        dplyr::select(!remove)
    }
  )
)


# Null-coalescing helper (avoids a hard dependency on rlang's %||% at load time)
`%||%` <- function(x, y) if (is.null(x)) y else x

# Monotonic counter backing Node id generation (see private$gen_id). Held in an
# environment so it stays mutable after the package namespace is locked on load.
.node_id_state <- new.env(parent = emptyenv())
.node_id_state$counter <- 0L
