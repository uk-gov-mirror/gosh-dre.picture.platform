#' Shiny UI module for the Filter a Patient Cohort
#'
#' Shiny UI function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#'
#' @return Shiny taglist for the UI
#'
#' @family ui_select_cohort
#' @export
ui_select_cohort_ui <- function() {

  ns <- shiny::NS("select_cohort_mod")

  shiny::tagList(

    # Always visible summary bar with the selected cohorts distinct patient count
    shiny::div(
      id = ns("final_count_box"),
      class = "cohort-final",
      shiny::uiOutput(ns("final_count"), inline = TRUE)
    ),

    shiny::fluidRow(
      shiny::column(
        4,
        class = "current_cohorts_col",
        shiny::selectInput(
          ns("current_cohorts"), "Current cohorts", selectize = FALSE,
          size = 10, choices = NULL),  # c("First one", "second one")),
        shiny::actionButton(ns("cohort_add"), "Add cohort", icon = shiny::icon("person-circle-plus")),
        shiny::actionButton(ns("cohort_remove"), "Remove cohort", icon = shiny::icon("person-circle-minus")),
        shiny::actionButton(ns("cohort_rename"), "Rename cohort", icon = shiny::icon("i-cursor")),
      ),
      shiny::column(
        8,
        class = "new_cohort_col",
        shiny::conditionalPanel(
          condition = paste0("input[\"", ns("current_cohorts"), "\"]"),
          shiny::tags$label("Define cohort"),
          # Top-level controls act on the cohort root (wrapping it in an
          # operator when needed - see ensure_operator_root()).
          shiny::div(
            class = "cohort-root-controls",
            shiny::actionButton(ns("add_filter_root"), "Add filter", icon = shiny::icon("filter"), class = "btn-sm"),
            shiny::actionButton(ns("add_and_root"), "Add AND block", icon = shiny::icon("plus"), class = "btn-sm"),
            shiny::actionButton(ns("add_or_root"), "Add OR block", icon = shiny::icon("plus"), class = "btn-sm"),
            shiny::actionButton(ns("add_not_root"), "Add NOT block", icon = shiny::icon("plus"), class = "btn-sm"),
          ),
          # The recursive tree is rendered server-side into here.
          shiny::uiOutput(ns("tree"), class = "cohort-tree"),
        )
      )
    )
  )
}

inclusion_types <- c(
  "Patient after first had ..." = "after_first",
  "Patient on first had ..." = "on_first",
  "Patient had ..." = "fully_concurrent",
  # "Patient ever concurrently had ..." = "ever_concurrent",
  "Patient ever had ..." = "ever",
  "Patient never had ..." = "never"
)

# ---------------------------------------------------------------------------
# Recursive tree rendering
# ---------------------------------------------------------------------------

# DOM ids must be fully namespaced so SortableJS (sortable_js css_id) and
# shinyjs (patch_counts) can address them. The module ns is the fixed string
# "select_cohort_mod", so these resolve to "select_cohort_mod-zone-<id>" etc.
zone_dom_id  <- function(ns, id) ns(paste0("zone-", id))
count_dom_id <- function(ns, id) ns(paste0("count-", id))

# Operator -> bs4Card header status (colour).
op_status <- function(value_type) {
  switch(value_type, "and" = "primary", "or" = "warning", "not" = "danger", "secondary")
}

# Badge text for a node's patient count. Non-evaluable nodes (empty operator,
# malformed NOT) show a dash rather than a stale / missing number.
count_text <- function(node) {
  if (!node$is_evaluable()) return("—")
  if (is.null(node$n)) return("…")
  format(node$n, big.mark = ",")
}

count_badge <- function(node, ns) {
  shiny::span(
    class = "cohort-count badge",
    id = count_dom_id(ns, node$id),
    count_text(node)
  )
}

# Human-readable one-line summary for a filter leaf, e.g.
# "Diagnosis code starts with E11, E12 (ever)". Falls back gracefully when the
# RDV/variable lookups don't resolve.
node_summary <- function(node) {

  rdv_pretty <- tryCatch(
    stringr::str_to_title(get_rdv_code(node$rdv)$label),
    error = function(e) stringr::str_to_title(node$rdv %||% "")
  )
  if (length(rdv_pretty) == 0 || is.na(rdv_pretty[[1]])) rdv_pretty <- node$rdv %||% ""

  col_pretty <- tryCatch(
    get_variable_label(node$rdv, node$column),
    error = function(e) node$column
  )
  if (length(col_pretty) == 0 || is.na(col_pretty[[1]])) col_pretty <- node$column %||% ""

  qt <- node$query_type %||% ""
  val <- node$value
  verb_val <- if (qt == "str_starts") {
    paste("starts with", paste(val, collapse = ", "))
  } else if (qt == "str_matches") {
    paste("is", paste(val, collapse = ", "))
  } else if (stringr::str_starts(qt, "str_")) {
    paste(stringr::str_match(qt, "^str_(.*)$")[[2]], paste(val, collapse = ", "))
  } else if (qt %in% c("date_between", "age_between", "numeric_between")) {
    paste("is between", val[[1]], "and", val[[2]])
  } else {
    paste(val, collapse = ", ")
  }

  incl <- names(inclusion_types)[match(node$inclusion, inclusion_types)]
  incl <- if (is.na(incl)) node$inclusion else gsub("\\.\\.\\.", "", incl)

  paste0(stringr::str_trim(paste(rdv_pretty, col_pretty)), " ", verb_val,
         if (!is.null(node$inclusion)) paste0("  —  ", stringr::str_trim(incl)) else "")
}

# JS for an action button: set a namespaced {id, nonce} input on click. The
# nonce + priority:event guarantee the observer fires even for identical edits;
# stopPropagation keeps the click from toggling the surrounding card.
js_set_target <- function(ns, input_name, node_id) {
  sprintf(
    "Shiny.setInputValue('%s', {id: '%s', nonce: Math.random()}, {priority: 'event'}); event.stopPropagation();",
    ns(input_name), node_id
  )
}

# SortableJS onEnd handler: report the move (dragged id, target/source parent
# dropzone ids, new 0-based index) back to R, which owns the tree.
onend_js <- function(ns) {
  htmlwidgets::JS(sprintf(
    paste0(
      "function(evt){ Shiny.setInputValue('%s', {",
      "moved_id: evt.item.getAttribute('data-node-id'), ",
      "new_parent_id: evt.to.getAttribute('data-node-id'), ",
      "old_parent_id: evt.from.getAttribute('data-node-id'), ",
      "new_index: evt.newIndex, nonce: Math.random()",
      "}, {priority: 'event'}); }"
    ),
    ns("node_moved")
  ))
}

act_btn <- function(ns, input_name, node_id, icon_name, label = NULL, extra_class = "") {
  shiny::tags$button(
    type = "button",
    class = paste("btn btn-sm cohort-btn", extra_class),
    onclick = js_set_target(ns, input_name, node_id),
    shiny::icon(icon_name), label
  )
}

# Controls rendered inside every operator box (add filter / nested blocks).
op_add_controls <- function(node, ns) {
  shiny::div(
    class = "cohort-controls",
    act_btn(ns, "add_filter_target",   node$id, "filter", "Filter", "btn-outline-secondary"),
    act_btn(ns, "add_block_and_target", node$id, "plus",  "AND",    "btn-outline-primary"),
    act_btn(ns, "add_block_or_target",  node$id, "plus",  "OR",     "btn-outline-warning"),
    act_btn(ns, "add_block_not_target", node$id, "plus",  "NOT",    "btn-outline-danger")
  )
}

#' Recursively render a cohort Node tree to nested, drag-and-drop UI.
#'
#' Operator nodes (`and`/`or`/`not`) render as collapsible boxes whose body
#' holds a SortableJS drop-zone (their children, rendered recursively) plus
#' add-controls. Filter/base leaves render as compact cards. Every node sits in
#' a `.cohort-node` wrapper carrying its id so drags and clicks can address it.
#'
#' @param node The `Node` to render.
#' @param ns The module namespace function.
#' @param is_root TRUE for the cohort root (suppresses its delete control).
#' @return A Shiny tag.
#' @noRd
render_node <- function(node, ns, is_root = FALSE) {

  vt <- node$value_type

  wrapper <- function(...) {
    shiny::div(
      class = paste("cohort-node",
                    if (vt == "base") "cohort-base",
                    if (vt %in% c("and", "or", "not")) paste0("cohort-op cohort-op-", vt)),
      `data-node-id` = node$id,
      ...
    )
  }

  # ---- Leaf nodes (base / filter) ----
  if (!vt %in% c("and", "or", "not")) {

    is_base <- vt == "base"
    return(wrapper(
      shiny::div(
        class = paste("cohort-leaf", if (is_base) "cohort-leaf-base"),
        shiny::span(class = "cohort-leaf-summary",
                    if (is_base) "Base cohort (all patients)" else node_summary(node)),
        count_badge(node, ns),
        if (!is_base)
          act_btn(ns, "delete_target", node$id, "trash", NULL, "btn-outline-danger cohort-del")
      )
    ))
  }

  # ---- Operator nodes (and / or / not) ----
  header <- shiny::span(
    class = "cohort-op-header",
    shiny::span(class = "cohort-op-name", toupper(vt)),
    count_badge(node, ns),
    if (vt != "not")
      shiny::tags$button(
        type = "button", class = "btn btn-sm cohort-btn cohort-toggle",
        onclick = js_set_target(ns, "toggle_target", node$id),
        "AND ⇄ OR"
      ),
    if (!is_root)
      act_btn(ns, "delete_target", node$id, "trash", NULL, "btn-outline-danger cohort-del")
  )

  zid <- zone_dom_id(ns, node$id)

  wrapper(
    bs4Dash::bs4Card(
      title = header,
      status = op_status(vt),
      collapsible = TRUE, collapsed = FALSE, width = 12,

      # Drop-zone: children are direct children so SortableJS sees them.
      shiny::div(
        class = "cohort-dropzone",
        id = zid,
        `data-node-id` = node$id,
        lapply(node$children, render_node, ns = ns)
      ),
      # SortableJS instance for this zone. Placed outside the dropzone (and
      # hidden) so it is never itself a sortable item. The shared group makes
      # all zones connected, so nodes can be dragged across nesting levels.
      sortable::sortable_js(
        css_id = zid,
        options = sortable::sortable_options(
          group = "cohort",
          animation = 150,
          draggable = ".cohort-node",
          filter = ".cohort-base",
          fallbackOnBody = TRUE,
          swapThreshold = 0.65,
          onEnd = onend_js(ns)
        )
      ),
      op_add_controls(node, ns)
    )
  )
}

n_label <- function(node) {
  if (is.null(node$n)) "" else paste0(" (n = ", node$n, ")")
}

flatten_tree <- function(node) {
  c(list(node), unlist(lapply(node$children, flatten_tree), recursive = FALSE))
}

# Patch every count badge in the tree in place (no full re-render, so collapse
# state and scroll position are preserved).
patch_counts <- function(root, ns) {
  for (node in flatten_tree(root))
    shinyjs::html(id = count_dom_id(ns, node$id), html = count_text(node))
}

stage2nodes <- function(stage) {
  lapply(seq_len(nrow(stage)), function(i) {
    row <- stage[i, ]
    Node$new(
      value_type = "filter",
      value      = row$val[[1]],
      rdv        = row$rdv,
      column     = row$column,
      query_type = row$query_type,
      inclusion  = row$inclusion,
      window     = row$window[[1]]
    )
  })
}


#' Shiny server module for the Filter a Patient Cohort
#'
#' Shiny server function encapsulated by namespace 'id'
#'
#' @param rdvs_server Reactive returning the named list of RDV tibbles.
#' @param input_cohorts Reactive returning the incoming named list of root Nodes.
#'
#' @return Reactive resolving to the named list of root `Node`s (consumed
#'   downstream by `filter_by_cohorts()`).
#'
#' @family ui_select_cohort
#' @export
ui_select_cohort_server <- function(rdvs_server, input_cohorts) {

  ns <- shiny::NS("select_cohort_mod")

  shiny::moduleServer(
    "select_cohort_mod",
    function(input, output, session) {

      # Edit triggers
      rerender_trigger <- shiny::reactiveVal(0)
      recount_trigger  <- shiny::reactiveVal(0)
      count_refresh    <- shiny::reactiveVal(0)
      # Raw target node id stashed when launching the criteria modal (NULL = root).
      pending_filter_parent <- shiny::reactiveVal(NULL)

      # Isolate the self-read so calling these inside a reactive/observer does
      # not make that consumer depend on (and thus re-fire on) its own bump -
      # which would be an infinite invalidation loop.
      rerender <- function() rerender_trigger(shiny::isolate(rerender_trigger()) + 1)
      bump <- function() {
        rerender()
        recount_trigger(shiny::isolate(recount_trigger()) + 1)
        # Subtle loading state until the debounced recount lands.
        shinyjs::addClass(id = ns("tree"), class = "cohort-busy")
        shinyjs::addClass(id = ns("final_count_box"), class = "cohort-busy")
      }

      current_root <- function() {
        cc <- input$current_cohorts
        if (is.null(cc)) return(NULL)
        session$userData$cohorts[[cc]]
      }

      # Warm the RDV data cache up-front: rdvs_server() lazily loads every RDV
      # table from disk on first read, so force it once at init rather than
      # blocking the user's first cohort edit. Fires once (rdvs_server is stable).
      shiny::observe({
        rdvs_server()
      })

      # Seed the cohort graph from the upstream definition once, on the first
      # non-empty push. input_cohorts() is live, so reseeding on every fire would
      # overwrite the user's in-progress graph; latch via seeded() to prevent it.
      seeded <- shiny::reactiveVal(FALSE)
      shiny::observe({
        incoming <- input_cohorts()
        if (isTRUE(shiny::isolate(seeded()))) return()
        if (is.null(incoming) || length(incoming) == 0) return()
        seeded(TRUE)
        session$userData$cohorts <- incoming
        shiny::isolate({
          regen_cohort_list()
          regen_patient_lists()
          rerender()
        })
      })

      # The cohort list is regenerated on the first UI render
      ui_rendered <- shiny::reactiveVal(FALSE)
      final_count_hidden_key <- paste0("output_", ns("final_count"), "_hidden")
      shiny::observe({
        if (isFALSE(session$clientData[[final_count_hidden_key]])) ui_rendered(TRUE)
      })
      shiny::observeEvent(ui_rendered(), {
        if (length(session$userData$cohorts) > 0) regen_cohort_list()
      }, ignoreInit = TRUE)

      # Render the recursive tree for the selected cohort.
      output$tree <- shiny::renderUI({
        rerender_trigger()
        shiny::req(input$current_cohorts)
        root <- current_root()
        if (is.null(root)) return(NULL)
        render_node(root, ns, is_root = TRUE)
      })

      # Always visible final cohort count. Re renders on cohort switch or any edit (bump()).
      output$final_count <- shiny::renderUI({
        rerender_trigger()
        count_refresh()
        root <- current_root()

        if (is.null(root)) {
          return(shiny::span(class = "cohort-final-empty", "No cohort selected"))
        }
        if (!root$is_evaluable()) {
          return(shiny::span(class = "cohort-final-incomplete",
                             "Incomplete — add filters"))
        }
        n <- tryCatch(root$patient_count(), error = function(e) NULL)
        if (is.null(n)) {
          return(shiny::span(class = "cohort-final-pending", "Updating…"))
        }
        shiny::tagList(
          shiny::span(class = "cohort-final-label", "Final cohort:"),
          shiny::span(class = "cohort-final-number", format(n, big.mark = ",")),
          shiny::span(class = "cohort-final-unit", "patients")
        )
      })

      # ---- Cohort list (add / remove / rename) ----

      shiny::observeEvent(input$cohort_add, {

        cohort_labels <- names(session$userData$cohorts)
        new_cohort_label <- LETTERS[which(!(LETTERS %in% cohort_labels))[1]]

        session$userData$cohorts[[new_cohort_label]] <- Node$new(value_type = "base")
        regen_patient_lists()
        regen_cohort_list(new_cohort_label)

        logger::log_info(glue::glue("Adding new cohort: {new_cohort_label}"))

        # Launch the modal for the first criterion; it will wrap the base root.
        pending_filter_parent(session$userData$cohorts[[new_cohort_label]]$id)
        launch_cohort_modal()
      })

      shiny::observeEvent(input$cohort_remove, {
        if (is.null(input$current_cohorts)) return()
        session$userData$cohorts <- delete_cohort(session$userData$cohorts, input)
        shiny::updateSelectInput(session, "current_cohorts",
                          choices = names(session$userData$cohorts),
                          selected = tail(names(session$userData$cohorts), n = 1))
      })

      shiny::observeEvent(input$cohort_rename, {
        rename_cohort_modal()
      })

      shiny::observeEvent(input$accept_rename, {
        logger::log_info("Renaming cohort")
        session$userData$cohorts <- rename_cohort(session$userData$cohorts, input)
        regen_patient_lists()
        regen_cohort_list(input$cohort_modal_name)
        close_rename_cohort_modal(ns)
      })

      # ---- Structural edits ----

      # Resolve a (raw) target id to an operator node id, wrapping a leaf root
      # in an AND when necessary. Returns NULL if the target can't take children.
      resolve_operator_target <- function(raw_id) {
        cc <- input$current_cohorts
        root <- session$userData$cohorts[[cc]]
        if (is.null(root)) return(NULL)
        if (is.null(raw_id)) raw_id <- root$id

        target <- root$find_by_id(raw_id)
        if (is.null(target)) return(NULL)

        if (!target$value_type %in% c("and", "or", "not")) {
          # Only the root leaf exposes add-controls (via the top toolbar) - wrap it.
          if (identical(target$id, root$id)) {
            new_root <- Node$new(value_type = "and", children = list(root))
            session$userData$cohorts[[cc]] <- new_root
            return(new_root$id)
          }
          return(NULL)
        }
        target$id
      }

      # Insert an (empty) operator block into a chosen parent.
      add_block <- function(raw_id, op) {
        pid <- resolve_operator_target(raw_id)
        if (is.null(pid)) return()
        parent <- session$userData$cohorts[[input$current_cohorts]]$find_by_id(pid)
        if (parent$value_type == "not" && length(parent$children) >= 1) {
          shiny::showNotification("A NOT block can only contain one element.", type = "warning")
          return()
        }
        parent$insert_at(Node$new(value_type = op))
        bump()
      }

      # Add-filter handlers: stash the raw target and open the criteria modal.
      shiny::observeEvent(input$add_filter_root, {
        pending_filter_parent(NULL); launch_cohort_modal()
      })
      shiny::observeEvent(input$add_filter_target, {
        pending_filter_parent(input$add_filter_target$id); launch_cohort_modal()
      })

      # Add-block handlers (root toolbar + per-node).
      shiny::observeEvent(input$add_and_root,  add_block(NULL, "and"))
      shiny::observeEvent(input$add_or_root,   add_block(NULL, "or"))
      shiny::observeEvent(input$add_not_root,  add_block(NULL, "not"))
      shiny::observeEvent(input$add_block_and_target, add_block(input$add_block_and_target$id, "and"))
      shiny::observeEvent(input$add_block_or_target,  add_block(input$add_block_or_target$id,  "or"))
      shiny::observeEvent(input$add_block_not_target, add_block(input$add_block_not_target$id, "not"))

      # Delete a node (and its subtree). The root is deleted via "Remove cohort".
      shiny::observeEvent(input$delete_target, {
        root <- current_root()
        if (is.null(root)) return()
        id <- input$delete_target$id
        if (identical(id, root$id)) return()
        root$remove_by_id(id)
        bump()
      })

      # Toggle an operator AND <-> OR.
      shiny::observeEvent(input$toggle_target, {
        root <- current_root()
        if (is.null(root)) return()
        node <- root$find_by_id(input$toggle_target$id)
        if (!is.null(node) && node$value_type %in% c("and", "or")) {
          node$value_type <- if (node$value_type == "and") "or" else "and"
          bump()
        }
      })

      # Drag-and-drop move. R is authoritative: validate, mutate, then always
      # re-render so the DOM reconciles to the tree (including snap-backs).
      shiny::observeEvent(input$node_moved, {
        ev <- input$node_moved
        root <- current_root()
        if (is.null(root)) return()

        moved  <- root$find_by_id(ev$moved_id)
        target <- root$find_by_id(ev$new_parent_id)

        # Reject: missing nodes, base node, cycle (drop into self/descendant),
        # or overfilling a NOT block. Re-render to snap back.
        if (is.null(moved) || is.null(target) || moved$value_type == "base" ||
            identical(moved$id, target$id) || moved$contains_id(target$id)) {
          rerender(); return()
        }
        if (target$value_type == "not") {
          already_here <- any(vapply(target$children,
                                     function(c) identical(c$id, moved$id), logical(1)))
          if (length(target$children) >= 1 && !already_here) { rerender(); return() }
        }

        root$remove_by_id(ev$moved_id)
        target$insert_at(moved, ev$new_index + 1)
        bump()
      })

      # Criteria modal result -> insert filter node(s) into the stashed parent.
      modal_reactive <- ui_select_cohort_modal_observers(input, output, session, rdvs_server)

      shiny::observeEvent(modal_reactive(), {
        new_cohort_stage <- modal_reactive()
        if (!is.null(new_cohort_stage)) {
          pid <- resolve_operator_target(pending_filter_parent())
          if (!is.null(pid)) {
            parent <- session$userData$cohorts[[input$current_cohorts]]$find_by_id(pid)
            for (n in stage2nodes(new_cohort_stage)) parent$insert_at(n)
          }
        }
        pending_filter_parent(NULL)
        bump()
      })

      # ---- Re-evaluation (debounced) ----

      recount <- shiny::debounce(shiny::reactive(recount_trigger()), 500)
      shiny::observeEvent(recount(), {
        root <- current_root()
        if (!is.null(root) && root$is_evaluable()) {
          tryCatch(
            root$evaluate(rdvs_server()),
            error = function(e)
              logger::log_warn(paste("Cohort evaluation failed:", conditionMessage(e)))
          )
        }
        if (!is.null(root)) patch_counts(root, ns)
        shinyjs::removeClass(id = ns("tree"), class = "cohort-busy")
        shinyjs::removeClass(id = ns("final_count_box"), class = "cohort-busy")
        # Refresh the final-count display against the freshly-evaluated root.
        count_refresh(shiny::isolate(count_refresh()) + 1)
      }, ignoreInit = TRUE)

      # Evaluate every (evaluable) cohort root - keeps counts and patient_lists
      # populated for downstream segmentation.
      regen_patient_lists <- function() {
        for (root in session$userData$cohorts) {
          if (!is.null(root) && root$is_evaluable())
            tryCatch(root$evaluate(rdvs_server()),
                     error = function(e)
                       logger::log_warn(paste("Cohort evaluation failed:", conditionMessage(e))))
        }
      }

      # Function to regenerate the input selector
      regen_cohort_list <- function(selection = NULL) {

        choice_list = names(session$userData$cohorts)
        if (is.null(selection)){
          tryCatch(
            selected_list <- choice_list[[1]]
            , error = function(e) {
              logger::log_info("No cohorts reloading the session")
              session$reload()
              }
            )
          }

        else selected_list <- selection

        shiny::updateSelectInput(
          session, "current_cohorts",
          choices = choice_list,
          selected = selected_list
        )
        return(selected_list)
      }

      launch_cohort_modal <- function() {
        ui_select_cohort_modal_show(names(rdvs_server()), ns)
      }

      rename_cohort_modal <- function() {
        ui_rename_cohort_modal(ns)
      }

      # Returned reactive (named list of root Nodes). Re-fires on structural
      # edits so downstream segmentation sees the updated trees.
      shiny::reactive({
        rerender_trigger()
        input$cohort_add
        input$cohort_remove
        input$cohort_rename
        session$userData$cohorts
      })

    }
  )

}

#' Function to rename cohort
#'
#' Shiny UI function encapsulated by namespace 'id'
#'
#' @param cohorts List session cohort list
#' @param input List of user inputs
#'
#' @return cohorts
#'
#' @family ui_select_cohort
#' @noRd
rename_cohort <- function(cohorts, input, test="prod"){
  # get new name
  new_cohort_label <- input$cohort_modal_name
  logger::log_info((glue::glue("Renaming cohort: {input$current_cohorts} to {new_cohort_label}")))
  if(!(new_cohort_label == "" || is.null(new_cohort_label))){
    # cohort to rename
    temp_cohort <- cohorts[which(names(cohorts) == input$current_cohorts)]

    # add new cohort label
    cohorts[new_cohort_label] <- temp_cohort

    #delete the original cohort
    cohorts <- delete_cohort(cohorts, input)
  }else{
    if (!(test=="test")){
      logger::log_warn("Empty cohort name provided using previous names")
      showNotification("Empty cohort name provided using previous names")
    }
  }

  return((cohorts))
}

#' Function to remove a cohort
#'
#' Shiny UI function encapsulated by namespace 'id'
#'
#' @param cohorts List session cohort list
#' @param input List of user inputs
#'
#' @return cohorts
#'
#' @family ui_select_cohort
#' @noRd
delete_cohort <- function(cohorts, input){
  logger::log_info((glue::glue("Removing cohort: {input$current_cohorts}")))
  idx <- which(names(cohorts) == input$current_cohorts)
  # Guard the negative index: `cohorts[-integer(0)]` returns an EMPTY list, so
  # an unmatched name (e.g. a rapid double-click racing an async select update)
  # would silently wipe every cohort. Only drop when we actually matched one.
  if (length(idx) == 0) {
    logger::log_warn(glue::glue(
      "No cohort named '{input$current_cohorts}' to remove; leaving cohorts unchanged."
    ))
    return(cohorts)
  }
  cohorts <- cohorts[-idx]
  return(cohorts)
}
