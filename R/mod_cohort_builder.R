# Code is a mostly a copy of mod_cohort_creator.R, but with all patient-data dependencies removed. 
# It is used in the app builder to create YAML initialCohorts without needing a patient list or RDV server connection.
# Useful when you want to create a cohort definition for an app without access to the underlying patient data
# or when you want to create a cohort definition for a dataset that is not yet available in the RDV server.
# or when you are changing the dataset you want


#' Standalone, data-free cohort builder for the app builder (YAML maker)
#'
#' Mirrors the look and feel of the main cohort creator (ui_select_cohort) but is
#' completely self-contained. It depends ONLY on the two RDV lookup CSVs shipped
#' in inst/configs (rdv_code_lookup.csv, rdv_variable_lookup.csv), accessed via
#' the pure helpers in utils_rdv_lookups.R. There is no connection to patient
#' data: no rdvs_server, no patient-list segmentation, and no cohort counts.
#'
#' `cohort_builder_server()` returns a reactive giving the named list of cohorts.
#' Each cohort is a list of filter-clause lists (type/rdv/column/inclusion/val/
#' query_type/window) ready to serialise straight into an app YAML's
#' `initialCohorts` (see .cohorts_to_initial_config in mod_drag_drop_server.R).
NULL

# Inclusion-criteria options (label -> code), matching the main cohort builder.
cohort_builder_inclusion_types <- c(
  "Patient after first had ..." = "after_first",
  "Patient on first had ..."    = "on_first",
  "Patient had ..."             = "fully_concurrent",
  "Patient ever had ..."        = "ever",
  "Patient never had ..."       = "never"
)


#' UI for the standalone cohort builder
#'
#' @param id Character namespace id for this module instance.
#' @return A Shiny tagList.
cohort_builder_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        4,
        class = "current_cohorts_col",
        shiny::selectInput(
          ns("current_cohorts"), "Current cohorts",
          selectize = FALSE, size = 10, choices = NULL
        ),
        shiny::actionButton(ns("cohort_add"), "Add cohort",
                            icon = shiny::icon("person-circle-plus")),
        shiny::actionButton(ns("cohort_remove"), "Remove cohort",
                            icon = shiny::icon("person-circle-minus")),
        shiny::actionButton(ns("cohort_rename"), "Rename cohort",
                            icon = shiny::icon("i-cursor")),
      ),
      shiny::column(
        8,
        class = "new_cohort_col",
        shiny::conditionalPanel(
          condition = paste0("input[\"", ns("current_cohorts"), "\"]"),
          shiny::tags$label("Define cohort"),
          shiny::uiOutput(ns("filter_list")),
          shiny::actionButton(ns("cohort_add_stage"), "Add new filter",
                              icon = shiny::icon("filter")),
        )
      )
    )
  )
}


#' Server for the standalone cohort builder
#'
#' @param id Character namespace id, matching `cohort_builder_ui`.
#' @return A reactive returning the named list of cohorts.
cohort_builder_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Named list of cohorts; each element is a list of filter-clause lists.
    cohorts <- shiny::reactiveVal(list())

    # ---- helpers -----------------------------------------------------------

    launch_filter_modal <- function() {
      output$modal_error <- shiny::renderText("")
      shiny::showModal(.cohort_filter_modal(ns))
    }

    launch_rename_modal <- function() {
      shiny::showModal(shiny::modalDialog(
        title = "Rename cohort",
        shiny::textInput(ns("rename_value"), "New name",
                         value = input$current_cohorts),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("rename_accept"), "Rename")
        )
      ))
    }

    # ---- cohort management -------------------------------------------------

    shiny::observeEvent(input$cohort_add, {
      cs <- cohorts()
      new_label <- LETTERS[which(!(LETTERS %in% names(cs)))[1]]
      cs[[new_label]] <- list()
      cohorts(cs)
      shiny::updateSelectInput(session, "current_cohorts",
                               choices = names(cs), selected = new_label)
      launch_filter_modal()
    })

    shiny::observeEvent(input$cohort_remove, {
      cur <- input$current_cohorts
      if (is.null(cur)) return()
      cs <- cohorts()
      cs[[cur]] <- NULL
      cohorts(cs)
      shiny::updateSelectInput(session, "current_cohorts",
                               choices = names(cs),
                               selected = utils::tail(names(cs), 1))
    })

    shiny::observeEvent(input$cohort_rename, {
      if (is.null(input$current_cohorts)) return()
      launch_rename_modal()
    })

    shiny::observeEvent(input$rename_accept, {
      cur <- input$current_cohorts
      new <- input$rename_value
      if (!is.null(cur) && !is.null(new) && nzchar(new) && !(new %in% names(cohorts()))) {
        cs <- cohorts()
        names(cs)[names(cs) == cur] <- new
        cohorts(cs)
        shiny::updateSelectInput(session, "current_cohorts",
                                 choices = names(cs), selected = new)
      }
      shiny::removeModal()
    })

    # ---- filter list display ----------------------------------------------

    output$filter_list <- shiny::renderUI({
      cur <- input$current_cohorts
      if (is.null(cur)) return(NULL)
      clauses <- cohorts()[[cur]]
      if (length(clauses) == 0)
        return(shiny::helpText("No filters yet — click \"Add new filter\"."))

      lapply(clauses, function(cl) {
        bs4Dash::bs4Card(
          class = "cohort_element", width = 12, collapsed = TRUE,
          title = .clause_title(cl),
          .clause_description(cl)
        )
      })
    })

    # ---- add-filter modal --------------------------------------------------

    shiny::observeEvent(input$cohort_add_stage, { launch_filter_modal() })

    # Column ("parameter") choices for the selected RDV, from the variable lookup.
    selected_vars <- shiny::reactive({
      rdv <- input$modal_rdv
      if (is.null(rdv) || !nzchar(rdv)) return(NULL)
      get_rdv_Variable_lookup() %>% dplyr::filter(rdv_code == rdv)
    })

    shiny::observe({
      vars <- selected_vars()
      choices <- if (is.null(vars)) character(0)
                 else stats::setNames(vars$variable_code, vars$label)
      shiny::updateSelectizeInput(session, "modal_col_1", choices = choices)
      shiny::updateSelectizeInput(session, "modal_col_2", choices = choices)
    })

    # Value widget per condition, chosen from the column's input_type. Rendered
    # server-side so it always uses a fixed inputId we can read back on accept.
    output$modal_val_1 <- shiny::renderUI(.value_widget(ns("modal_val_input_1"), input$modal_col_1))
    output$modal_val_2 <- shiny::renderUI(.value_widget(ns("modal_val_input_2"), input$modal_col_2))

    shiny::observeEvent(input$modal_accept, {
      cur <- input$current_cohorts
      if (is.null(cur)) { shiny::removeModal(); return() }

      rdv <- input$modal_rdv
      new_clauses <- list()

      for (idx in 1:2) {
        col <- input[[paste0("modal_col_", idx)]]
        if (is.null(col) || !nzchar(col)) {
          if (idx == 1) break else next
        }

        val <- input[[paste0("modal_val_input_", idx)]]
        if (is.null(val) || (is.character(val) && !nzchar(paste(val, collapse = "")))) {
          output$modal_error <- shiny::renderText("Please enter a value for the selected parameter.")
          return()
        }

        new_clauses[[length(new_clauses) + 1]] <- list(
          type       = if (length(new_clauses) == 0) "filter" else "and",
          rdv        = rdv,
          column     = col,
          inclusion  = input$modal_inclusion,
          val        = val,
          query_type = as.character(get_variable_label(rdv, col, "filter_type")),
          window     = input$modal_window
        )

        # Only look at the second condition if the user asked for it.
        if (idx == 1 && !isTRUE(input$modal_another_1)) break
      }

      if (length(new_clauses) == 0) {
        output$modal_error <- shiny::renderText("Please choose a category and parameter.")
        return()
      }

      cs <- cohorts()
      cs[[cur]] <- c(cs[[cur]], new_clauses)
      cohorts(cs)
      shiny::removeModal()
    })

    # Return the cohorts reactive for serialisation by the caller.
    cohorts
  })
}


# Build the add-filter modal. RDV ("category") choices come from the code lookup;
# column and value inputs are populated/rendered by the server observers above.
.cohort_filter_modal <- function(ns) {
  rdv_opts    <- get_rdv_code_lookup()
  rdv_choices <- stats::setNames(rdv_opts$rdv_code, rdv_opts$label)

  selectize_opts <- list(
    placeholder  = "Please select an option",
    onInitialize = I('function() { this.setValue(""); }')
  )

  shiny::modalDialog(
    title = "Add filter",

    shiny::selectInput(ns("modal_inclusion"), "Inclusion criteria",
                       cohort_builder_inclusion_types, width = "100%"),

    shiny::sliderInput(ns("modal_window"), "Inclusion window", -100, 100, c(0, 0)),

    shiny::selectizeInput(ns("modal_rdv"), "Select category", rdv_choices,
                          options = selectize_opts),

    # Condition 1 (shown once a category is picked)
    shiny::conditionalPanel(
      condition = paste0("input['", ns("modal_rdv"), "'] != ''"),
      shiny::selectizeInput(ns("modal_col_1"), "Select parameter",
                            choices = NULL, options = selectize_opts),
      shiny::uiOutput(ns("modal_val_1")),
      shiny::checkboxInput(ns("modal_another_1"), "Add additional condition", FALSE)
    ),

    # Condition 2 (shown only if "Add additional condition" is ticked)
    shiny::conditionalPanel(
      condition = paste0("input['", ns("modal_another_1"), "']"),
      shiny::selectizeInput(ns("modal_col_2"), "Select parameter",
                            choices = NULL, options = selectize_opts),
      shiny::uiOutput(ns("modal_val_2"))
    ),

    footer = shiny::tagList(
      shiny::span(style = "color:red; font-weight:bold;", shiny::textOutput(ns("modal_error"), inline = TRUE)),
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns("modal_accept"), "Accept")
    )
  )
}


# Render the value input appropriate to a column's input_type. `input_id` is
# already namespaced; free-text is enabled on selects since there is no data to
# populate coded choices.
.value_widget <- function(input_id, col) {
  if (is.null(col) || !nzchar(col)) return(NULL)

  switch(
    .col_input_type(col),
    select        = shiny::selectizeInput(
                      input_id, "Value(s)", choices = NULL, multiple = TRUE,
                      options = list(create = TRUE, placeholder = "Type value(s) and press enter")),
    text          = shiny::textInput(input_id, "Search for"),
    date_range    = shiny::dateRangeInput(input_id, "Date range"),
    numeric_range = shiny::sliderInput(input_id, "Range", -10000, 10000, c(-10000, 10000)),
    age_range     = shiny::sliderInput(input_id, "Age range (years)", 0, 18, c(0, 18)),
    shiny::textInput(input_id, "Value")
  )
}


# Look up a column's input_type from the variable lookup (defaults to "text").
.col_input_type <- function(col) {
  lk <- get_rdv_Variable_lookup()
  it <- lk$input_type[match(col, lk$variable_code)]
  if (length(it) == 0 || is.na(it)) "text" else it
}


# Human-readable title for a filter card.
.clause_title <- function(cl) {
  rdv_label <- tryCatch(get_rdv_code(cl$rdv)$label[[1]], error = function(e) cl$rdv)
  var_label <- tryCatch(get_variable_label(cl$rdv, cl$column), error = function(e) cl$column)
  if (length(var_label) == 0 || is.na(var_label[[1]])) var_label <- cl$column
  paste0(if (identical(cl$type, "and")) "AND — " else "", rdv_label, ": ", var_label)
}

# Human-readable body for a filter card.
.clause_description <- function(cl) {
  incl_label <- names(cohort_builder_inclusion_types)[cohort_builder_inclusion_types == cl$inclusion]
  if (length(incl_label) == 0) incl_label <- cl$inclusion
  shiny::tags$span(sprintf(
    "%s  %s  (%s)",
    cl$query_type, paste(unlist(cl$val), collapse = ", "), incl_label
  ))
}
