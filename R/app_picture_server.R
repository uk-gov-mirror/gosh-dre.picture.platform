#' Shiny server function
app_picture_server <- function(app_config, data_dir, n_max) {

  all_btn_names <- sapply(app_config, .cfg2btn)

  function(input, output, session) {
    session_id <- toString(session$token)
    logger::log_shiny_input_changes(input, excluded_inputs = 'password')
    logger::log_info(sprintf("session id: %s", session_id))
    session$onSessionEnded(function(){
      logger::log_info("App has stopped", app = "stop")
      log_output_filename <- glue::glue("log/logfile_{session_id}_{Sys.Date()}.log")
      file.rename("log/logfile_temp.log",log_output_filename)
      })

    # Prevent timeout when running in the cloud partner system
    autoInvalidate <- shiny::reactiveTimer(20000)
    shiny::observe({
      autoInvalidate()
      cat(".")
    })
    # session$allowReconnect("force")

    current_app <- reactiveVal(NULL)

    # Observer for the "run analysis" button
    shiny::observeEvent(input$run_analysis, {
      shinyjs::show(id = "analysis_menuitem")
      # shinyjs::hide(id = "run_analysis")
    })

    # Create observers to populate the selected analysis
    analysis_select_observer_factory <- function(cfg) {

      btn_name <- .cfg2btn(cfg)

      # Reset the session if "stopping" an analysis
      shiny::observeEvent(input[[btn_name]], priority = 10, {
        if (is.null(current_app())) return()
        session$reload()
      })

      # Observer to populate the analysis
      shiny::observeEvent(input[[btn_name]], {

        # Skip if not changed
        if (!is.null(current_app()) && current_app() == cfg$id) return()
        current_app(cfg$id)

        # Change the button and disable other analysis options until stopped
        shiny::updateActionButton(inputId = btn_name, label = "Stop")
        sapply(all_btn_names [!all_btn_names %in% btn_name], shinyjs::disable)

        # Populate the UI and server functions from the config
        .cfg2ui(cfg, output)
        .cfg2server(input, output, session, cfg, data_dir, n_max)

      })

    }
    lapply(app_config, analysis_select_observer_factory)
  }

}


.cfg2ui <- function(cfg, output) {

  # Check if any of the cohorts require a patient select
  offer_patient_select <- FALSE
  for (init_cohort in cfg$initialCohorts)
    for (stage in init_cohort$config)
      if (is.character(stage$val[[1]]) && stage$val[[1]] == "SELECT_PROJECT_ID")
        offer_patient_select <- TRUE

  # Patient select
  if (offer_patient_select) {

    # Add the menu item
    output$patient_select_menuitem <- bs4Dash::renderMenu({
      bs4Dash::menuItem(
        "Patient Selection",
        tabName = "patient_select_tab",
        icon = shiny::icon("user")
      )
    })

    # Create the main tab
    output$patient_tab_output <- renderUI({
      shiny::tagList(
        ui_select_patient_ui("main_patient_select"),
      )
    })

  }

  # Cohort builder
  if (cfg$offerCohortBuilder) {

    # Add the menu item
    output$cohort_select_menuitem <- bs4Dash::renderMenu({
      bs4Dash::menuItem(
        "Cohort Builder",
        tabName = "cohort_select_tab",
        icon = shiny::icon("people-group")
      )
    })

    # Create the main tab
    output$cohort_tab_output <- renderUI({
      shiny::tagList(
        shiny::h1("Cohort Builder"),
        ui_select_cohort_ui()
      )
    })

  }

  # Show the run button
  shinyjs::show(id = "run_analysis")

  # Analysis
  analysis_item_list <- list()
  analysis_idx <- 0
  for (analysis in cfg$analysis) {

    analysis_idx <- analysis_idx + 1
    tab_name <- paste0("analysis_tab_", analysis_idx)
    output_name <- paste0(tab_name, "_output")

    # Add the menu item
    analysis_item_list <- c(
      analysis_item_list,
      list(bs4Dash::menuItem(analysis$tab, tabName = tab_name))
    )

    # Create the main tab
    gen_maintab <- function(on, an) {
      force(on)
      force(an)
      output[[on]] <- renderUI({

        # Separate the pipeline elements from the analytics elements
        ppl_methods_bool <- sapply(an$methodList, function(x) stringr::str_starts(x$fn, "ppl_"))
        ppl_methods <- an$methodList[ppl_methods_bool]
        anl_methods <- an$methodList[!ppl_methods_bool]

        # Add the pipeline control elements
        ppl_controls <- sapply(ppl_methods, .method2fncall_ui)

        # Add the sub-tabs - one per method
        tab_panels <- sapply(anl_methods, .method2fncall_ui)

        # Output the finished page taglist (controls and tabpanel)
        shiny::tagList(
          do.call(shiny::fluidRow, ppl_controls),
          do.call(bs4Dash::tabsetPanel, tab_panels)
        )
      })
    }
    gen_maintab(output_name, analysis)
  }

  # Combine all the output analysis menu
  shinyjs::hide(id = "analysis_menuitem")
  output$analysis_menuitem <- bs4Dash::renderMenu({
    do.call(bs4Dash::menuItem, c(list(
      text = "Analysis",
      icon = shiny::icon("magnifying-glass-chart"),
      startExpanded = TRUE
    ), analysis_item_list
    ))
  })

}


.method2fncall_ui <- function(mthd) {

  # Convert method name to a UI function
  f_ui <- .get_func_from_name(mthd$fn, "_ui", mthd$rpkg)

  # Run the function with a unique id
  html_ui <- f_ui(digest::sha1(mthd))

  # If not a template, wrap the ui in a tabpanel and list
  if (!stringr::str_starts(mthd$fn, "tpl_")) {
    html_ui <- list(shiny::tabPanel(title = mthd$tab_lbl, html_ui))
  }

  html_ui
}


.cfg2server <- function(input, output, session, cfg, data_dir, n_max) {
  # Load RDV reactive
  session$userData$specialty <- cfg$dataset
  logger::log_info(glue::glue("Cohorts selected: {session$userData$specialty}"))
  rdvs_server <- ui_load_rdvs_server(data_dir, cfg$dataset, n_max = n_max, app_yaml=cfg)
  dataset_description <- load_dataset_description(data_dir, cfg$dataset)

  # Define cohort pipeline
  # Initial value
  initial_cohorts <- shiny::reactive({
    .initialCohorts2defn(cfg$initialCohorts, cfg$dataset)
  })

  # Patient select
  ps_cohorts <- ui_select_patient_server("main_patient_select", rdvs_server, initial_cohorts)

  # Cohort builder
  final_cohorts <- ui_select_cohort_server(rdvs_server, ps_cohorts)
  # Dataset segmentation (split by cohorts)
  run_reactive <- shiny::reactive(input$run_analysis)
  cohorts_servers <- segment_cohort_servers(final_cohorts, rdvs_server, run_reactive)

  # Interactive output analysis servers
  for (analysis in cfg$analysis) {
    for (mthd in analysis$methodList) {

      rtn_reactive <- .method2fncall_server(mthd, cohorts_servers)

      # Store returned reactive if there is a required output name
      if (!is.null(mthd$output) && is(rtn_reactive, "reactive")) {
        cohorts_servers[[mthd$output]] <- rtn_reactive
      }
    }
  }

  # Report output analysis server
  if (!is.null(cfg$outputs$pdf) && cfg$outputs$pdf) {
    populate_report(cfg, dataset_description, cohorts_servers, final_cohorts)
  }
}


.method2fncall_server <- function(mthd, cohorts_servers) {

  # Convert method name to a server function
  f_server <- .get_func_from_name(mthd$fn, "_server", mthd$rpkg)

  # Form the parameters for the function call
  param_list <- c(
    list(id = digest::sha1(mthd)),
    mthd$params
  )

  # Substitute known RDVs into parameters
  available_rdvs <- names(cohorts_servers)
  fsubs_rdvs <- function(idx = NULL) {
    if (is.null(idx)) {
      vals <- c()
      for (ii in seq(length(param_list))) {vals <- c(vals, fsubs_rdvs(ii))}
      return(vals)
    }
    n <- length(param_list[[idx]])
    if (n > 1 || is(param_list[[idx]], 'list')) {
      vals <- c()
      for (ii in seq(n)) {vals <- c(vals, fsubs_rdvs(c(idx,ii)))}
      return(vals)
    } else {
      if (param_list[[idx]] %in% available_rdvs)  return(list(idx))
      else                                        return(NULL)
    }
  }
  uses_rdvs <- fsubs_rdvs()
  for (idx in uses_rdvs) {
    param_list[[idx]] <- cohorts_servers[[param_list[[idx]]]]
  }

  # Remove any params not required for this function
  valid_params <- param_list[names(param_list) %in% formalArgs(f_server)]

  # Add defaults for any missing known servers (this does not work recursively)
  required_rdvs <- formalArgs(f_server)
  missing_rdvs <- required_rdvs[!required_rdvs %in% names(valid_params)]
  available_rdvs <- missing_rdvs[missing_rdvs %in% names(cohorts_servers)]
  if (length(available_rdvs) > 0) {
    required_reactives <- cohorts_servers[available_rdvs]
    valid_params <- c(valid_params, required_reactives)
  }

  # Call the server function
  do.call(f_server, valid_params)
}


.get_func_from_name <- function(fn, suffix, rpkg = NULL) {
  full_fn <- paste0(fn, suffix)
  if (is.null(rpkg))  return(get(full_fn))
  else                return(getFromNamespace(full_fn, rpkg))
}


#' Build the initial cohort definitions from the app YAML.
#'
#' Returns a named list (one entry per cohort label) of `Node` trees - the root
#' of each cohort's logic tree. This replaces the previous tibble-per-cohort
#' representation; the tree is resolved to a patient_list by `Node$evaluate()`.
#'
#' Each cohort config is expected to carry a recursive `tree:`
#'
#' Legacy cohorts with a flat `config:` list of clauses are still accepted and
#' wrapped into an AND of those filters over the base population.
#'
#' @param cohort_config The `cfg$initialCohorts` list parsed from the app YAML.
#' @param base_dataset Character; the dataset / specialty name.
#' @return Named list of root `Node`s.
.initialCohorts2defn <- function(cohort_config, base_dataset) {

  cohort_defn <- list()

  for (cc in cohort_config) {

    if (!is.null(cc$tree)) {
      # New recursive tree config
      cohort_defn[[cc$label]] <- .cohort_config2node(cc$tree)
    } else if (length(cc$config) > 0) {
      # Legacy flat config: AND together the filter clauses over the base
      filters <- lapply(cc$config, .cohort_config2node)
      cohort_defn[[cc$label]] <- Node$new(value_type = "and", children = filters)
    } else {
      # No clauses at all: the whole base population ("all patients"). Building
      # an empty AND here would be non-evaluable and throw at run time, so seed
      # a bare base node instead.
      cohort_defn[[cc$label]] <- Node$new(value_type = "base")
    }

  }

  cohort_defn
}


#' Recursively convert one YAML cohort node into a `Node`.
#'
#' Operator nodes (`and`/`or`/`not`) recurse into their `children`; leaf nodes
#' (`base`/`filter`) carry the clause fields. Accepts both the new `val_type`
#' key and the legacy `type` key for the node type, and both `value`/`val` for
#' the query value.
#'
#' @param node_config A single node from the cohort tree config.
#' @return A `Node`.
.cohort_config2node <- function(node_config) {

  value_type <- node_config$val_type %||% node_config$type

  if (value_type %in% c("and", "or", "not")) {
    children <- lapply(node_config$children, .cohort_config2node)
    return(Node$new(value_type = value_type, children = children))
  }

  # Leaf node (base / filter)
  Node$new(
    value_type = value_type,
    value      = node_config$value %||% node_config$val,
    rdv        = node_config$rdv,
    column     = node_config$column,
    query_type = node_config$query_type,
    inclusion  = node_config$inclusion,
    window     = node_config$window %||% c(0L, 0L)
  )
}
