# Used in development --deprecated-- to run the YAML maker app from the R console.
yaml_maker_app_run <- function(...) {
    shiny::runApp(yaml_maker_app(...))
}

yaml_maker_app <- function(mod_dirs = c("driveanalytics/R"), namespaces = c("driveanalytics", "picture.platform")) {
    shiny::shinyApp(
        ui      = yaml_maker_ui(mod_dirs),
        server  = yaml_maker_server(mod_dirs, namespaces),
        options = list(launch.browser = TRUE)
    )
}


yaml_maker_server <- function(mod_dirs, namespaces = c("driveanalytics", "picture.platform")) {
    function(input, output, session) {

        all_mods <- .get_avaliable_mod(mod_dirs)

        # Cache param names at startup
        mod_params <- stats::setNames(
            lapply(all_mods, .get_mod_params, namespaces = namespaces),
            all_mods
        )

        # Initialise every module upfront with empty param values.
        # Slots persist for the lifetime of the session, so removing a module from
        # the bucket and re adding it automatically restores its previous values.
        param_store <- shiny::reactiveValues()
        tab_title_store <- shiny::reactiveValues()

        for (mod in all_mods) {
            param_store[[mod]] <- stats::setNames(
                as.list(rep("", length(mod_params[[mod]]))),
                mod_params[[mod]]
            )
            tab_title_store[[mod]] <- ""
        }

        # Whenever the bucket changes, sweep all currently visible inputs into
        # param_store before renderUI destroys them on its next run.
        shiny::observeEvent(input$modules_added, {
            for (mod in all_mods) {
                for (p in mod_params[[mod]]) {
                    val <- input[[paste0(mod, "__", p)]]
                    if (!is.null(val)) param_store[[mod]][[p]] <- val
                }
                val <- input[[paste0(mod, "_title")]]
                if (!is.null(val)) tab_title_store[[mod]] <- val
            }
        })

        output$module_param_boxes <- shiny::renderUI({
            mods <- input$modules_added
            if (length(mods) == 0) return(NULL)

            .module_param_boxes(param_store, mods, mod_params, tab_title_store)
        })

        # Standalone, data-free cohort builder (only reads the RDV lookup CSVs).
        # Returns a reactive giving the named list of cohorts.
        cohort_defn <- cohort_builder_server("yaml_cohort")

        yaml_text <- reactive({
            yaml::as.yaml(list(
                title          = input$app_title,
                description    = input$app_description,
                creator        = input$app_creator,
                img            = input$app_png,
                dataset        = input$app_dataset,
                initialCohorts = .cohorts_to_initial_config(cohort_defn()),
                analysis       = .analysis(input, mod_params, namespaces)
            ))
        })

        output$yaml_out <- shiny::renderText({
            yaml_text()
        })

        shiny::observeEvent(input$yaml_out_button, {
            .create_yaml(input, yaml_text())
        })

        data_preview <- shiny::reactiveValues()

        # Update data select dropdown when simple_names or dataset changes
        shiny::observe({
            choices <- .get_data_names(
                simple_names = input$simple_names,
                dataset      = input$app_dataset
            )
            shiny::updateSelectInput(session, "data_select", choices = choices, selected = input$data_select)
        })

        output$data_preview_table <- DT::renderDT({
            paste0("picture.platform/DummyData/parquet/", input$data_select) %>%
                arrow::read_parquet() %>%
                head()
            }, 
            options = list(
                scrollX = TRUE,
                dom = 'tip'
            )
        )
    }
}

# Returns param names for a module's _server and _ui functions, excluding id/title.
# Tries each namespace in order and uses the first that resolves the function.
.get_mod_params <- function(mod_name, namespaces = c("driveanalytics", "picture.platform")) {
    get_param_names <- function(fn_name) {
        fns <- lapply(namespaces, function(ns) {
            tryCatch(getFromNamespace(fn_name, ns), error = function(e) NULL)
        })
        fn <- Filter(Negate(is.null), fns)
        if (length(fn) == 0) return(character(0))
        names(formals(fn[[1]]))
    }

    all_params <- unique(c(
        get_param_names(paste0(mod_name, "_server")),
        get_param_names(paste0(mod_name, "_ui"))
    ))
    setdiff(all_params, c("id"))
}

.analysis <- function(input, mod_params, namespaces = c("driveanalytics", "picture.platform")) {
    get_rpkg <- function(mod) {
        rpkg <- lapply(namespaces, function(ns) {
            tryCatch(
                environmentName(environment(getFromNamespace(mod, ns))),
                error = function(e) NULL
            )
        })
        if (length(Filter(Negate(is.null), rpkg)) >= 1) {
            return (Filter(Negate(is.null), rpkg)[[1]])
        }
        NULL

    }
    lapply(input$modules_added, function(mod) {
        list(
            "tab" = input[[paste0(mod, "_title")]],
            "methodList" = list(
                "fn" = mod,
                "rpkg" = get_rpkg(mod),
                "params" = setNames(lapply(mod_params[[mod]], function(p) {
                    input[[paste0(mod, "__", p)]]
                }), mod_params[[mod]])
            )
        )
    })
}

# Serialize the cohort-builder output (a named list of cohorts, each a list of
# filter-clause lists from cohort_builder_server) into the app YAML's
# `initialCohorts` structure. This mirrors .initialCohorts2defn
# (app_picture_server.R). Cohorts with no filters emit just a label (the
# "All Patients" shape). A window is written only when it is non-zero.
.cohorts_to_initial_config <- function(cohorts) {
    if (length(cohorts) == 0) return(list(list(label = "All Patients")))

    lapply(names(cohorts), function(label) {
        clauses <- cohorts[[label]]
        if (length(clauses) == 0) return(list(label = label))

        config <- lapply(clauses, function(cl) {
            entry <- list(
                type       = cl$type,
                rdv        = cl$rdv,
                column     = cl$column,
                inclusion  = cl$inclusion,
                val        = cl$val,
                query_type = cl$query_type
            )
            if (!is.null(cl$window) && !all(unlist(cl$window) == 0)) {
                entry$window <- cl$window
            }
            entry
        })
        list(label = label, config = config)
    })
}

.module_param_boxes <- function(param_store, mods, mod_params, tab_title_store) {
    shiny::tagList(
        lapply(mods, function(mod) {
            saved <- param_store[[mod]]

            # Create Box With Title value
            bs4Dash::box(
                title       = mod,
                width       = 12,
                collapsible = TRUE,
                shiny::tagList(
                    shiny::textInput(
                        inputId = paste0(mod, "_title"),
                        label = "Tab title",
                        value = tab_title_store[[mod]]
                    ),
                    .module_param_ui_inputs(mod_params, mod, saved)
                )
            )
        })
    )
}

.module_param_ui_inputs <- function(mod_params, mod, saved) {
    lapply(mod_params[[mod]], function(p) {
        # Double-underscore avoids collisions with underscores in names
        if (grepl("^df_", p)) {
            shiny::selectInput(
                inputId  = paste0(mod, "__", p),
                label    = p,
                choices  = picture.platform::rdv_list,
                selected = saved[[p]] # Change it so `if (p %in% picture.platform::rdv_list) p` the first time only for better UX
            )
        } else {
            shiny::textInput(
                inputId = paste0(mod, "__", p),
                label   = p,
                value   = saved[[p]]
            )
        }
    })
}

.create_yaml <- function(input, yaml_out, path = "picture.platform/inst/apps", session = shiny::getDefaultReactiveDomain()) {
    file_path <- file.path(path, paste0(sub("\\.yaml$", "", input$yaml_out_filename), ".yaml"))
    dir_path <- dirname(file_path)

    if (file.exists(file_path)) {
        shiny::showNotification(sprintf("File '%s' already exists. Not overwriting.", file_path), type = "error", session = session)
        return (invisible(NULL))
    } else if (!dir.exists(dir_path)) {
        shiny::showNotification(sprintf("Directory '%s' does not exist. Please check the filename or path.", dir_path), type = "error", session = session)
        return (invisible(NULL))
    } else {
        write(yaml_out, file = file_path)
        shiny::showNotification(sprintf("YAML file written to '%s'", file_path), type = "message", session = session)
        return (invisible(NULL))
    }
}
