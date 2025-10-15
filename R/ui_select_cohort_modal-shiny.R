
ui_select_cohort_modal_show <- function(rdv_names, ns) {

  available_rdvs <- stringr::str_match(rdv_names, "^df_(.*)")[,2]
  rdv_opts <- get_rdv_code(available_rdvs)

  select_rdv <- rdv_opts$rdv_code
  names(select_rdv) <- rdv_opts$label

  selectize_opts <- list(
    placeholder = 'Please select an option',
    onInitialize = I('function() { this.setValue(""); }')
  )

  md <- shiny::modalDialog(

    shiny::selectInput(ns("cohort_modal_inclusion"), "Inclusion criteria",
                inclusion_types, width = "100%"),

    shiny::sliderInput(ns("cohort_modal_window"), "Inclusion window",
                       -100, 100, c(0,0)),

    shiny::selectizeInput(ns("cohort_modal_select_rdv"), "Select category", select_rdv,
                   options = selectize_opts),

    conditional_group(ns, 1, selectize_opts),
    conditional_group(ns, 2, selectize_opts, FALSE),
    # conditional_group(ns, 3, selectize_opts, FALSE),

    footer = shiny::tagList(

      shiny::textOutput(ns("error_msg")),
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns("accept"), "Accept")

    )

  )

  shiny::showModal(md)


}


conditional_group <- function(ns, id, selectize_opts, add_another = TRUE) {

  # Get names
  select_rdv <- ns("cohort_modal_select_rdv")
  select_col <- ns("cohort_modal_select_col")
  select_val <- ns("cohort_modal_select_val")
  select_another <- ns("cohort_modal_select_another")

  select_col_this <- paste0(select_col, "_", id)
  select_val_this <- paste0(select_val, "_", id)
  select_another_this <- paste0(select_another, "_", id)
  select_another_prev <- paste0(select_another, "_", id-1)

  select_col_this_input <- paste0("input[\"", select_col_this, "\"]")

  type_lookup <- get_type_lookup()

  # Generate JS condition on whether to show this group
  condition <- ifelse(id == 1,
                      paste0("input[\"", select_rdv, "\"]"),
                      paste0("input[\"", select_another_prev, "\"]"))

  shiny::conditionalPanel(
    condition = condition,

    # Column selection
    shiny::selectizeInput(
      select_col_this, "Select parameter", list(),
      options = selectize_opts),

    # Factor select
    shiny::conditionalPanel(
      condition = paste0(type_lookup['select',]$js, ".indexOf(", select_col_this_input, ") > -1"),

      shiny::selectizeInput(paste0(select_val_this, "_select"), "Select from", list(),
                     multiple = TRUE, options = selectize_opts, ),
    ),

    # String match
    shiny::conditionalPanel(
      condition = paste0(type_lookup['text',]$js, ".indexOf(", select_col_this_input, ") > -1"),

      shiny::textInput(paste0(select_val_this, "_text"), "Search for"),
    ),

    # Date range
    shiny::conditionalPanel(
      condition = paste0(type_lookup['date_range',]$js, ".indexOf(", select_col_this_input, ") > -1"),

      shiny::dateRangeInput(paste0(select_val_this, "_daterange"), "Select date range",
                     start = default_start_date, end = Sys.Date(),
                     max = Sys.Date()),
    ),

    # Numeric range
    shiny::conditionalPanel(
      condition = paste0(type_lookup['numeric_range',]$js, ".indexOf(", select_col_this_input, ") > -1"),

      shiny::sliderInput(
        paste0(select_val_this, "_numericrange"),
        "Select range",
        -10000, 10000, c(-10000,10000)
      ),
    ),

    # Age range
    shiny::conditionalPanel(
      condition = paste0(type_lookup['age_range',]$js, ".indexOf(", select_col_this_input, ") > -1"),

      shiny::sliderInput(
        paste0(select_val_this, "_agerange"),
        "Select age range (years)",
        0, 18, c(0,18)
      ),
    ),

    # Button to add another condition
    shiny::conditionalPanel(
      condition = select_col_this_input,
      if (add_another)
        shiny::checkboxInput(select_another_this, "Add additional condition", FALSE)
    )
  )
}


conditional_group_server <- function(input, output, session, rdvs_server, selected_opts, id) {

  # Manual namespacing
  select_col_id <- paste0("cohort_modal_select_col_", id)
  select_val_id <- paste0("cohort_modal_select_val_", id, "_select")

  shiny::observe({

    col <- input[[select_col_id]]

    if (is.null(col)) return()

    selected_var <- selected_opts() %>%
      dplyr::filter(variable_code == col)

    if (col_is_select(col)) {

      df_name = paste0("df_", input$cohort_modal_select_rdv)
      selected_rdv <- rdvs_server()[[df_name]]

      if (!(col %in% names(selected_rdv))) return()

      code_detect <- stringr::str_match(col, "(.*_)(local|nat)_code$")
      label_name <- paste0(code_detect[2], "name")
      if (!is.na(code_detect[1]) & label_name %in% names(selected_rdv)) {

        df_choices <- selected_rdv %>%
          dplyr::select(!!as.name(col), !!as.name(label_name)) %>%
          dplyr::distinct(.keep_all = TRUE) %>%
          tidyr::drop_na() %>%
          dplyr::arrange(!!as.name(col)) %>%
          dplyr::mutate(label = paste(.[[1]], "-", .[[2]]))

        possible_vals <- setNames(df_choices[[1]], df_choices[[3]])

        # Hacky function to group the possible vals if there are lots of them
        if (length(possible_vals) > 26) {
          new_cats <- sort(unique(substr(possible_vals,1,1)))
          new_pvals <- list()
          for (nc in new_cats) {
            new_pvals[nc] = list(possible_vals[stringr::str_starts(possible_vals, nc)])
          }
          possible_vals <- new_pvals
        }

      } else {

        possible_vals <- sort(unique(na.omit(selected_rdv[[col]])))

      }

      # Update with server
      shiny::updateSelectizeInput(session, select_val_id, choices = possible_vals, server = TRUE)


    }

  })
}


ui_select_cohort_modal_observers <- function(input, output, session, rdvs_server) {

  selected_opts <- shiny::reactive({

    rdv <- input$cohort_modal_select_rdv
    if (is.null(rdv)) return()
    get_rdv_Variable_lookup() %>% dplyr::filter(rdv_code == rdv)

  })

  shiny::observe({

    choices <- selected_opts()$variable_code
    names(choices) <- selected_opts()$label

    output$error_msg <- shiny::renderText("")

    shiny::updateSelectInput(session, "cohort_modal_select_col_1", choices = choices)
    shiny::updateSelectInput(session, "cohort_modal_select_col_2", choices = choices)
    # shiny::updateSelectInput(session, "cohort_modal_select_col_3", choices = choices)

  })

  conditional_group_server(input, output, session, rdvs_server, selected_opts, 1)
  conditional_group_server(input, output, session, rdvs_server, selected_opts, 2)

  shiny::eventReactive(input$accept, {

    # Record input data to a DF
    df_output <- tibble::tibble(
      id = numeric(0),
      type = character(0),
      inclusion = character(0),
      window = list(),
      rdv = character(0),
      column = character(0),
      query_type = factor(0),
      val = list(),
    )

    rdv <- input$cohort_modal_select_rdv

    for (idx in 1:2) {

      col <- input[[paste0("cohort_modal_select_col_", idx)]]
      query_type <- get_variable_label(rdv, col, "filter_type")

      if(col_is_select(col)) {
        val <- input[[paste0("cohort_modal_select_val_", idx, "_select")]]
      } else if(col_is_date_range(col)) {
        val <- input[[paste0("cohort_modal_select_val_", idx, "_daterange")]]
      } else if(col_is_age_range(col)) {
        val <- input[[paste0("cohort_modal_select_val_", idx, "_agerange")]]
      } else if(col_is_numeric_range(col)) {
        val <- input[[paste0("cohort_modal_select_val_", idx, "_numericrange")]]
      } else {
        val <- input[[paste0("cohort_modal_select_val_", idx, "_text")]]
      }

      # Input validity check
      if (is.null(val)) val <- NA
      if (is.character(val)) if (val[[1]] == "") val <- NA
      if (col == "") col <- NA
      if (identical(query_type, character(0))) query_type <- NA

      df_output <- df_output %>% dplyr::add_row(
        id = 1,
        type = ifelse(idx == 1, "filter", "and"),
        inclusion = input$cohort_modal_inclusion,
        window = list(input$cohort_modal_window),
        rdv = input$cohort_modal_select_rdv,
        column = col,
        query_type = query_type,
        val = list(val)
      )

      # Stop looping if the next condition is not enabled
      select_another <- input[[paste0("cohort_modal_select_another_", idx)]]
      if (!is.null(select_another)) if (!select_another) break
    }

    # Check all inputs are valid
    if (nrow(df_output) == 0 | any(is.na(df_output))) {
      output$error_msg <- shiny::renderText("Missing inputs")
      return(NULL)

    } else {
      output$error_msg <- shiny::renderText("")
      shiny::removeModal()
      return(df_output)
    }

  })

}
