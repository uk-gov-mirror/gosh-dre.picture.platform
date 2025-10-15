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
          shiny::div(id = "cohort_element_column"),
          shiny::actionButton(ns("cohort_add_stage"), "Add new filter", icon = shiny::icon("filter")),
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

cohort_defn_row2box <- function(config, idx) {

  cohort_desc <- format_cohort_spec(config, idx)

  bs4Dash::bs4Card(
    class = "cohort_element",
    title = cohort_desc$title,
    collapsed = TRUE,
    width = 12,
    cohort_desc$description
  )
}

regen_cohort_taglist <- function(df_cohort_defn = NULL) {

  # Clear old cohort UI elements
  removeUI("#cohort_element_column >" , multiple = TRUE)

  # Add in new ones
  for (idx in unique(df_cohort_defn$id))
    insertUI("#cohort_element_column",
             ui = cohort_defn_row2box(df_cohort_defn, idx))
}


#' Shiny server module for the Filter a Patient Cohort
#'
#' Shiny server function encapsulated by namespace 'id'
#'
#' @param xxxxxxx Character label of the namespace for the module instance.
#' @param xxxxxxx DESCRIPTION.
#'
#' @return Shiny moduleServer function for the server functionality
#'
#' @family ui_select_cohort
#' @export
ui_select_cohort_server <- function(rdvs_server, input_cohorts) {

  ns <- shiny::NS("select_cohort_mod")

  shiny::moduleServer(
    "select_cohort_mod",
    function(input, output, session) {

      # Pushed input cohorts
      shiny::observe({
        session$userData$cohorts <- input_cohorts()
        regen_cohort_list()
        regen_patient_lists()

        # Regenerate based on selected cohort
        currcoh <- shiny::isolate(input$current_cohorts)
        if (!is.null(currcoh))
          selected_cohort <- session$userData$cohorts[[currcoh]]
        else
          selected_cohort <- session$userData$cohorts[[1]]
        regen_cohort_taglist(selected_cohort)
      })

      # Pushed config observer
      shiny::observe({
        # HACK to force regen
        input$cohort_add
        # Update the cohort list
        selected <- regen_cohort_list()
      })

      # Pushed config observer
      shiny::observe({
        # HACK to force regen
        input$cohort_remove
        # Update the cohort list
        selected <- regen_cohort_list()
      })

      # Update cohort description
      shiny::observe({
        # Skip if not initialised
        if (is.null(input$current_cohorts)) return()

        # Regenerate based on selected cohort
        selected_cohort <- session$userData$cohorts[[input$current_cohorts]]
        regen_cohort_taglist(selected_cohort)
      })

      # Add a new cohort
      shiny::observeEvent(input$cohort_add, {

        # Compute new label
        cohort_labels <- names(session$userData$cohorts)
        new_cohort_label <- LETTERS[which(!(LETTERS %in% cohort_labels))[1]]

        # Add the cohort to the user session
        session$userData$cohorts[[new_cohort_label]] <-
          tibble::tibble(id = 1, type = "base", rdv = NA, val = list(session$userData$specialty))
        regen_patient_lists()

        # Update the UI
        regen_cohort_list(new_cohort_label)

        logger::log_info(glue::glue("Adding new cohort: {new_cohort_label}"))

        # Launch the cohort modal for the first criteria
        launch_cohort_modal()
      })

      # Remove a cohort
      shiny::observeEvent(input$cohort_remove, {
        if (is.null(input$current_cohorts)) return()
        session$userData$cohorts <- delete_cohort(session$userData$cohorts, input)
        regen_patient_lists()
        shiny::updateSelectInput(session, "current_cohorts",
                          choices = names(session$userData$cohorts),
                          selected = tail(names(session$userData$cohorts), n=1))
      })

      # Rename a cohort
      shiny::observeEvent(input$cohort_rename, {
        rename_cohort_modal()
      })

      shiny::observeEvent(input$accept_rename, {
        logger::log_info("Renaming cohort")
        session$userData$cohorts <- rename_cohort(session$userData$cohorts, input)
        # Reload the data
        regen_patient_lists()
        # Update the UI
        regen_cohort_list(input$cohort_modal_name)
        close_rename_cohort_modal(ns)
      })



      # Observer function to open the new cohort stage modal dialog
      shiny::observeEvent(input$cohort_add_stage, {
        launch_cohort_modal()
      })

      # Reactive element to return the modal dialog's cohort stage
      modal_reactive <- ui_select_cohort_modal_observers(input, output, session, rdvs_server)

      # Reactive function to update the cohort spec from the modal
      shiny::observeEvent(modal_reactive(), {

        # Check the new cohort stage is valid
        new_cohort_stage <- modal_reactive()
        if (!is.null(new_cohort_stage)) {

          # Set the id for the new cohort stage
          new_id <- max(session$userData$cohorts[[input$current_cohorts]]$id) + 1
          new_cohort_stage <- dplyr::mutate(new_cohort_stage, id = new_id)

          # Add to the current cohort
          session$userData$cohorts[[input$current_cohorts]] <-
            dplyr::bind_rows(
              session$userData$cohorts[[input$current_cohorts]],
              new_cohort_stage
            )
          regen_patient_lists()
        }
        regen_cohort_taglist(session$userData$cohorts[[input$current_cohorts]])
      })

      # Update session cohort patient lists
      regen_patient_lists <- function(){

        session$userData$cohorts <- lapply(
          session$userData$cohorts,
          update_patient_lists,
          df_rdvs = rdvs_server()
        )

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

      # Function to launch the cohort criteria modal
      launch_cohort_modal <- function() {
        ui_select_cohort_modal_show(names(rdvs_server()), ns)
      }

      rename_cohort_modal <- function() {
        ui_rename_cohort_modal(ns)
      }

      shiny::reactive({
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
  cohorts <- cohorts[-which(names(cohorts) == input$current_cohorts)]
  return(cohorts)
}
