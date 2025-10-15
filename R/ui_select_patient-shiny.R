#' Shiny UI module for the Select a Patient ID
#'
#' Shiny UI function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#'
#' @return Shiny taglist for the UI
#'
#' @family ui_select_patient
#' @export
ui_select_patient_ui <- function(id) {

  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(class = "mx-auto mw-720", shiny::column(width = 12,
      shiny::h1("Patient Selection"),
      shiny::selectizeInput(
        ns("select_patient"),
        paste("Search and select patient by ID"),
        choices = NULL,
      ),
      shiny::h2("Patient details", class = "pt-4"),
      shiny::tableOutput(ns("patient_info"))
    ))
  )
}


#' Shiny server module for the Select a Patient ID
#'
#' Shiny server function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#' @param rdv_server DESCRIPTION.
#'
#' @return Shiny moduleServer function for the server functionality
#'
#' @family ui_select_patient
#' @export
ui_select_patient_server <- function(id, rdv_server, initial_cohorts) {

  ns <- shiny::NS(id)

  shiny::moduleServer(
    id,
    function(input, output, session) {


      # HACKY observer to create a reactive that triggers once the UI has been
      # rendered. This is an issue because the dynamic UI might not render
      # before the reactives are run and therefore they will not be initialised
      ui_rendered <- shiny::reactiveValues()
      shiny::observeEvent(input$select_patient, {
        ui_rendered$first <- TRUE
      }, ignoreNULL = TRUE, ignoreInit = TRUE, once = TRUE)


      # Observer to set the patient selection options once pde is provided
      shiny::observe({

        # Re-call this once the UI has been rendered the first time
        ui_rendered$first

        project_ids <- sort(unique(rdv_server()$df_pde$project_id))
        shiny::updateSelectizeInput(
          session, "select_patient", choices = project_ids, server = TRUE
        )

      })


      # Observer to update patient details table
      shiny::observeEvent(input$select_patient, {
        output$patient_info <- shiny::renderTable({

          colnm <- function(x){
            x %>%
              stringr::str_replace_all("_name", "") %>%
              stringr::str_replace_all("_", " ") %>%
              stringr::str_to_title()
          }

          tbl <- rdv_server()$df_pde %>%
            dplyr::filter(project_id == isolate(input$select_patient)) %>%
            dplyr::select(tidyselect::any_of(c(
              "sex_name",
              "birth_date", "death_date",
              "ethnicity_name"
            ))) %>%
            dplyr::select_if(~ !any(is.na(.))) %>%
            dplyr::mutate(
              dplyr::across(
                tidyselect::ends_with("_date"),
                function(x) format(x, "%d %B %Y")
              )
            ) %>%
            dplyr::rename_with(colnm) %>%
            t()

          if (ncol(tbl) == 0) return(NULL)
          else                return(tbl)

        }, rownames = TRUE, colnames = FALSE, width = "100%")
      })


      # Reactive to return the selected patient
      shiny::reactive({

        # If no selection yet
        if (is.null(input$select_patient)) return(initial_cohorts())

        # Substitute selected value
        ic <- initial_cohorts()
        for (idx in 1:length(ic)) {
          for (idx2 in 1:nrow(ic[[idx]])) {
            ic[[idx]]$val[[idx2]] <- stringr::str_replace_all(ic[[idx]]$val[[idx2]], "SELECT_PROJECT_ID", input$select_patient)
          }
        }

        return(ic)
      })

    }
  )
}
