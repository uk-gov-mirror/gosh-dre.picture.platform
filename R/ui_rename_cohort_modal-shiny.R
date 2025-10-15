ui_rename_cohort_modal <- function(ns) {

  md <- shiny::modalDialog(

    shiny::textInput(ns("cohort_modal_name"), "Cohort new name"),

    footer = shiny::tagList(

      shiny::textOutput(ns("error_msg")),
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns("accept_rename"), "Accept")

    ),
  )

  shiny::showModal(md)

}

close_rename_cohort_modal <- function(ns) {
  shiny::removeModal()
}
