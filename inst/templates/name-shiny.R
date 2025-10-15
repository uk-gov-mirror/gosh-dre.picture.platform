#' Shiny UI module for the {{title}}
#'
#' Shiny UI function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#'
#' @return Shiny taglist for the UI
#'
#' @family {{name}}
#' @export
{{name}}_ui <- function(id, title = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(

    bs4Dash::tabBox(
      title = title,
      width = 12,
      side = "right",
      shiny::tabPanel(
        title = "Title Goes Here..."
        # TODO #
      ),
      ui_info_panel("{{name}}-text.Rmd")
    )

  )
}


#' Shiny server module for the {{title}}
#'
#' Shiny server function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance. {{ #inputs }}
#' @param {{.}} DESCRIPTION. {{ /inputs }}
#'
#' @return Shiny moduleServer function for the server functionality
#'
#' @family {{name}}
#' @export
{{name}}_server <- function(id, {{inputs}}) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      # TODO

    }
  )
}
