#' Core UI structure for bootstrap 4 based basic shiny UI
#'
#' @return Shiny taglist for the bootstrap based app UI
#'
#' @export
ui_bootstrap_gosh <- function(logo_xl, logo_xs, sidebar_menu, body, ctrlbar = NULL) {

  shiny::addResourcePath(prefix = 'www', directoryPath = system.file("www", package = "picture.platform"))

  shiny::fluidPage(

    shiny::tagList(

      shinyjs::useShinyjs(),

      shiny::tags$head(
        shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css"),
        shiny::tags$script(src = "www/core.js"),
        shiny::tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Montserrat"),
        shiny::tags$link(rel = "shortcut icon", href = "www/favicon.ico")
      ),

      bs4Dash::bs4DashPage(

        header = bs4Dash::bs4DashNavbar(
          title = shiny::tagList(
            shiny::div(class = "picture-logo logo-xl", logo_xl),
            shiny::div(class = "picture-logo logo-xs", logo_xs),
          ),
          controlbarIcon = shiny::icon("sliders")
        ),

        sidebar = bs4Dash::bs4DashSidebar(
          skin = "light",
          sidebar_menu,
          customArea = shiny::div(
            shiny::div(class = "float-left", shiny::img(src='www/GOSH_DRIVE_Logo_RGB.png', height = 50)) #,
            # shiny::div(
            #   shiny::img(class = "logo-xl hide-when-dark", src='www/GOSH_FT_Logo_White_RGB.png', height = 50),
            #   shiny::img(class = "logo-xl hide-when-light", src='www/GOSH_FT_Logo_Colour_RGB.png', height = 50)
            # )
          )
        ),

        body = bs4Dash::dashboardBody(body),

        # controlbar = bs4Dash::bs4DashControlbar(
        #   skin = "light",
        #   ctrlbar
        # ),

        dark = FALSE,
        fullscreen = TRUE
      )
    )
  )
}
