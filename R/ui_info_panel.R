#' Function to generate standardised info panel designs
#'
#' @export
ui_info_panel <- function(input_filename) {

  # Use package lookup if provided
  if (file.exists(input_filename)) {
    input_file <- input_filename
  } else {
    input_file <- system.file("about", input_filename, package = "picture.platform")
  }

  # Support old Rmd files
  if (stringr::str_ends(input_filename, ".Rmd")) {
    about_html <- markdown::markdownToHTML(file = input_file, fragment.only = T)
  } else {
    about_html <- readLines(input_file)
  }


  shiny::tabPanel(
    class = "about-info-panel",
    title = "About", icon = shiny::icon("circle-info"),
    shiny::HTML(about_html)
  )
}
