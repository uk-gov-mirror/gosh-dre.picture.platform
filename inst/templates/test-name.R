#' Unit testing for {{title}}

#' Test runs the low-level module on some test data to check correctness
test_that("run data", {

  mod_output <- picture.platform::{{{ name }}}(TODO_TESTING_INPUTS)

  # Output data
  testthat::expect_equal(
    TODO
  )

})


#' Test checks that we can create the shiny app and can be used for debugging
test_that("shiny app", {

  app_ui <- shiny::fluidPage(picture.platform::{{{ name }}}_ui("test"))
  app_server <- function(input, output, session) {
    TODO_ADD_INPUT_REACTIVES
    picture.platform::{{{ name }}}_server("test", TODO_TESTING_INPUTS)
  }

  app <- shiny::shinyApp(ui = app_ui, server = app_server)
  testthat::expect_equal(class(app), "shiny.appobj")

  # shiny::runApp(app) # Use this line to run app interactively for debugging
})


#' Tests that the shiny server functions correctly
test_that("shiny server", {

  # To test shiny, we can test the server functionality, but not the UI yet

  # Load the 'true' values from the raw function
  mod_extern <- picture.platform::{{{ name }}}(TODO_TESTING_INPUTS)

  # Test the shiny server module
  shiny::testServer(
    picture.platform::{{{ name }}}_server,
    args = list(
      TODO_TESTING_INPUTS # Module inputs go here as named list
    ),
    {
      # It is necessary to set at least one input to get this to work
      session$setInputs( TODO )

      # Standard testthat tests on module reactives/outputs
      testthat::expect_equal(TODO_OUTPUT_NAME(), mod_extern)
      testthat::expect_snapshot(TODO_MORE_TESTING)
    }
  )

})


#' Tests that the module has a functional markdown file
test_that("markdown", {

  rmd_file <- system.file("rmd", "{{{ name }}}-report.Rmd", package = "picture.platform")

  # Markdown parameters
  render_params <- list(
    # RMD parameters go here as a named list
  )
  output_dir <- withr::local_tempdir(clean = FALSE)

  # HTML report
  result_html <- rmarkdown::render(
    input = rmd_file,
    output_file = output_dir,
    params = render_params,
    output_format = rmarkdown::html_document()
  )

  # PDF report
  result_pdf <- rmarkdown::render(
    input = rmd_file,
    output_file = output_dir,
    params = render_params,
    output_format = rmarkdown::pdf_document(includes = list(in_header = tex_preamble))
  )

  # Tests
  testthat::expect_true(file.exists(result_html))
  testthat::expect_true(file.exists(result_pdf))
  testthat::expect_snapshot_file(result_html, "report.html")
  # testthat::expect_snapshot_file(result_pdf, "report.pdf") # PDF not supported
  # testthat::expect_snapshot(file.info(result_pdf)$size)

  # browseURL(result_html) # Use this line to run app interactively for debugging
  # browseURL(result_pdf) # Use this line to run app interactively for debugging
})
