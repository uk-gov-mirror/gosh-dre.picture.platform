#' Small function to help scaffold a new module for the package
#'
#' Uses templates to scaffold the main files needed to create a new module
#'
#' @param name R code name to use for module (e.g. my_module_analysis).
#'             It is recommended to start the module with the three character
#'             abbreviation of the primary RDV type (e.g. "dia_NAME" for
#'             diagnoses), or "gen_NAME" in the case of a generic module that
#'             can work across many RDVs.
#' @param title Pretty text name for the module (e.g. "Analysis of something")
#' @param inputs List of function input names, if known
#'               (e.g. "df_pde, df_dia, some_constant"), optional
#'
#' @export
utils_scaffold_new_module <- function(
  name, title, inputs = NULL, target_dir = NULL,
  use_report = TRUE, use_testthat = TRUE, use_about = TRUE
) {

  # Build data list for usethis
  template_data <- list(
    name = name,
    title = title,
    inputs = inputs
  )

  # If target_dir isn't specified, use r package defaults as in driveanalytics
  if (is.null(target_dir)) {
    source_dir <- "./R/"
    report_dir <- "./inst/rmd/"
    testing_dir <- "./tests/testthat/"
    about_dir <- "./inst/about/"
  } else {
    source_dir <- report_dir <- testing_dir <- about_dir <- target_dir
  }

  # Main R code source file
  usethis::use_template(
    "name.R",
    data = template_data,
    save_as = file.path(source_dir, paste0(name, ".R")),
    package = "picture.platform")

  # Shiny R code source file
  usethis::use_template(
    "name-shiny.R",
    data = template_data,
    save_as = file.path(source_dir, paste0(name, "-shiny.R")),
    package = "picture.platform")

  # Report Rmd code source file
  if (use_report) {
    usethis::use_template(
      "name-report.Rmd",
      data = template_data,
      save_as = file.path(report_dir, paste0(name, "-report.Rmd")),
      package = "picture.platform")
  }

  # Testthat source file
  if (use_testthat) {
    usethis::use_template(
      "test-name.R",
      data = template_data,
      save_as = file.path(testing_dir, paste0("test-", name, ".R")),
      package = "picture.platform")
  }

  # About file
  if (use_about) {
    usethis::use_template(
      "name-text.Rmd",
      data = template_data,
      save_as = file.path(about_dir, paste0(name, "-text.Rmd")),
      package = "picture.platform")
  }

  # Vignette
  # usethis::use_vignette(name)

}
