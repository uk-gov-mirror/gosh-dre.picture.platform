#' Run PICTURE
#'
#' Helper function to run PICTURE locally
#' Run this in the local folder as:
#'  devtools::load_all(); picture.platform::app_picture_run()
#' As with shiny::runApp, this does not return until ctrl+c or closed UI
#'
#' @family app_picture
#' @export
app_picture_run <- function(...) {

  # Create and run the app
  app <- app_picture(...)
  shiny::runApp(app)

}


#' Assemble the PICTURE App
#'
#' Helper function to assemble the UI and server functions for the app
#'
#' @return the PICTURE shiny app
#'
#' @family app_picture
#' @export
app_picture <- function(
  config_yaml = NULL, config = "default"
) {

  # Initialise logging
  if (!dir.exists("log")) dir.create("log")
  log_folder <- getwd()
  log_file <- file.path(log_folder,glue::glue("log/logfile_temp.log"))
  logger::log_formatter(logger::formatter_glue_or_sprintf)
  logger::log_warnings()
  logger::log_threshold("INFO")
  logger::log_layout(logger::layout_json())
  logger::log_appender(logger::appender_file(log_file))

  # Get config
  cfg <- .load_config_yaml(config_yaml, config)
  external_data_dir <<- cfg$external_data_dir
  app_config <- .app_yaml_to_config(cfg$app_yaml)

  # Build shiny app
  app_ui <- app_picture_ui(app_config)
  app_server <- app_picture_server(app_config, cfg$data_dir, n_max)
  if(cfg$infrastructure == "cloud"){
    shiny::shinyApp(
      ui = app_ui,
      server = app_server,
      options = list(port = 8080, launch.browser = FALSE, host = '0.0.0.0')
    )
  } else {
    shiny::shinyApp(
      ui = app_ui,
      server = app_server,
      options = list(launch.browser = TRUE)
    )
  }
}


#' Load the selected config YAML and format inputs
.load_config_yaml <- function(config_yaml, config) {

  # Load the base configuration
  if (is.null(config_yaml))
    config_yaml <- system.file("configs", "config.yaml", package = "picture.platform")
  cfg <- config::get(config = config, file = config_yaml)

  # Separate the config values
  apps_lookup <- function(fn) {
    if (file.exists(fn))
      return(fn)
    else if (fn == "*")
      return(list.files(system.file("apps", package = "picture.platform"), "*.yaml", full.names = T))
    else if (stringr::str_ends(fn, "/*")) {
      return(Sys.glob(paste0(fn, ".yaml")))
    } else
      return(system.file("apps", fn, package = "picture.platform"))
  }
  cfg$app_yaml <- sapply(cfg$app_yaml, apps_lookup)
  cfg$n_max <- as.numeric(cfg$n_max)

  # Log inputs
  system_info <- get_system_info()
  logger::log_info(glue::glue("System_info: {jsonlite::toJSON(system_info, auto_unbox = TRUE, force=TRUE)}"))
  env_info <- get_env_info()
  logger::log_info(glue::glue("Env_info: {jsonlite::toJSON(env_info, auto_unbox = TRUE, force=TRUE)}"))
  packages_info <- get_packages_info()
  logger::log_debug(glue::glue("Packages_installed: {jsonlite::toJSON(packages_info, auto_unbox = TRUE, force=TRUE)}"))
  logger::log_info("PICTURE App YAMLs: #{seq(cfg$app_yaml)} {cfg$app_yaml}")
  logger::log_info("PICTURE Data Directories: #{seq(cfg$data_dir)} {cfg$data_dir}")
  logger::log_info("PICTURE External Data Directory: {cfg$external_data_dir}")
  logger::log_info("PICTURE N Max: {cfg$n_max}")
  logger::log_info("PICTURE N Max: {cfg$infrastructure}")

  return(cfg)
}


#' Convert the app config yaml to an app config structure
.app_yaml_to_config <- function(app_yamls) {

  # For each input yaml
  id <- 1
  all_configs <- list()
  for (ay in app_yamls) {

    # Custom handlers for input lists dates
    datelist_hdl <- function(x) lapply(x, lubridate::as_datetime)

    # Load the app config yaml
    app_config <- yaml::read_yaml(ay, handlers = list(datelist = datelist_hdl))

    # Check against schema
    # TODO

    app_config$id <- id
    all_configs <- c(all_configs, list(app_config))
    id <- id + 1

  }
  return(all_configs)
}
