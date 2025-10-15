#' Helper (UI-less) server function to load all RDVs for a given specialty
#' It can load in either all rdvs in a folder or if "limited" is specified in app
#' yaml it will load only the rdvs and rdv columns specified in the app yaml
#'
#' @param data_dir string; file path of rdvs to load
#' @param select_specialty string;  name of hospital specialty to use
#' @param n_max int; max rows to load
#' @param selected_rdvs list; list of rdvs and columns that are needed
#' @param app_yaml list; app yaml loaded as a list
#' @param show_progress boolean; show progress bar
#'
#' @return rdvs list; list of rdvs loaded into picture as tibbles
#' @export
ui_load_rdvs_server <- function(
  data_dir, select_specialty = NULL, n_max = Inf, selected_rdvs = NULL, app_yaml=NULL,
  show_progress = TRUE
) {

  # Get table of all available RDVs in data directory
  all_rdvs <- get_rdvs_in(data_dir)

  # Default to all available RDV types if none are input
  if(is.null(selected_rdvs))
    selected_rdvs <- unique(all_rdvs$type)
  # Default input for select specialty server
  if (is.null(select_specialty))
    select_specialty_server <- shiny::reactive({unique(all_rdvs$group)})
  else if (is.character(select_specialty))
    select_specialty_server <- shiny::reactive({select_specialty})
  else
    select_specialty_server <- select_specialty

  # Set default value for amount_of_data_to_load and check value is allowed
  if (!("amount_of_data_to_load" %in% names(app_yaml)) ||
      !(app_yaml$amount_of_data_to_load %in% c("all", "limited"))) {
    app_yaml$amount_of_data_to_load <- "all"
  }

  shiny::reactive({


    # Get available RDVs for the selected specialty
    filtered_rdvs <- all_rdvs %>%
      dplyr::filter(
        type %in% selected_rdvs,
        group %in% select_specialty_server()
      )
    n <- nrow(filtered_rdvs)
    # rdv_cols is empty unless limited is being used (its used to determine how data is loaded)
    rdv_cols <- ""
    if(app_yaml$amount_of_data_to_load == "limited"){
      rdv_cols <- get_rdv_cols(app_yaml, filtered_rdvs)
    }

    # Nothing to do if none selected
    if (n == 0) {
      shiny::showNotification("Warning: The required dataset was not found in the provided data directories")
      logger::log_warn("Warning: The required dataset was not found in the provided data directories")
      return( list() )
    }

    # Show a progress bar while loading data
    if (show_progress) {
      progress <- shiny::Progress$new(max = n)
      on.exit(progress$close())
      progress$set(message = "Loading data", value = 0)
    }

    # Load those RDVs in a named list
    rdvs = list()
    df_orders = NULL
    for (idx in seq(n)) {

      # Get RDV details
      rdv <- filtered_rdvs[idx,]
      # Increment progress bar
      if (show_progress) progress$set(idx-1, detail = rdv$type)
      # Load data
      df_name <- paste0("df_", rdv_lookup(rdv$type, "rdv_type", "rdv_code"))
      if(typeof(rdv_cols) == "list"){
        if(rdv$type %in% names(rdv_cols)){
          parquet_df <- arrow::read_parquet(as.character(rdv$filename), col_select = rdv_cols[[rdv$type]])
        }
      }else{
        parquet_df <- arrow::read_parquet(as.character(rdv$filename))
      }
      df <- tibble::as_tibble(parquet_df, n_max = n_max)

      # Cast all the datetimes as actual datetimes rather than just dates
      df <- df %>%
        dplyr::mutate(dplyr::across(dplyr::ends_with("_datetime"), safe.as.POSIXct))

      # RDV specific wrangling
      if("patient_diagnosis" %in% names(rdv_cols) & all(df_name %in% c("df_dia_conditions", "df_dia_other"))){

          df_dia_conditions <- df %>%
            dplyr::filter(stringr::str_starts(diag_local_code, "[A-Ua-u]"))
          rdvs["df_dia_conditions"] <- list(df_dia_conditions)
          df_dia_other <- df %>%
            dplyr::filter(stringr::str_starts(diag_local_code, "[V-Zv-z]"))
          rdvs["df_dia_other"] <- list(df_dia_other)
      }
      else if("patient_procedures" %in% names(rdv_cols) & all(df_name %in% c("df_pro_opcs", "df_pro_lab",
                                                                        "df_pro_img", "df_pro_other"))){

          # PROCEDURES
          df_pro_principal <- df %>%
            dplyr::filter(principal_procedure == 1)
          rdvs["df_pro_principal"] <- list(df_pro_principal)
          df_pro_opcs <- df %>%
            dplyr::filter(stringr::str_starts(proc_code_set, "(?i)OPCS"))
          rdvs["df_pro_opcs"] <- list(df_pro_opcs)
          df_pro_lab <- df %>%
            dplyr::filter(stringr::str_starts(proc_local_code, "(?i)LAB"))
          rdvs["df_pro_lab"] <- list(df_pro_lab)
          df_pro_img <- df %>%
            dplyr::filter(stringr::str_starts(proc_local_code, "(?i)IMG"))
          rdvs["df_pro_img"] <- list(df_pro_img)
          df_pro_other <- df %>%
            dplyr::filter(stringr::str_starts(proc_local_code, "(?i)OPCS|LAB|IMG", negate = TRUE))
          rdvs["df_pro_other"] <- list(df_pro_other)
      }
      else {
        # ALL OTHER RDVs
        rdvs[df_name] <- list(df)
      }

      }

    # Update and close progress bar
    if (show_progress) progress$set(n, detail = "completed")
    Sys.sleep(0.2)
    rdvs

  })

}


safe.as.POSIXct <- function(x) lubridate::as_datetime(x, tz = "")


#' Load the description file for the selected specialty, if available
load_dataset_description <- function(data_dir, specialty) {

  desc_file <- get_descriptions_in(data_dir) %>%
    dplyr::filter(group %in% specialty) %>%
    dplyr::pull(filename)

  # Check only one yaml found
  if (length(desc_file) > 1) {
    print("Multiple description files found, using the first")
    desc_file <- desc_file[[1]]
  }

  if (length(desc_file) == 1) {

    # Load description
    yaml::read_yaml(desc_file)[[1]]

  } else {

    # Default description
    guess_name <- specialty %>%
      stringr::str_replace_all("_", " ") %>%
      stringr::str_to_title()
    list(
      name = guess_name,
      description = guess_name,
      min_activity_date = "Unknown",
      max_activity_date = "Unknown",
      extraction_date = "Unknown"
    )
  }
}
