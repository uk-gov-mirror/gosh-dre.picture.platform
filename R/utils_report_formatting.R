
report_list2kable <- function(listin) {

  tibble::tibble(n1 = names(listin), v1 = unlist(listin)) %>%
    report_tibble2kable()
}

report_tibble2kable <- function(tibblein, col_names = NULL) {

  # Only designed to work with latex
  stopifnot(knitr::opts_knit$get('rmarkdown.pandoc.to') == "latex")

  tibblein %>%
    kableExtra::kable(format = "latex", escape = FALSE, col.names = col_names) %>%
    # Bold column names, if supplied
    {if(!is.null(col_names)) kableExtra::row_spec(., 0, bold = TRUE) else .} %>%
    # Format column 1
    kableExtra::column_spec(1, bold = TRUE, include_thead = TRUE) %>%
    kableExtra::column_spec(1, latex_column_spec = "m{4cm}") %>%
    # Full width
    kableExtra::kable_styling(full_width = T, latex_options = "hold_position")
}


report_patient_info_table <- function(lastname, firstname, mrn, dob, sex) {

  # Cast the dob to date, if necessary
  if(is.character(dob)) {
    dob <- as.Date(dob)
  } else {
    dob <- dob
  }

  # Combine the values
  tbl_pat <- list()
  tbl_pat$Patient <- paste0(
    stringr::str_to_upper(lastname), ", ", stringr::str_to_title(firstname))
  tbl_pat$MRN <- mrn
  tbl_pat$DOB <- format(dob, "%d %B %Y")
  tbl_pat$Age <- paste(round(lubridate::time_length(difftime(Sys.Date(), dob), "years"), 2), "years")
  tbl_pat$Sex <- sex

  report_list2kable(tbl_pat)

}


report_cohort_info_table <- function(cohort_defs) {

  tbl_out <- tibble::tibble(Cohort = character(), Description = character())

  for (cohort in names(cohort_defs)) {

    cohort_spec <- cohort_defs[[cohort]]
    desc <- paste(lapply(
      unique(cohort_spec$id), function(x) text_cohort_spec(cohort_spec, x)
    ), collapse = " and ")  # "\\newline and ")

    tbl_out <- tibble::add_row(tbl_out, Cohort = cohort, Description = desc)  # kableExtra::linebreak(desc))

  }

  tbl_out %>%
    report_tibble2kable(col_names = names(tbl_out))

}


report_analysis_info_table <- function(session_string) {

  tbl_ana <- list()
  tbl_ana$`PICTURE Version` <- paste("v", packageVersion("driveanalytics"))
  tbl_ana$Date <- format(Sys.time(), "%d %B %Y at %H:%M:%S")
  tbl_ana$Analyst <- get_username()
  tbl_ana$`Report ID` <- substr(digest::digest(paste0(paste0(tbl_ana, collapse = "-"), session_string)), 1, 12)

  report_list2kable(tbl_ana)

}


report_dataset_info_table <- function(dataset_summary) {

  # Format the date, first parsing the input as a date in case it's a string
  format_dateish <- function(dateish) {
    tryCatch(
      {
        format(as.Date(dateish), "%d %B %Y")
      },
      error = function(cond) {
        dateish
      }
    )
  }

  tbl <- list()
  tbl$Name <- dataset_summary$name
  tbl$Description <- dataset_summary$description
  tbl$Period <- paste(
    format_dateish(dataset_summary$min_activity_date), "to",
    format_dateish(dataset_summary$max_activity_date)
  )
  tbl$`Extraction Date` <- format_dateish(dataset_summary$extraction_date)

  report_list2kable(tbl)

}


get_username <- function() {
  stringr::str_extract(system("whoami", intern = T), ".*\\\\(.*)", group = 1)
}
