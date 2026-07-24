#' Print a cohort description as text
#'
#' @param df_spec The full cohort specification
#' @param idx The index of the stage of the cohort to format
text_cohort_spec <- function(df_spec, idx) {

  # browser()

  config <- df_spec %>%
    dplyr::filter(id == idx)

  rdv <- config$rdv[[1]]
  rdv_pretty <- stringr::str_to_title(get_rdv_code(rdv)$label)

  if (config$id[[1]] == 1) {
    txt <- paste0("All patients from the '", stringr::str_replace_all(config$val[[1]], "_", " "), "' patient cohort")
  } else {

    txt <- "who had"
    for (idx in seq(nrow(config))) {

      # Inputs
      col <- config$column[[idx]]
      query_type <- config$query_type[[idx]]
      inclusion_type <- config$inclusion[[idx]]
      val <- config$val[[idx]]

      # Format
      main <- paste("\\newline", rdv_pretty, " ", get_variable_label(rdv, col), "that")

      if (query_type == "str_starts") {
        verb <- "starts with"
        main <- paste(main, verb, paste(val, collapse = ", "))

      } else if (stringr::str_starts(query_type, "str_") && query_type != "str_starts") {
        verb <- stringr::str_match(query_type, "^str_(.*)$")[[2]]
        main <- paste(main, verb, paste(val, collapse = ", "))
      } else if (query_type == "date_between") {
        main <- paste(main, "is between", format(val[[1]], "%d %B %Y"), "and", format(val[[2]], "%d %B %Y"))
      } else if (query_type == "age_between") {
        main <- paste(main, "is between", val[[1]], "and", val[[2]])
      } else if (query_type == "numeric_between") {
        main <- paste(main, "is between", val[[1]], "and", val[[2]])
      }

      txt <- paste(txt, main, "\\newline", "Data (inclusion type):", gsub("[[:punct:]]", " ", inclusion_type), "event")

    }
  }

  txt

}
