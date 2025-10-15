#' Print a cohort description as text
#'
#' @param df_spec The full cohort specification
#' @param idx The index of the stage of the cohort to format
format_cohort_spec <- function(df_spec, idx) {

  config <- df_spec %>%
    dplyr::filter(id == idx)

  n <- tail(config$n, 1)
  n_periods <- tail(config$n_periods, 1)
  type <- config$type[[1]]
  rdv <- config$rdv[[1]]

  element_pretty <- stringr::str_to_title(type)
  rdv_pretty <- stringr::str_to_title(get_rdv_code(rdv)$label)
  n_pretty <- ifelse(is.null(n) || is.na(n), "", paste0("(patients n = ", n, " / periods n = ", n_periods, ")"))

  if (config$id[[1]] == 1) {
    title <- paste("Base cohort", n_pretty)
    description <- paste("Full", config$val[[1]], "patient cohort")
  } else {
    title <- paste(element_pretty, "by", rdv_pretty, n_pretty)

    description <- NULL
    for (idx in seq(nrow(config))) {

      # Inputs
      col <- config$column[[idx]]
      query_type <- config$query_type[[idx]]
      inclusion_type <- config$inclusion[[idx]]
      val <- config$val[[idx]]

      # Format
      main <- shiny::tagList(shiny::strong(get_variable_label(rdv, col)))
      if (stringr::str_starts(query_type, "str_")) {
        verb <- stringr::str_match(query_type, "^str_(.*)$")[[2]]
        main <- shiny::tagList(main, verb, shiny::br(), shiny::span(paste(val, collapse = ", ")))
      } else if (query_type == "date_between") {
        main <- shiny::tagList(main, "is between",
                               shiny::br(), shiny::span(format(val[[1]], "%d %B %Y %H:%M:%S %Z"), "and"),
                               shiny::br(), shiny::span(format(val[[2]], "%d %B %Y %H:%M:%S %Z")))
      } else if (query_type == "age_between") {
        main <- shiny::tagList(main, "between", val[[1]], "years and", val[[2]], "years")
      } else if (query_type == "numeric_between") {
        main <- shiny::tagList(main, "is between", val[[1]], "and", val[[2]])
      }

      # Create description
      description <- shiny::tagList(
        description,
        if(idx > 1) shiny::p("and")
        else shiny::p(names(inclusion_types)[inclusion_types == inclusion_type]),
        shiny::p(main)
      )

    }
  }

  list("title" = title, "description" = description)

}


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
