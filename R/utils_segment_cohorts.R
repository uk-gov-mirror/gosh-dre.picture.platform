
segment_cohort_servers <- function(cohort_defn, rdvs_server, run_reactive){
  lapply(picture.platform::rdv_list, rdv_reactive, rdvs_server = rdvs_server, cohort_defn = cohort_defn, run_reactive = run_reactive)
}


rdv_reactive <- function(rdv, rdvs_server, cohort_defn, run_reactive) {
  shiny::eventReactive(run_reactive(), {

    rdv_selected <- rdvs_server()[[rdv]]

    if (is.null(rdv_selected)) {
      logger::log_warn(sprintf("Warning: App requests an RDV (%s) that is not available. Returning empty data.", rdv))
      shiny::showNotification(sprintf("Warning: App requests an RDV (%s) that is not available. Returning empty data.", rdv))
      return(NULL)
    }

    filter_by_cohorts(rdv_selected, cohort_defn(), rdvs_server())
  })
}


# Filter an RDV based on a cohort definition. `cohorts` is a named list of `Node`
# trees; each root resolves to a patient_list via `Node$evaluate()`, cached on
# the node (R6) so the 16 per-RDV reactives trigger evaluation only once.
filter_by_cohorts <- function(rdv, cohorts, df_rdvs) {

  # If no cohort, return the empty data
  if (length(cohorts) == 0) {
    return(NULL)
  }


  rdv_out <- tibble::tibble()
  for (idx in seq(length(cohorts))) {

    cohort_root <- cohorts[[idx]]

    # Skip cohorts whose tree not in an evaluable state yet 
    if (inherits(cohort_root, "Node") && !cohort_root$is_evaluable()) {
      logger::log_warn(glue::glue(
        "Skipping non-evaluable cohort '{names(cohorts)[[idx]]}'."
      ))
      next
    }

    # Use the cached membership if already resolved
    df_init_patient_list <- cohort_root$patient_list
    if (is.null(df_init_patient_list)) {
      df_init_patient_list <- tryCatch(
        cohort_root$evaluate(df_rdvs),
        error = function(e) {
          logger::log_warn(glue::glue(
            "Failed to evaluate cohort '{names(cohorts)[[idx]]}': {conditionMessage(e)}"
          ))
          NULL
        }
      )
    }
    if (is.null(df_init_patient_list)) {
      next
    }

    df_patient_list <- df_init_patient_list %>%
      dplyr::group_by(project_id) %>%
      # Add a cohort ID that allows the same patient to appear multiple distinct
      # times in the cohort at different periods
      dplyr::mutate(cohort_id = paste0(
        project_id, "-",
        stringr::str_pad(dplyr::row_number(project_id), 6, pad = "0")
      )) %>%
      dplyr::ungroup()

    rdv_out <- dplyr::bind_rows(
      rdv_out,
      rdv %>%
        # Eliminate any patients who don't exist in the cohort
        dplyr::filter(project_id %in% df_patient_list$project_id) %>%
        # Add the cohort entry and exit dates to the rows
        dplyr::left_join(df_patient_list, by = "project_id") %>%
        # Eliminate any non-concurrent events
        ff_concurrent() %>%
        # Crop the events to fit within the cohort entry/exit dates
        ff_crop_start_end() %>%
        # Rename the project IDs to be unique and add the cohort label
        dplyr::mutate(
          # project_id = paste0("CT-", stringr::str_pad(dplyr::row_number(), 8, pad = "0")),
          cohort = as.factor(names(cohorts)[[idx]])
        ) %>%
        # Rename the cohort dates for ease of use
        dplyr::rename(cohort_entry_date = entry_date, cohort_exit_date = exit_date)
    )

  }



  rdv_out
}


str_regex_closure <- function(regex, col) {
  r <- regex
  c <- col
  function(x) dplyr::filter(x, stringr::str_detect(!!as.name(c), r))
}
val_between_closure <- function(rangev, col) {
  d1 <- rangev[[1]]
  d2 <- rangev[[2]]
  c <- col
  function(x) dplyr::filter(x, (!!as.name(c) >= d1 & !!as.name(c) <= d2))
}
age_between_closure <- function(rangev, col) {
  start_age <- rangev[[1]]
  end_age <- rangev[[2]]

  function(x) {
    x %>%
      # Add start and end datetimes to this RDV based on when they would be
      # at the required age
      dplyr::mutate(
        start_datetime = lubridate::as_datetime(
          lubridate::add_with_rollback(
            birth_date,
            dplyr::if_else(
              is.numeric(start_age),
              lubridate::years(start_age),
              lubridate::period(start_age)
            )
          )
        ),
        end_datetime = lubridate::as_datetime(
          lubridate::add_with_rollback(
            birth_date,
            dplyr::if_else(
              is.numeric(end_age),
              lubridate::years(end_age),
              lubridate::period(end_age)
            )
          )
        )
      ) %>%
      ff_concurrent()
  }
}


merge_contiguous <- function(x, y) {

  n <- nrow(x)
  if (n == 1) return(x)

  for (idx1 in 1:(n-1)) {

    # Jump to next if already removed this one
    if (x[[idx1,"remove"]]) next

    for (idx2 in (idx1+1):n) {

      # Jump to next if already removed this one
      if (x[[idx2,"remove"]]) next

      # Merge if overlapped
      if (
        x[[idx1,"entry_date"]] <= x[[idx2,"exit_date"]] &&
        x[[idx1,"exit_date"]] >= x[[idx2,"entry_date"]]
      ) {
        x[[idx2,"remove"]] <- TRUE
        x[[idx1,"entry_date"]] <- min(x[[idx1,"entry_date"]], x[[idx2,"entry_date"]])
        x[[idx1,"exit_date"]] <- max(x[[idx1,"exit_date"]], x[[idx2,"exit_date"]])
      }

    }
  }

  x
}


# Filter any events that are not within the existing cohort entry/exit window
ff_concurrent <- function(x) {
  # Only works if RDV supports start and end datetimes
  if (!"start_datetime" %in% names(x) | !"end_datetime" %in% names(x)) return(x)

  # Filter if event start is before the cohort exit and end is after cohort
  # Special case if no end_datetime (end_datetime == NA)
  # i.e. admission/event ongoing at point of extraction
  dplyr::filter(
    x, start_datetime <= exit_date,
    end_datetime >= entry_date | is.na(end_datetime)
  )
}


# Crop cohort window to overlap with the event
ff_crop_entry_exit_closure <- function(cropwindow = NULL) {

  if (is.null(cropwindow)) cropwindow <- c(0,0)
  w1 <- lubridate::days(cropwindow[[1]])
  w2 <- lubridate::days(cropwindow[[2]])

  function(x) {
    # Only works if RDV supports start and end datetimes
    if (!"start_datetime" %in% names(x) | !"end_datetime" %in% names(x)) return(x)
    dplyr::mutate(
      x,
      entry_date = pmax(entry_date, start_datetime + w1, na.rm = TRUE),
      exit_date = pmin(exit_date, end_datetime + w2, na.rm = TRUE)
    )
  }
}


# Crop cohort entry to the start of the event
ff_crop_entry_closure <- function(cropwindow = NULL) {

  if (is.null(cropwindow)) cropwindow <- c(0,0)
  w1 <- lubridate::days(cropwindow[[1]])

  function(x) {
    # Only works if RDV supports start datetimes
    if (!"start_datetime" %in% names(x)) return(x)
    dplyr::mutate(x, entry_date = pmax(entry_date, start_datetime + w1, na.rm = TRUE))
  }
}


# Crop cohort entry to the first event
ff_crop_first_closure <- function(cropwindow = NULL) {

  if (is.null(cropwindow)) cropwindow <- c(0,0)
  w1 <- lubridate::days(cropwindow[[1]])
  w2 <- lubridate::days(cropwindow[[2]])

  function(x) {
    # Only works if RDV supports start datetimes
    if (!"start_datetime" %in% names(x)) return(x)

    x %>%
      dplyr::group_by(project_id) %>%
      dplyr::mutate(
        entry_date = pmax(entry_date, min(start_datetime) + w1, na.rm = TRUE),
        exit_date = pmin(exit_date, min(start_datetime) + w2, na.rm = TRUE)
      )
  }
}


# Crop and event to only the section within the cohort entry/exit window
ff_crop_start_end <- function(x) {
  # Only works if RDV supports start and end datetimes
  if (!"start_datetime" %in% names(x) | !"end_datetime" %in% names(x)) return(x)

  dplyr::mutate(
    x,
    start_datetime = pmax(entry_date, start_datetime),
    end_datetime = pmin(exit_date, end_datetime)
  )
}
