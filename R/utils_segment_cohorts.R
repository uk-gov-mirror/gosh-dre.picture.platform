
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

    filter_by_cohorts(rdv_selected, cohort_defn())
  })
}


# Filter an RDV based on a cohort definition
filter_by_cohorts <- function(rdv, cohorts) {

  # If no cohort, return the empty data
  if (length(cohorts) == 0) {
    return(NULL)
  }


  rdv_out <- tibble::tibble()
  for (idx in seq(length(cohorts))) {

    cohort_idx <- cohorts[[idx]]
    df_init_patient_list <- cohort_idx$patient_list[[nrow(cohort_idx)]][[1]]

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


# Update/add patient lists to a cohort definition table
update_patient_lists <- function(cohort, df_rdvs) {
  # Check inputs
  if (is.null(df_rdvs$df_pde) | nrow(cohort) == 0)
    return(cohort)
  # Add patient list row if not already added
  if (!"patient_list" %in% names(cohort)) {
    cohort$patient_list <- NA
    cohort$n <- NA
    cohort$n_periods <- NA
  }

  # For each row of the cohort definition
  filter_functions <- c()
  for (idx in seq(nrow(cohort))) {

    spec <- cohort[idx,]

    if (is.na(spec$patient_list) || is.null(spec$patient_list[[1]])) {

      if (spec$type == "base") {

        # Base patient list
        patient_list <- df_rdvs$df_pde %>%
          dplyr::transmute(
            project_id,
            entry_date = lubridate::as_date(birth_date),  # as.Date('1-1-1'),
            exit_date = lubridate::as_date(dplyr::if_else(is.na(death_date), Sys.Date(), death_date))
          )

      } else if (spec$type == "filter" | spec$type == "and") {

        # Reset the filter list if a new filter
        if (spec$type == "filter") filter_functions <- c()

        # Val should be a list of length 1 - check this
        if (class(spec$val) != "list" || length(spec$val) != 1) {
          stop("Cohort formatting has been corrupted")
        }

        # Compute the new filter function
        rdv_name <- paste0("df_", stringr::str_to_lower(spec$rdv))
        col <- spec$column
        inclusion <- spec$inclusion
        cropwindow <- unlist(spec$window)
        query_type <- spec$query_type
        query_val <- spec$val[[1]]  # unlist kills date types
        patient_list <- cohort$patient_list[[idx-1]][[1]]

        if (stringr::str_starts(query_type, "str_")) {
          # Escape any brackets in the input query
          qv <- stringr::str_replace_all(query_val, c("\\(" = "\\\\(", "\\)" = "\\\\)"))
          #qv <- gsub(pattern = "([[:punct:]])", replacement="\\\\\\1", qv)
          # Build regex
          regex <- paste0("(", paste(qv, collapse = "|"), ")")
          if (query_type == "str_starts") regex <- paste0("^", regex)
          else if (query_type == "str_matches") regex <- paste0("^", regex, "$")
          else if (query_type == "str_contains") regex <- paste0("(?i)", regex)
          f <- pryr::unenclose(str_regex_closure(regex, col))
        } else if (query_type == "date_between") {
          f <- pryr::unenclose(val_between_closure(query_val, col))
        } else if (query_type == "age_between") {
          f <- pryr::unenclose(age_between_closure(query_val, col))
        } else if (query_type == "numeric_between") {
          f <- pryr::unenclose(val_between_closure(query_val, col))
        }
        filter_functions <- c(filter_functions, f)

        # Add functions to enforce the inclusion criteria type
        if(inclusion == "fully_concurrent")
          filter_functions <- c(filter_functions, ff_concurrent, ff_crop_entry_exit_closure(cropwindow))
        else if (inclusion == "after_first")
          filter_functions <- c(filter_functions, ff_concurrent, ff_crop_entry_closure(cropwindow))
        else if (inclusion == "on_first")
          filter_functions <- c(filter_functions, ff_concurrent, ff_crop_first_closure(cropwindow))
        # else if (inclusion == "ever_concurrent")
        #   filter_functions <- c(filter_functions, ff_concurrent)

        # Apply the filters in the RDV
        rdv_to_filter <- df_rdvs[[rdv_name]] %>%
          dplyr::right_join(patient_list, by = "project_id")
        for (ff in filter_functions) rdv_to_filter <- rdv_to_filter %>% ff()

        # Extract the new patient list
        new_patient_list <- rdv_to_filter %>%
          dplyr::select(project_id, entry_date, exit_date)

        # Store the updated patient list, unless "never"/exclusion criteria
        if (inclusion != "never") {
          patient_list <- new_patient_list
        } else {
          # Take the complement of the patient list
          patient_list <- patient_list %>%
            dplyr::filter(!project_id %in% unique(new_patient_list$project_id))
        }

        # Merge any periods that are contiguous or overlapping
        if (nrow(patient_list > 0)) {
          patient_list <- patient_list %>%
            dplyr::mutate(remove = FALSE) %>%
            dplyr::group_by(project_id) %>%
            dplyr::group_modify(merge_contiguous) %>%
            dplyr::ungroup() %>%
            dplyr::filter(!remove) %>%
            dplyr::select(!remove)
        }
      }

      # Store the patient list and n values
      cohort$patient_list[[idx]] <- list(patient_list)
      cohort$n[[idx]] <- dplyr::n_distinct(patient_list$project_id)
      cohort$n_periods[[idx]] <- nrow(patient_list)
    }
  }
  return(cohort)
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
