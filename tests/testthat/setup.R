# testthat setup file, run before any tests

# Latex preamble file
tex_preamble <- system.file("rmd/templates", "preamble.tex", package = "picture.platform")


# Load the example RDVs
df_pde <- dummyData::dmv_caboodle_patient_demographics
df_dia <- dummyData::dmv_caboodle_patient_diagnoses
df_pro <- dummyData::dmv_caboodle_patient_procedures
df_had <- dummyData::dmv_caboodle_patient_hospital_admissions
df_med_admins <- dummyData::dmv_caboodle_patient_medication_admins
df_med_orders <- dummyData::dmv_caboodle_patient_medication_orders
df_lab_main <- dummyData::dmv_caboodle_patient_selected_lab_components_main
df_flw_main <- dummyData::dmv_caboodle_patient_selected_flowsheetrows_main
df_wst <- dummyData::dmv_caboodle_patient_ward_stays

# Cohort definition (Female vs Male)
cohort_spec <- list(
  A = list(df_spec = structure(list(
    id = c(1, 2),
    type = c("base", "filter"),
    rdv = c(NA, "pde"),
    val = list("cardiology", "Female"),
    inclusion = c(NA, "after_first"),
    query_type = c(NA, "str_matches")),
    class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -2L))),
  B = list(df_spec = structure(list(
    id = c(1, 2),
    type = c("base", "filter"),
    rdv = c(NA, "pde"),
    val = list("cardiology", "Male"),
    inclusion = c(NA, "after_first"),
    column = c(NA, "sex_name"),
    query_type = c(NA, "str_matches")),
    class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -2L)))
)

# Arbitrary test cohorts based on sex
df_pde <- df_pde %>%
  dplyr::mutate(
    cohort = ifelse(sex_name == "Female", "A", "B"),
    cohort_id = project_id,
    cohort_entry_date = lubridate::as_datetime(birth_date, tz = ""),
    cohort_exit_date = lubridate::as_datetime(dplyr::if_else(is.na(death_date), Sys.Date(), death_date), tz = "")
  )
df_dia <- df_dia %>%
  dplyr::left_join(
    dplyr::select(df_pde, project_id, tidyselect::starts_with("cohort")),
    by = "project_id"
  )
df_pro <- df_pro %>%
  dplyr::left_join(
    dplyr::select(df_pde, project_id, tidyselect::starts_with("cohort")),
    by = "project_id"
  )
df_had <- df_had %>%
  dplyr::left_join(
    dplyr::select(df_pde, project_id, tidyselect::starts_with("cohort")),
    by = "project_id"
  )
df_med_admins <- df_med_admins %>%
  dplyr::left_join(
    dplyr::select(df_pde, project_id, tidyselect::starts_with("cohort")),
    by = "project_id"
  )
df_med_orders <- df_med_orders %>%
  dplyr::left_join(
    dplyr::select(df_pde, project_id, tidyselect::starts_with("cohort")),
    by = "project_id"
  )
df_lab_main <- df_lab_main %>%
  dplyr::left_join(
    dplyr::select(df_pde, project_id, tidyselect::starts_with("cohort")),
    by = "project_id"
  )
df_flw_main <- df_flw_main %>%
  dplyr::left_join(
    dplyr::select(df_pde, project_id, tidyselect::starts_with("cohort")),
    by = "project_id"
  )
df_wst <- df_wst %>%
  dplyr::left_join(
    dplyr::select(df_pde, project_id, tidyselect::starts_with("cohort")),
    by = "project_id"
  )

# Derived RDVs
df_dia_conditions <- df_dia %>%
  dplyr::filter(stringr::str_detect(diag_local_code, "^[A-T]"))
df_dia_other <- df_dia %>%
  dplyr::filter(stringr::str_detect(diag_local_code, "^[U-Z]"))
df_pro_opcs <- df_pro %>%
  dplyr::filter(stringr::str_detect(proc_local_code, "^[A-Z][0-9]"))

df_med <- df_med_admins %>%
  dplyr::left_join(df_med_orders, by = c("project_id", "drug_code", "medication_order_id"), suffix = c("", ".ord"))


gen_test_folder <- function(temp_dir){
  df_pde_filename <- file.path(temp_dir, "dmv_caboodle_patient_demographics.parquet")
  arrow::write_parquet(df_pde, df_pde_filename)
  df_dia_filename <- file.path(temp_dir, "dmv_caboodle_patient_diagnoses.parquet")
  arrow::write_parquet(df_dia, df_dia_filename)
  df_dia_filename_wrong <- file.path(temp_dir, "wrong_file_name.parquet")
  arrow::write_parquet(df_dia, df_dia_filename_wrong)
  df_dia_filename_wrong <- file.path(temp_dir, "dmv_caboodle_group_diagnoses.parquet")
  arrow::write_parquet(df_dia, df_dia_filename_wrong)
}
