# test_yaml
test_yaml <- testthat::test_path("data", "test.yaml")

# Lookup tables
table_sources <- c("caboodle")
fn_rdv_lookup <- system.file("configs", "rdv_code_lookup.csv",
                             package = "picture.platform")
df_rdv_lookup <- readr::read_csv(fn_rdv_lookup, show_col_types = FALSE)

test_that("get RDVs in function", {
  #1 make a temp folder for dummy data
  test_parquet_rdvs <- tempdir()
  #2 save as parguet files
  gen_test_folder(test_parquet_rdvs)
  rdvs <- picture.platform:::get_rdvs_in(test_parquet_rdvs)
  expect_equal(nrow(rdvs), 2)
  expect_true(all(rdvs$group %in% "dmv"))
  expect_true(all(rdvs$source %in% "caboodle"))
  expect_true(all(stringr::str_starts(rdvs$type, "patient_")))
})

test_that("Test can get columns from yaml",{
  # Custom handlers for input lists dates
  datelist_hdl <- function(x) lapply(x, lubridate::as_datetime)
  # Load the app config yaml
  app_config <- yaml::read_yaml(test_yaml, handlers = list(datelist = datelist_hdl))
  comb <- picture.platform::get_rdv_cols(app_config)
  expected_demographic_columns <- c("project_id", "sex_name", "ethnicity_name",
                "birth_date", "death_date",  "ethnicity_nat_code",
                "deceased_flag")
  expected_list_names <- c("patient_diagnoses", "patient_hospital_admissions",
                           "patient_demographics")
  expect_equal(c(names(comb)), expected_list_names)
  expect_equal(comb$patient_demographics, expected_demographic_columns)
})

test_that("Test rdv_lst_names_to_labels based on yaml",{
  datelist_hdl <- function(x) lapply(x, lubridate::as_datetime)
  app_config <- yaml::read_yaml(test_yaml, handlers = list(datelist = datelist_hdl))
  initial_config <- rdvs_from_initial_config(app_config)
  col_lst <- get_cols_from_analysis(app_config)
  initial_config <- rdv_lst_names_to_labels(df_rdv_lookup, initial_config)
  expected <- c("project_id", "sex_name", "ethnicity_name",
                "birth_date", "death_date",  "ethnicity_nat_code",
                "deceased_flag")
  expect_equal(col_lst$pde, expected)
})

test_that("Test rdv_lst_names_to_labels based on yaml empty
          ",{
  datelist_hdl <- function(x) lapply(x, lubridate::as_datetime)
  app_config <- yaml::read_yaml(test_yaml, handlers = list(datelist = datelist_hdl))
  initial_config <- list()
  col_lst <- get_cols_from_analysis(app_config)
  initial_config <- rdv_lst_names_to_labels(df_rdv_lookup, list())
  expect_equal(initial_config, list())
})

test_that("Test rdv_lst_names_to_labels based on empty df_rdv_lookup",{
  datelist_hdl <- function(x) lapply(x, lubridate::as_datetime)
  app_config <- yaml::read_yaml(test_yaml, handlers = list(datelist = datelist_hdl))
  initial_config <- rdvs_from_initial_config(app_config)
  col_lst <- get_cols_from_analysis(app_config)
  initial_config <- rdv_lst_names_to_labels(list(), initial_config)
  expected <- c("project_id", "sex_name", "ethnicity_name",
                "birth_date", "death_date",  "ethnicity_nat_code",
                "deceased_flag")
  expect_equal(c(names(initial_config)), c("had", "dia_conditions", "microblgy_pos"))
})
