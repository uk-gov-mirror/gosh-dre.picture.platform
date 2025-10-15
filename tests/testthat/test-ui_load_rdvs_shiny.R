test_parquet_rdvs <- tempdir()
gen_test_folder(test_parquet_rdvs)

test_that("load rdv shiny server", {
  app_yaml <- list()
  app_yaml[["amount_of_data_to_load"]] <- "all"
  rdv_reactive <- ui_load_rdvs_server(data_dir=test_parquet_rdvs, show_progress = FALSE, app_yaml=app_yaml)
  expect_s3_class(rdv_reactive, "reactiveExpr")

  rdvs <- shiny::isolate(rdv_reactive())
  expect_equal(length(rdvs), 3)
  expect_equal(nrow(rdvs[[1]]), 200)
  # TODO more checks the imported RDVs are full and correct

})


test_that("load dataset description", {

  data_dir <- system.file("parquet", package = "dummyData")
  desc <- load_dataset_description(test_parquet_rdvs, "dmv")
  # TODO update this once we get good dataset descriptions
  expect_equal(desc$name, "Dmv")
  expect_equal(desc$description, "Dmv")
  expect_equal(desc$min_activity_date, "Unknown")
  expect_equal(desc$max_activity_date, "Unknown")
  expect_equal(desc$extraction_date, "Unknown")

})


test_that("safe posix dates", {

  test_date <- as.Date("2000-1-1")
  posix_date <- safe.as.POSIXct(test_date)

  expect_s3_class(posix_date, "POSIXct")
  expect_true(test_date == posix_date)

})
