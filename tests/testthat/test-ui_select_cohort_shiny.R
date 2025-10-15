make_test_cohort <- function(){
  test_list <- list()
  key1 <- "test1"
  value1 <- 1
  key2 <- "test2"
  value2 <- 2
  test_list[[ key1 ]] <- value1
  test_list[[ key2 ]] <- value2
  return(test_list)
}

test_cohort <- make_test_cohort()

test_that("rename cohort", {
  input <- list()
  input[["current_cohorts"]] <- "test1"
  input[["cohort_modal_name"]] <- "hello"
  test_cohort <- rename_cohort(test_cohort, input)
  expect_equal(names(test_cohort), c("test2", "hello"))
})

test_that("rename cohort with empty cohort name", {
  input <- list()
  input[["current_cohorts"]] <- "test1"
  input[["cohort_modal_name"]] <- ""
  test_cohort <- rename_cohort(test_cohort, input, "test")
  expect_equal(names(test_cohort), c("test1", "test2"))
})

test_that("delete cohort", {
  input <- list()
  input[["current_cohorts"]] <- "test1"
  test_cohort <- delete_cohort(test_cohort, input)
  expect_equal(names(test_cohort), "test2")
})
