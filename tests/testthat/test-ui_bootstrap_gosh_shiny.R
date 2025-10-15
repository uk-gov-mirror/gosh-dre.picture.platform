test_that("create ui", {
  testthat::skip(message = "Issue with hash test")
  html <- ui_bootstrap_gosh("logo_xl", "xs", NULL, NULL)
  expect_known_hash(html, "16683bcff2")

})
