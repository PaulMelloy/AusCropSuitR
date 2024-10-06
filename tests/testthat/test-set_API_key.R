test_that("silo api is saved to system and .Renvion", {

  test_email <- "test@melloy.com.au"

  set_API_key(test_email)
  expect_equal(Sys.getenv("SILO_API_KEY"),expected = test_email)
  renv_con <- file(".Renviron", open = "r")
  renv_lines <- readLines(renv_con)
  close(renv_con)
  expect_true(any(grepl("SILO_API_KEY=",
                        renv_lines)))


})
