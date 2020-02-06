context("get_linelist")





test_that("Can retrieve the linelist without errors", {
  data <- get_linelist()

  expect_true(!is.null(data))
})

