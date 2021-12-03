test_path <- "custom_data/mtcars.csv"
target <- dplyr::as_tibble(head(mtcars))

test_that("csv_reader can read in a simple dataset", {
  test <- csv_reader(test_path)
  expect_s3_class(test, "tbl_df")
  expect_equal(
    colnames(test),
    colnames(target)
  )
  attributes(test)$spec <- NULL
  attributes(test)$problems <- NULL
  expect_equal(
    test,
    target
  )
})

test_that("csv_reader verbosity is controlled as expected", {
  expect_gte(
    length(capture.output(tmp <- csv_reader(test_path, verbose = TRUE),
      type = "message"
    )),
    1
  )
  expect_equal(
    length(capture.output(tmp <- csv_reader(test_path, verbose = FALSE),
      type = "message"
    )),
    0
  )
})

test_that("csv_reader fails as expected when given a file that doesn't exist", {
  expect_error(csv_reader("nonsense.csv", verbose = FALSE))
})
