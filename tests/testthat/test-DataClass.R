test_that("DataClass can be initialised", {
  expect_error(DataClass$new(), NA)
})

test_that("DataClass methods cannot be called when the previous method have not
           been called", {
  a <- DataClass$new()
  expect_error(a$get())
  expect_error(a$download())
  expect_error(a$clean())
  expect_error(a$filter())
  expect_error(a$process())
  expect_error(a$return())
  a$steps <- TRUE
  expect_error(a$return())
})

D <- R6::R6Class("D",
  inherit = DataClass,
  public = list(
    common_data_urls = list(
      main = "custom_data/ecdc.csv"
    ),
    clean_common = function() {
      self$data$clean <- self$data$raw$main
    }
  )
)
d <- D$new(verbose = FALSE)

test_that("An inherited DataClass class can be intialised", {
  expect_true(identical(class(d), c("D", "DataClass", "R6")))
})

test_that("DataClass can download data", {
  expect_error(d$download(), NA)
  expect_s3_class(d$data$raw$main, "data.frame")
  expect_equal(nrow(d$data$raw$main), 250)
})
d$download()

test_that("DataClass can clean data", {
  expect_error(d$clean(), NA)
  expect_s3_class(d$data$clean, "data.frame")
  expect_equal(d$data$clean, d$data$raw$main)
})
d$clean()

test_that("DataClass can filter data", {
  expect_error(d$filter("MadeUpLand"))
  expect_error(d$filter("Zimbabwe"), NA)
  expect_s3_class(d$data$clean, "data.frame")
  expect_equal(unique(d$data$clean$level_1_region), "Zimbabwe")
  expect_true(nrow(d$data$clean) != 0)
})
d$filter()

test_that("DataClass can process data", {
  expect_error(suppressMessages(d$process()), NA)
  expect_s3_class(d$data$processed, "data.frame")
  expect_true(nrow(d$data$processed) != 0)
})
suppressMessages(d$process())

test_that("DataClass can return data", {
  expect_error(d$return(), NA)
  expect_s3_class(d$return(), "data.frame")
  d$steps <- TRUE
  expect_equal(names(d$return()), c("raw", "clean", "processed", "return"))
})

test_that("DataClass can use the get method", {
  expect_error(suppressMessages(d$get()), NA)
  d$steps <- TRUE
  expect_equal(names(d$return()), c("raw", "clean", "processed", "return"))
})

test_that("DataClass returns a summary", {
  expect_error(d$summary(), NA)
  sum <- d$summary()
  expect_s3_class(sum, "data.frame")
  expect_true(nrow(sum) == 1)
  expect_equal(
    names(sum),
    c(
      "origin", "class", "level_1_region", "level_2_region",
      "type", "data_urls", "source_data_cols"
    )
  )
  expect_true(sum$class[1] == "D")
})
