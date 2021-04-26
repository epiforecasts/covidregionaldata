test_that("CountryDataClass specific filter method works correctly", {
  C <- R6::R6Class("C",
    inherit = CountryDataClass,
    public = list(
      common_data_urls = list(
        main = "custom_data/ecdc.csv"
      ),
      clean_common = function() {
        self$data$clean <- self$data$raw$main
      }
    )
  )
  c <- C$new(verbose = FALSE)
  suppressMessages(c$get())
  expect_error(c$filter("madeupland"))
  expect_error(c$filter("zimbabwe"), NA)
  expect_error(c$filter("Zimbabwe"), NA)
})
