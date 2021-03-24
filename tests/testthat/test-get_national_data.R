test_get_national_data <- function(source) {
  test_that(paste0("get_national_data returns", source, " data"), {
    true <- readRDS(paste0("custom_data/", source, ".rds"))
    # initiate class
    test_class <- get(toupper(source))$new()
    # set raw to mock data
    test_class$region$raw <- true$raw
    # stub the download function for a return of the mock data
    mockery::stub(test_class$download, "download", "stub has been called!")
    expect_equal(test_class$download, "stub has been called!")
    mockery::stub(test_class$download, "download", true$raw)
    # check
    d <- get_national_data(
      country = "Afghanistan", source = source,
      verbose = FALSE
    )
    expect_s3_class(d, "data.frame")
    expect_true(all(d$country == "Afghanistan"))
    expect_true(sum(as.numeric(d$cases_new) < 0, na.rm = TRUE) == 0)
    expect_error(get_national_data(
      country = "rwfwf", source = source,
      verbose = FALSE
    ))
    expect_equal(
      true,
      get_national_data(source = source, steps = TRUE, verbose = FALSE)
    )
  })
}

# testthat::skip("Test in development")
test_get_national_data("who")
# test_get_national_data("ecdc")
