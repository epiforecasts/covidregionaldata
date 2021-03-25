test_get_national_data <- function(source) {
  test_that(paste0("get_national_data returns", source, " data"), {
    true <- readRDS(paste0("custom_data/", source, ".rds"))
    stub_download <- function() {
      pf <- parent.frame()
      pf$nation_class$region$raw <- true$raw
    }
    mockery::stub(
      get_national_data,
      "nation_class$download",
      stub_download
    )
    d <- get_national_data(
      country = "Afghanistan", source = source,
      verbose = FALSE
    )
    print(d)

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
