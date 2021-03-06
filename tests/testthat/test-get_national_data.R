test_get_national_data <- function(source) {
  test_that(paste0("get_national_data returns", source, " data"), {
    true <- readRDS(paste0("custom_data/", source, ".rds"))
    raw <- true
    raw$clean <- NULL
    raw$processed <- NULL
    raw$return <- NULL
    mockery::stub(get_national_data, "download_regional", raw)
    d <- get_national_data(country = "Afghanistan", source = source,
                           verbose = FALSE)
    expect_s3_class(d, "data.frame")
    expect_true(all(d$country == "Afghanistan"))
    expect_true(sum(as.numeric(d$cases_new) < 0, na.rm = TRUE) == 0)
    expect_equal(
      true,
      get_national_data(source = source, steps = TRUE, verbose = FALSE)
      )
  })
}

test_get_national_data("ecdc")
test_get_national_data("who")
