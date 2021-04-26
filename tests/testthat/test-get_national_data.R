source("functions/test_initialise.R")
test_get_national_data <- function(source) {
  test_that(paste0("get_national_data returns ", source, " data"), {
    national <- readRDS(paste0("custom_data/", source, ".rds"))
    true <- national$return()
    true_R6 <- national$clone()
    true_R6$steps <- TRUE
    true_steps <- true_R6$return()
    mockery::stub(
      get_national_data, "initialise_dataclass",
      test_initialise(class = national)
    )
    d <- get_national_data(
      countries = "Zimbabwe", source = source,
      verbose = FALSE
    )
    p <- get_national_data(
      countries = "zimbabwe", source = source,
      verbose = FALSE
    )
    expect_s3_class(d, "data.frame")
    expect_true(all(d$country == "Zimbabwe"))
    expect_true(sum(as.numeric(d$cases_new) < 0, na.rm = TRUE) == 0)
    expect_equal(d, p)
    expect_error(get_national_data(
      countries = "rwfwf", source = source,
      verbose = FALSE
    ))
    expect_warning(get_national_data(
      country = "Zimbabwe", source = source,
      verbose = FALSE
    ))
    expect_equal(
      true_steps,
      get_national_data(source = source, steps = TRUE, verbose = FALSE)
    )
    expect_equal(
      true_R6,
      get_national_data(
        source = source, steps = TRUE, class = TRUE,
        verbose = FALSE
      )
    )
    totals <- get_national_data(totals = TRUE, verbose = FALSE)
    expect_s3_class(totals, "data.frame")
    expect_true(nrow(totals) > 0)
    expect_true(sum(grepl("_total", colnames(totals))) > 4)
    expect_true(all(!grepl("date", colnames(totals))))
  })
}

test_get_national_data("ecdc")
test_get_national_data("who")
