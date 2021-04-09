test_get_national_data <- function(source) {
  test_that(paste0("get_national_data returns ", source, " data"), {
    national <- readRDS(paste0("custom_data/", source, ".rds"))
    true <- national$return()
    true_R6 <- national$clone()
    true_R6$steps <- TRUE
    true_steps <- true_R6$return()
    mockery::stub(
      get_national_data, "check_country_available",
      function(country, level, totals, localise,
               verbose, steps, regions) {
        class <- national$clone()
        class$verbose <- verbose
        class$steps <- steps
        if (!missing(regions)) {
          class$target_regions <- regions
        }
        return(class)
      }
    )
    d <- get_national_data(
      countries = "Zambia", source = source,
      verbose = FALSE
    )
    p <- get_national_data(
      countries = "zambia", source = source,
      verbose = FALSE
    )
    expect_s3_class(d, "data.frame")
    expect_true(all(d$country == "Zambia"))
    expect_true(sum(as.numeric(d$cases_new) < 0, na.rm = TRUE) == 0)
    expect_equal(d, p)
    expect_error(get_national_data(
      countries = "rwfwf", source = source,
      verbose = FALSE
    ))
    expect_warning(get_national_data(
      country = "Zambia", source = source,
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
  })
}

test_get_national_data("ecdc")
test_get_national_data("who")
