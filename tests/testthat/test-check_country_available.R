test_that(
  "Test error is thrown for numeric country",
  {
    expect_error(
      check_country_available(
        country = 1, level = "1",
        totals = FALSE, localise = TRUE,
        verbose = FALSE, steps = FALSE
      ),
      "is.character(country) is not TRUE",
      fixed = TRUE
    )
  }
)
test_that(
  "Test error is thrown for numeric level",
  {
    expect_error(
      check_country_available(
        country = "1", level = 1,
        totals = FALSE, localise = TRUE,
        verbose = FALSE, steps = FALSE
      ),
      "is.character(level) is not TRUE",
      fixed = TRUE
    )
  }
)
test_that(
  "Test error is thrown for unknown country/source",
  {
    expect_error(
      check_country_available(
        country = "amadeupcountry", level = "1",
        totals = FALSE, localise = TRUE,
        verbose = FALSE, steps = FALSE
      ),
      "No data avaliable for country' amadeupcountry '.",
      fixed = TRUE
    )
  }
)
test_that(
  "Test error is thrown when level not avaliable for data",
  {
    expect_error(
      check_country_available(
        country = "Italy", level = "2",
        totals = FALSE, localise = TRUE,
        verbose = FALSE, steps = FALSE
      ),
      "Target spatial level not supported in the selected country.
               use get_available_datasets() to see supported options",
      fixed = TRUE
    )
  }
)
test_that(
  "Test returns an R6 object",
  {
    region_class <- check_country_available(
      country = "WHO", level = "1",
      totals = FALSE, localise = TRUE,
      verbose = FALSE, steps = FALSE
    )
    to_check <- class(region_class)
    expect_equal(to_check[length(to_check)], "R6")
  }
)
