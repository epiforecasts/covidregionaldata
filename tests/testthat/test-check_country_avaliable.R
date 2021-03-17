test_that(
  "Test error is thrown for numeric country",
  {
    expect_error(
      check_country_avaliable(
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
      check_country_avaliable(
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
      check_country_avaliable(
        country = "amadeupcountry", level = "1",
        totals = FALSE, localise = TRUE,
        verbose = FALSE, steps = FALSE
      ),
      "No data avaliable for country' Amadeupcountry '.",
      fixed = TRUE
    )
  }
)
test_that(
  "Test error is thrown when level not avaliable for data",
  {
    expect_error(
      check_country_avaliable(
        country = "italy", level = "2",
        totals = FALSE, localise = TRUE,
        verbose = FALSE, steps = FALSE
      ),
      "Target spatial level not supported in the selected country.
               See available_datasets for supported options",
      fixed = TRUE
    )
  }
)
test_that(
  "Test returns an R6 object",
  {
    region_class <- check_country_avaliable(
      country = "who", level = "1",
      totals = FALSE, localise = TRUE,
      verbose = FALSE, steps = FALSE
    )
    to_check <- class(region_class)
    expect_equal(to_check[length(to_check)], "R6")
  }
)
