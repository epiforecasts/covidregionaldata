test_that(
  "Test all county R6 objects contain required fields",
  {
    check_country_fields <- function(regionClass) {
      expected_public_fields <- names(CountryTemplate$public_fields)
      region_public_fields <- names(regionClass$public_fields)
      for (field in expected_public_fields) {
        if (!(field %in% region_public_fields)) {
          stop(
            paste0(
              field, " required but missing in '", regionClass$classname, "'"
            )
          )
        }
      }
    }

    check_all_country_fields <- function() {
      avalibale_country_data <- get_available_datasets()
      available_sources <- avalibale_country_data$country
      for (country in available_sources) {
        regionClass <- get(country)
        check_country_fields(regionClass)
      }
      return(0)
    }
  }
)

test_that(
  "Test all county R6 objects contain required fields",
  {
    check_country_methods <- function(regionClass) {
      expected_public_methods <- names(CountryTemplate$public_methods)
      region_public_methods <- names(regionClass$public_methods)
      for (method in expected_public_methods) {
        if (!(method %in% region_public_methods)) {
          stop(paste0("function: ", method, "() required but missing in '", regionClass$classname, "'"))
        }
      }
    }

    check_all_country_methods <- function() {
      avalibale_country_data <- get_available_datasets()
      available_sources <- avalibale_country_data$country
      for (country in available_sources) {
        regionClass <- get(country)
        check_country_methods(regionClass)
      }
      return(0)
    }
  }
)
