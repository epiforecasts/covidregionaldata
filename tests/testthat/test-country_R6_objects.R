test_that(
  "Test all county R6 objects contain required fields",
  {
    check_country_fields <- function(regionClass){
      expected_public_fields <- names(NameOfCountry$public_fields)
      region_public_fields <- names(regionClass$public_fields)
      for (field in expected_public_fields) {
        if (!(field %in% region_public_fields)){
          stop(paste0(field, " required but missing in '", regionClass$classname, "'"))
        }
      }
    }
    
    check_all_country_fields <- function(){
      avaliable_sources = covidregionaldata::region_codes$country
      for (country in avaliable_sources){
        country <- paste0(
          toupper(substr(country, 1, 1)),
          tolower(substr(country, 2, nchar(country)))
        )
        regionClass = get(country)
        check_country_fields(regionClass)
      }
      return(0)
    }
    expect_equal(check_all_country_fields(), 0)
  }
)

test_that(
  "Test all county R6 objects contain required fields",
  {
    check_country_methods <- function(regionClass){
      expected_public_methods <- names(NameOfCountry$public_methods)
      region_public_methods <- names(regionClass$public_methods)
      for (method in expected_public_methods) {
        if (!(method %in% region_public_methods)){
          stop(paste0("function: " , method, "() required but missing in '", regionClass$classname, "'"))
        }
      }
    }
    
    check_all_country_methods <- function(){
      avaliable_sources = covidregionaldata::region_codes$country
      for (country in avaliable_sources){
        country <- paste0(
          toupper(substr(country, 1, 1)),
          tolower(substr(country, 2, nchar(country)))
        )
        regionClass = get(country)
        check_country_methods(regionClass)
      }
      return(0)
    }
    expect_equal(check_all_country_methods(), 0)
  }
)
