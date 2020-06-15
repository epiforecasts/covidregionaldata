#' Fetch daily COVID cases by state for United States of America
#' @description Fetches daily COVID cases county collated by the
#' New York Times
#' Data is available at https://github.com/nytimes/covid-19-data
#' @return A data.frame of COVID cases by region in the US
#' @importFrom dplyr rename mutate
get_us_regional_cases_with_level_2 <- function(){
  
  url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

  data <- csv_reader(url) %>% 
      dplyr::rename(region_level_2 = county,
                    region_level_1 = state,
                    level_2_region_code = fips,
                    cases_total = cases,
                    deaths_total = deaths) %>% 
      dplyr::mutate(cases_total = replace(cases_total, cases_total < 0 , 0),
                    deaths_total = replace(deaths_total, deaths_total < 0 , 0))

  return(data)
}


#' Fetch daily COVID cases by state for United States of America
#' @description Fetches daily COVID cases county collated by the
#' New York Times
#' Data is available at https://github.com/nytimes/covid-19-data
#' @return A data.frame of COVID cases by region in the US
#' @importFrom dplyr rename mutate
get_us_regional_cases_only_level_1 <- function(){

  url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
  
  data <- csv_reader(url) %>% 
    dplyr::rename(region_level_1 = state,
                  cases_total = cases,
                  deaths_total = deaths) %>% 
    dplyr::mutate(cases_total = replace(cases_total, cases_total < 0 , 0),
                  deaths_total = replace(deaths_total, deaths_total < 0 , 0)) %>%
    dplyr::select(date, region_level_1, cases_total, deaths_total)
  
  return(data)
}
