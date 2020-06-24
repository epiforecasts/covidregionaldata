#' US Regional Daily COVID-19 Count Data - States
#' 
#' @description Extracts daily COVID-19 data for the USA, stratified by state.
#' Data available at \url{https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv}. 
#' It is loaded and then sanitised.
#' @return A data frame of daily COVID cases for the US by state, to be further processed by \code{get_regional_data()}.
#' @importFrom dplyr rename mutate select %>%
#' 
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


#' US Regional Daily COVID-19 Count Data - Counties
#' 
#' @description Extracts daily COVID-19 data for the USA, stratified by county.
#' Data available at \url{https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv}. 
#' It is loaded and then sanitised.
#' @return A data frame of daily COVID cases for the US by county, to be further processed by \code{get_regional_data()}.
#' @importFrom dplyr rename mutate %>%
#' 
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