
#' Fetch daily COVID cases by state for United States of America
#' @description Fetches daily COVID cases by state or county collated by the
#' New York Times
#' Data is available at https://github.com/nytimes/covid-19-data
#' @param level Character String specifying admin level "state", "county", default: "state".
#' @param out Character String specifying output data format "timeseries", "total", default: "timeseries".
#' @return A data.frame of COVID cases by region in the US
#' @export
#' @importFrom memoise cache_filesystem memoise
#' @importFrom dplyr select summarise arrange group_by mutate ungroup 
#' @importFrom readr read_csv
#' @importFrom tidyr drop_na
#' @examples
#' get_us_regional_cases(level = 'state', out = 'timeseries')
#'
#' \dontrun{
#'
#'  regions <- rnaturalearth::ne_states("United States of America", returnclass = "sf")
#'
#'  cases <- get_us_regional_cases(out = 'total')
#'
#'  regions %>% dplyr::filter(name %in% cases$state) %>%
#'  ggplot2::ggplot() + ggplot2::geom_sf()
#'
#' }

get_us_regional_cases <- function(level = 'state', out = 'timeseries'){
  
  if(!out %in% c('timeseries', 'total')){
    stop('Unknown input. Please provide output format: "timeseries", "total". default = "timeseries"')
  }
  
  if(!level %in% c('state', 'county')){
    stop('Unknown input. Please provide admin level: "state", "county". default = "state"')
  }
  
  nyt_state_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
  nyt_county_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
  
  ch <- memoise::cache_filesystem(".cache")
  
  mem_read <- memoise::memoise(readr::read_csv, cache = ch)
  
  get_nyt_state <- function(path, out){
    
    cases <- mem_read(path) %>% 
      dplyr::arrange(date) %>% 
      dplyr::group_by(state) %>% 
      dplyr::mutate(cases = c(0, diff(cases)),
                    deaths = c(0, diff(deaths))) %>%
      dplyr::mutate(cases = replace(cases, cases < 0 , 0),
                    deaths = replace(deaths, deaths < 0 , 0)) %>% 
      dplyr::ungroup()
    
    
    if (out == 'total'){
      cases <- cases %>% 
        dplyr::select(-date) %>% 
        dplyr::group_by(state) %>% 
        dplyr::summarise(fips = unique(fips),
                         cases = sum(cases, na.rm = TRUE),
                         deaths = sum(deaths, na.rm = TRUE)) %>% 
        dplyr::arrange(-cases)
    }
    
    return(cases)
    
  }
  
  get_nyt_county <- function(path, out){
    
    cases <- mem_read(path) %>% 
      dplyr::arrange(date) %>% 
      dplyr::group_by(fips) %>% 
      tidyr::drop_na(fips) %>% 
      dplyr::mutate(cases = c(0, diff(cases)),
                    deaths = c(0, diff(deaths))) %>% 
      dplyr::mutate(cases = replace(cases, cases < 0 , 0),
                    deaths = replace(deaths, deaths < 0 , 0)) %>% 
      dplyr::ungroup()
    
    if (out == 'total'){
      cases <- cases %>%
        tidyr::drop_na() %>% 
        dplyr::select(-date) %>% 
        dplyr::group_by(fips) %>%
        dplyr::summarize(county = unique(county),
                         state = unique(state),
                         cases = sum(cases, na.rm = TRUE),
                         deaths = sum(deaths, na.rm = TRUE)) %>% 
        dplyr::arrange(-cases)
    }
    
    
    return(cases)
    
  }
  
  if (level == 'state'){
    
    return(get_nyt_state(nyt_state_url, out))
    
  }else if(level == 'county'){
    
    return(get_nyt_county(nyt_county_url, out))
    
  }
  
}
