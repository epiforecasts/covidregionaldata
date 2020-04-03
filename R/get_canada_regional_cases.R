#' Fetch daily COVID cases by province for Canada
#' @description Fetches daily COVID cases, and deaths by province collated by Provincial Canadian Health Authorities
#' Data is available at https://www.canada.ca/en/public-health/services/diseases/2019-novel-coronavirus-infection.html
#' @param out Character String specifying output data format "timeseries", "total", default: "timeseries".
#' @return A data.frame of COVID cases by province in Canada
#' @export
#' @importFrom dplyr filter select summarise arrange group_by
#' @importFrom tidyr replace_na
#' @importFrom readr read_csv
#' @examples
#'
#' \dontrun{
#'
#'  get_canada_regional_cases(out = 'timeseries')
#'
#' }

get_canada_regional_cases <- function(out = 'timeseries'){
  
  if(!out %in% c('timeseries', 'total')){
    stop('Unknown input. Please provide output format: "timeseries", "total". default = "timeseries"')
  }
  
  url <- "https://health-infobase.canada.ca/src/data/covidLive/covid19.csv"
  
  get_cases_ts <- function(url){
    
    data <- readr::read_csv(url) %>% 
      dplyr::select(-percentoday, -numtotal, -numconf, -numtested) %>% 
      dplyr::filter(pruid != 1) %>% 
      mutate(prname = gsub("Repatriated travellers", "Repatriated Travellers", prname))
    
    data$date <- as.Date(data$date, format = '%d-%m-%Y')
    
    colnames(data) <- c("pruid", "province", "province_fr", "date", "cases_probable", "deaths", "cases_confirmed")
    
    return(data)
    
  }
  
  get_cases_tot <- function(url){
    
    data <- readr::read_csv(url) %>% 
      dplyr::select(-percentoday, -numtotal, -numconf, -numtested) %>% 
      dplyr::filter(pruid != 1) %>% 
      dplyr::mutate(prname = gsub("Repatriated travellers", "Repatriated Travellers", prname)) %>% 
      dplyr::group_by(pruid) %>% 
      dplyr::summarise(prname = unique(prname), 
                prnameFR = unique(prnameFR),
                numprob = sum(numprob),
                numdeaths = sum(numdeaths),
                numtoday = sum(numtoday)) %>% 
      dplyr::arrange(-numtoday)
    
    colnames(data) <- c("pruid", "province", "province_fr", "cases_probable", "deaths", "cases_confirmed")
    
    return(data)
    
  }
  
  if (out == "timeseries"){
    
    return(get_cases_ts(url))
    
  } else if (out == "total"){
    
    return(get_cases_tot(url))
    
  }
  
}

