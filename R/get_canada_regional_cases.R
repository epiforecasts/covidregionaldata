#' Fetch daily COVID cases by province for Canada
#' @description Fetches daily COVID cases, and deaths by province collated by Provincial Canadian Health Authorities
#' Data is available at https://www.canada.ca/en/public-health/services/diseases/2019-novel-coronavirus-infection.html
#' @param out Character String specifying output data format "timeseries", "total", default: "timeseries".
#' @return A data.frame of COVID cases by province in Canada
#' @export
#' @importFrom dplyr filter select summarise arrange group_by
#' @importFrom tidyr replace_na
#' @importFrom readr read_csv
#' @importFrom lubridate dmy 
#' @examples
#'
#'\dontrun{
#'  total_cases = get_canada_regional_cases(out = "total")
#'  
#'  rnaturalearth::ne_states(country = "canada", returnclass="sf") %>% 
#'    dplyr::left_join(total_cases, by = c('name_en' = 'province')) %>% 
#'    ggplot2::ggplot() +
#'    ggplot2::theme_void() +
#'    ggplot2::geom_sf(aes(fill = cases_confirmed)) +
#'    ggplot2::labs(fill = 'Confirmed Cases')  
#'}

#update to get num tested and num recovered and add example

get_canada_regional_cases <- function(out = "timeseries"){
  
  if(!out %in% c("timeseries", "total")){
    stop('Unknown input. Please provide output format: "timeseries", "total". default = "timeseries"')
  }
  
  url <- "https://health-infobase.canada.ca/src/data/covidLive/covid19.csv"
  
  get_cases_ts <- function(url){
    
    data <- readr::read_csv(url) %>% 
      dplyr::select(-percentoday, -numtotal, -numconf, -numtested, -percentrecover, -ratetested) %>% 
      dplyr::filter(pruid != 1) %>% 
      dplyr::mutate(prname = gsub("Repatriated travellers", "Repatriated Travellers", prname),
             numrecover = as.numeric(gsub("N/A", 0, numrecover)),
             date = lubridate::dmy(date)) %>% 
      dplyr::rename(province = prname, province_fr = prnameFR, cases_probable = numprob, 
             deaths = numdeaths, recovered = numrecover, cases_confirmed = numtoday) %>% 
      tidyr::replace_na(list(cases_probable = 0, deaths = 0, recovered = 0, cases_confirmed = 0)) %>% 
      dplyr::mutate(cases_probable = replace(cases_probable, cases_probable < 0 , 0),
                    deaths = replace(deaths, deaths < 0 , 0),
                    recovered = replace(recovered, recovered < 0 , 0),
                    cases_confirmed = replace(cases_confirmed, cases_confirmed < 0 , 0))

    return(data)
    
  }
  
  get_cases_tot <- function(url){
    
    data <- readr::read_csv(url) %>% 
      dplyr::select(-percentoday, -numtotal, -numconf, -numtested, -percentrecover, -ratetested) %>% 
      dplyr::filter(pruid != 1) %>% 
      dplyr::mutate(prname = gsub("Repatriated travellers", "Repatriated Travellers", prname),
                    numrecover = as.numeric(gsub("N/A", 0, numrecover))) %>% 
      dplyr::rename(province = prname, province_fr = prnameFR, cases_probable = numprob, 
                    deaths = numdeaths, recovered = numrecover, cases_confirmed = numtoday) %>% 
      tidyr::replace_na(list(cases_probable = 0, deaths = 0, recovered = 0, cases_confirmed = 0)) %>% 
      dplyr::group_by(pruid) %>% 
      dplyr::summarise(province = unique(province), 
                       province_fr = unique(province_fr),
                       cases_probable = sum(cases_probable),
                       deaths = sum(deaths),
                       recovered = sum(recovered),
                       cases_confirmed = sum(cases_confirmed)) %>% 
      dplyr::arrange(-cases_confirmed) %>% 
      dplyr::mutate(cases_probable = replace(cases_probable, cases_probable < 0 , 0),
                    deaths = replace(deaths, deaths < 0 , 0),
                    recovered = replace(recovered, recovered < 0 , 0),
                    cases_confirmed = replace(cases_confirmed, cases_confirmed < 0 , 0))
    
    return(data)
    
  }
  
  if (out == "timeseries"){
    
    return(get_cases_ts(url))
    
  } else if (out == "total"){
    
    return(get_cases_tot(url))
    
  }
  
}

