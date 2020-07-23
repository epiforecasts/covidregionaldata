#' Get International Linelist Data
#'
#' @description This function downloads the latest international linelist. It uses `memoise` to cache the
#' results locally. To clear the cache and redownload the data use `reset_cache`. The cache will be located
#' in the directory at which the function is run. As this linelist is experiencing a high user demand it may not always be available.
#' To account for this we keep a cache in the `covidregionaldata` GitHub repo which this function will fall back to with a warning if the source
#' cannot be downloaded.
#' @param clean_dates Logical, defaults to `TRUE`. Should the data returned be cleaned for use.
#' @param report_delay_only Logical, defaults to `FALSE`. Should only certain variables (id, country, onset date, days' delay), and observations (patients with a report delay) be returned
#' @importFrom dplyr if_else select mutate filter
#' @importFrom lubridate dmy
#' @importFrom tibble as_tibble
#' @return A linelist of case data
#' @export
#' @author Sam Abbott <sam.abbott@lshtm.ac.uk>
#' @examples
#'
#'get_international_linelist
get_international_linelist <- function(clean_dates = TRUE, report_delay_only = FALSE) {

  message("Downloading linelist")
  
  url <- "https://raw.github.com/beoutbreakprepared/nCoV2019/master/latest_data/latestdata.tar.gz"
  
  download.file(url, destfile = "tmp.tar.gz")
  
  linelist <- untar("tmp.tar.gz", files = "latestdata.csv") %>%
    tibble::as_tibble()
  
  
  if (any(class(linelist) %in% "try-error") | nrow(linelist) == 1) {
    
    if(nrow(linelist) == 1){
      message("Problem reading linelist")
    } else {
      message("Problem getting linelist source")
    }
    
    date <- list.files("data-raw/linelist-delays")[1]
    date <- sub("linelist-delays-", "", date)
    date <- sub(".csv", "", date)
    
    message(paste0("Using the covidregionaldata cache for report delays. Cache may not be up to date: downloaded on ", date))

    path <- paste0("data-raw/linelist-delays/linelist-delays-", date, ".csv")

      linelist <- suppressWarnings(
        suppressMessages(
          read.csv(path) %>%
            tibble::as_tibble()
        )
      )
      
      return(linelist)
  }


  if (clean_dates) {
    
    linelist <- linelist %>%
      dplyr::mutate(date_confirm = suppressWarnings(lubridate::dmy(date_confirmation)),
                    date_onset = suppressWarnings(lubridate::dmy(date_onset_symptoms)),
                    date_admission_hospital = suppressWarnings(lubridate::dmy(date_admission_hospital)),
                    date_death_or_discharge = suppressWarnings(lubridate::dmy(date_death_or_discharge)),
                    days_onset_to_report = as.integer(as.Date(date_confirm) - as.Date(date_onset))) %>%
      dplyr::select(id = ID, country, 
                    date_onset, date_confirm, date_admission_hospital, date_death_or_discharge,
                    days_onset_to_report)
    
  }
  
  if(report_delay_only){
    
    linelist <- dplyr::filter(linelist,
                              !is.na(days_onset_to_report)) %>%
      dplyr::select(id, country, date_onset, days_onset_to_report)
    
  }

  return(linelist)

}
