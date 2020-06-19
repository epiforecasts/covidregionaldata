#' Regional Daily COVID-19 Count Data
#'
#' @description Extracts daily COVID-19 data for Italy, stratified by Region. 
#' Data available at  \url{https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni-__date__.csv}. 
#' It is loaded and then sanitised.
#' @return A data frame of daily COVID cases for Italy by region, to be further processed by \code{get_regional_covid_data()}.
#' @importFrom lubridate ymd as_date
#' @importFrom purrr map_dfr
#' @importFrom dplyr mutate select arrange group_by ungroup %>%
#'
get_italy_regional_cases <- function() {

  ## Function to get daily files --------------------------------------------
  url <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni-__date__.csv"
  
  get_daily_files = function(date){
      csv_reader(file = gsub("__date__", format(date, "%Y%m%d"), x = url))
  }

  ## Extract daily data -----------------------------------------------------
  start_date <- as.Date(format(lubridate::ymd(20200224), "%Y-%m-%d"))
  end_date <-  as.Date(Sys.Date() - 1)

  dates <- seq(start_date, end_date, by = "day")
  italy_data <- purrr::map_dfr(dates, get_daily_files)

  ## Clean data -------------------------------------------------------------
  italy_data <- italy_data %>%
                  dplyr::mutate(date = lubridate::as_date(lubridate::ymd_hms(data)),
                                region_level_1 = as.character(denominazione_regione),
                                cases_total = totale_casi,
                                deaths_total = deceduti,
                                tested_total = tamponi) %>%
                  dplyr::select(date, region_level_1, cases_total, deaths_total, tested_total) %>%
                  dplyr::arrange(date) %>%
                  dplyr::mutate(region_level_1 = dplyr::recode(region_level_1,
                                                       "P.A. Trento" = "Trentino-Alto Adige",
                                                       "P.A. Bolzano" = "Trentino-Alto Adige")) %>%
                  dplyr::group_by(date, region_level_1) %>%
                  dplyr::mutate(cases_total = sum(cases_total)) %>%
                  dplyr::ungroup()

  return(italy_data)
}


