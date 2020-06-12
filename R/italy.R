#' Italian Regional Covid-19 Data
#'
#' @description Fetches COVID data by region. Data is collated by the Italian Department of Civil Protection
#' and is available on github: https://github.com/pcm-dpc/COVID-19. Cleans and sanitises ready for further use.
#' @return A data frame of Italian regional case counts ready to be used by \code{get_regional_covid_data()}
#' @importFrom readr read_csv
#' @importFrom lubridate ymd
#' @importFrom purrr map_dfr
#' @importFrom dplyr mutate select arrange group_by ungroup %>%
#' @importFrom memoise cache_filesystem memoise
#'
get_italy_regional_cases <- function() {

  ## Path to data
  url <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni-__date__.csv"

  ## Function to get daily files
  get_daily_files = function(date){
      csv_reader(file = gsub("__date__", format(date, "%Y%m%d"), x = url))
  }

  ## Extract daily data
  start_date <- as.Date(format(lubridate::ymd(20200224), "%Y-%m-%d"))
  end_date <-  as.Date(Sys.Date() - 1)

  dates <- seq(start_date, end_date, by = "day")
  italy_data <- purrr::map_dfr(dates, get_daily_files)

  ## Clean variables
  italy_data <- italy_data %>%
                  dplyr::mutate(date = as_date(lubridate::ymd_hms(data)),
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


