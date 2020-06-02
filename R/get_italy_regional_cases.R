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

  ## Set up cache
  ch <- memoise::cache_filesystem(".cache")
  mem_read <- memoise::memoise(readr::read_csv, cache = ch)

  ## Function to get daily files
  get_daily_files = function(date){
      mem_read(file = gsub("__date__", format(date, "%Y%m%d"), x = url), col_types = readr::cols())
  }

  ## Extract daily data
  start_date <- as.Date(format(lubridate::ymd(20200224), "%Y-%m-%d"))
  end_date <-  as.Date(Sys.Date() - 1)

  dates <- seq(start_date, end_date, by = "day")
  italy_data <- purrr::map_dfr(dates, get_daily_files)

  ## Clean variables
  italy_data <- italy_data %>%
                  dplyr::mutate(date = as_date(lubridate::ymd_hms(data)),
                                region = as.character(denominazione_regione),
                                cumulative_cases = totale_casi,
                                cumulative_deaths = deceduti,
                                cumulative_tests = tamponi) %>%
                  dplyr::select(date, region, cumulative_cases, cumulative_deaths, cumulative_tests) %>%
                  dplyr::arrange(date) %>%
                  dplyr::mutate(region = dplyr::recode(region,
                                                       "P.A. Trento" = "Trentino-Alto Adige",
                                                       "P.A. Bolzano" = "Trentino-Alto Adige")) %>%
                  dplyr::group_by(date, region) %>%
                  dplyr::mutate(cumulative_cases = sum(cumulative_cases)) %>%
                  dplyr::ungroup()

  return(italy_data)
}


