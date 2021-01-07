#' Confirmed COVID cases by Province each day in Cuba
#'
#' @description Fetches and cleans daily confirmed COVID-19 case data at the province level for Cuba
#' Data available at \url{https://covid19cubadata.github.io/data/covid19-casos.csv}.
#' @return A data frame of daily COVID cases for Cuba by province, to be further processed by \code{get_regional_data()}.
#' @importFrom dplyr select count group_by
#' @importFrom tidyr complete
get_cuba_regional_cases <- function() {
  
  ## Fetch case data
  url <- "https://covid19cubadata.github.io/data/covid19-casos.csv"
  cuba_data <- csv_reader(url)
  
  ## Get daily case counts by province
  cuba_data <- cuba_data %>%
    dplyr::count(fecha_confirmacion, provincia) %>%
    dplyr::select(date = fecha_confirmacion, region_level_1 = provincia, cases_new = n) %>%
    dplyr::filter(!is.na(region_level_1)) %>%
    dplyr::mutate(cases_new = as.numeric(cases_new),
                  date = lubridate::as_date(lubridate::ymd(date)))
  
  return(cuba_data)
}
