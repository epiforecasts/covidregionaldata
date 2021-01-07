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
    dplyr::select(province = provincia, date_confirmation = fecha_confirmacion) %>%
    dplyr::count(date_confirmation, province)
  
  ## Pad out dates and rename
  cuba_data %>%
    dplyr::group_by(province) %>%
    tidyr::complete(date_confirmation = seq.Date(from = min(cuba_data$date_confirmation),
                                                 to = max(cuba_data$date_confirmation),
                                                 by = "day"), fill = list(n = 0)) %>%
    dplyr::select(date_confirmation, province, confirmed_cases = n)
  
  
  return(cuba_data)
}
