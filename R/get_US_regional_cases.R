
#' Fetch daily COVID cases by state for United States of America
#'
#' @return
#' @export
#' @importFrom memoise cache_filesystem memoise
#' @importFrom dplyr filter select mutate arrange group_by n lag
#' @importFrom tidyr gather
#' @importFrom lubridate mdy
#' @examples
#'
get_US_regional_cases <- function(){

  path <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"

  ch <- memoise::cache_filesystem(".cache")


  mem_read <- memoise::memoise(readr::read_csv, cache = ch)

  cases <- mem_read(path)

  cases <- cases %>% dplyr::filter(`Province/State` %in% state.name) %>%
    dplyr::select(-`Country/Region`,-Lat,-Long, region = `Province/State`) %>%
    tidyr::gather(key = "date", value = "total_cases", -region) %>%
    dplyr::mutate(date = lubridate::mdy(date)) %>%
    dplyr::arrange(date) %>%
    dplyr::group_by(region) %>%
    dplyr::mutate(
      index = 1:dplyr::n(),
      cases = total_cases - ifelse(index == 1, 0, dplyr::lag(total_cases))) %>%
    dplyr::ungroup() %>%
    dplyr::select(-index, -total_cases) %>%
    ## Adjust negative cases by setting to 0
    dplyr::mutate(cases = ifelse(cases < 0 , 0, cases)) %>%
    dplyr::filter(region != "Grand Princess", region != "Diamond Princess")


  # Example plotting code
  # regions <- rnaturalearth::ne_states("United States of America", returnclass = "sf")
  #
  # regions %>% dplyr::filter(name %in% cases$region) %>%
  #   ggplot2::ggplot() + ggplot2::geom_sf()

  return(cases)
}



