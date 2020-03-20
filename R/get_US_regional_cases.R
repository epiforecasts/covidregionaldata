
#' Fetch daily COVID cases by state for United States of America
#' @description Fetches daily COVID cases by state collated by the
#' John Hopkins Centre for Systems Science and Engineering (JHU CSSE)
#' Data is available at https://github.com/CSSEGISandData/COVID-19
#' @return A data.frame of COVID cases by region in the US
#' @export
#' @importFrom memoise cache_filesystem memoise
#' @importFrom dplyr filter select mutate arrange group_by n lag
#' @importFrom tidyr gather
#' @importFrom lubridate mdy
#' @examples
#' get_us_regional_cases
#'
#' \dontrun{
#'
#'  regions <- rnaturalearth::ne_states("United States of America", returnclass = "sf")
#'
#'  regions %>% dplyr::filter(name %in% cases$region) %>%
#'  ggplot2::ggplot() + ggplot2::geom_sf()
#'
#' }
get_us_regional_cases <- function(){

  path <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"

  ch <- memoise::cache_filesystem(".cache")


  mem_read <- memoise::memoise(readr::read_csv, cache = ch)

  cases <- mem_read(path)

  cases <- cases %>%
    dplyr::filter(`Province/State` %in% state.name) %>%
    dplyr::select(-`Country/Region`, -Lat, -Long, region = `Province/State`) %>%
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
    dplyr::filter(region != "Grand Princess", region != "Diamond Princess") %>%
    dplyr::full_join(data.frame(region = state.name, region_code = state.abb), by = "region")

  return(cases)
}



