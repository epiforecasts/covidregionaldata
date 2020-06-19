#' Get Russia daily cases
#'
#'
#' @description Fetches COVID case counts by region
#' This data is sourced from https://github.com/grwlf/COVID-19_plus_Russia#data-sources
#' @return A dataframe of case counts in Russian regions
#' @export
#' @importFrom memoise cache_filesystem memoise
#' @importFrom dplyr select group_by ungroup mutate left_join
#' @importFrom tidyr pivot_longer
#' @importFrom readr read_csv
#' @importFrom lubridate mdy
#' @examples
#'
#'
#'\dontrun{
#'
#' Mapping
#' regions <- rnaturalearth::ne_states(geounit = "Russia", returnclass = "sf")
#' data <- get_russia_regional_cases() %>%
#'   dplyr::filter(date == max(date))
#' regions_with_data <- dplyr::left_join(regions, data, by = "iso_3166_2")
#' regions_with_data %>%
#'   ggplot2::ggplot(ggplot2::aes(fill = cases)) +
#'   ggplot2::geom_sf() +
#'   ggplot2::coord_sf(xlim = c(20, max(sf::st_coordinates(regions_with_data))))
#'   }

get_russia_regional_cases <- function() {

  # Read data
  url <- "https://raw.githubusercontent.com/grwlf/COVID-19_plus_Russia/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_RU.csv"

  # Reshape
  russia <- csv_reader(url) %>%
    tidyr::pivot_longer(cols = 12:tidyr::last_col(), names_to = "date") %>%
    dplyr::select(date, region_level_1 = Province_State, cases_total = value) %>%
    dplyr::mutate(date = lubridate::mdy(date))

  return(russia)

}

