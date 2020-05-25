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
#'}
#'

# # Mapping
#' regions <- rnaturalearth::ne_states(geounit = "Russia", returnclass = "sf")
#' data <- get_russia_regional_cases() %>%
#'   dplyr::filter(date == max(date))
#' regions_with_data <- dplyr::left_join(regions, data, by = "iso_3166_2")
#' regions_with_data %>%
#'   ggplot2::ggplot(ggplot2::aes(fill = cases)) +
#'   ggplot2::geom_sf() +
#'   ggplot2::coord_sf(xlim = c(20, max(sf::st_coordinates(regions_with_data))))

get_russia_regional_cases <- function() {

  # Read data
  path <- "https://raw.githubusercontent.com/grwlf/COVID-19_plus_Russia/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_RU.csv"

  # Using cache
  ch <- memoise::cache_filesystem(".cache")
  mem_read <- memoise::memoise(readr::read_csv, cache = ch)
  russia <- mem_read(path)

  # Reshape
  russia <- russia %>%
    tidyr::pivot_longer(cols = 12:133, names_to = "date") %>%
    dplyr::select(date, country = Country_Region, region = Province_State, cases = value) %>%
    dplyr::mutate(date = lubridate::mdy(date))

  # Cumualative to daily
  russia <- dplyr::group_by(russia, region) %>%
    dplyr::mutate(
      cases = c(cases[1], diff(cases)),
      cases = ifelse(cases < 0, 0, cases)) %>%
    dplyr::ungroup()

  # Join to ISO region codes
  iso_path <- "https://raw.githubusercontent.com/ZFTurbo/Covid-19-spread-prediction/master/input/russia_regions.csv"
  iso <- suppressMessages(readr::read_csv(iso_path)) %>%
    dplyr::select(iso_3166_2 = iso_code, region = csse_province_state) %>%
    dplyr::mutate(
      region = stringr::str_replace_all(region, "Republic of Altay", "Altay Republic"),
      region = ifelse(iso_3166_2 == "RU-NEN", "Nenetskiy autonomous oblast", region),
      region = ifelse(iso_3166_2 == "RU-CHU", "Chukotskiy autonomous oblast", region)
      )
  russia <- dplyr::left_join(russia, iso, by = "region")

  return(russia)

}

