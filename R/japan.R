#' Japan regional cases, daily

#' @description Extracts regional case counts for Japan.
#' [Source](https://mhlw-gis.maps.arcgis.com/apps/opsdashboard/index.html#/0c5d0502bbb54f9a8dddebca003631b8).
#' @importFrom jsonlite fromJSON
#' @importFrom rvest html_nodes html_text html_table
#' @importFrom tidyr replace_na
#' @importFrom dplyr mutate filter select group_by summarise n full_join
#' @importFrom lubridate as_datetime

get_japan_regional_cases <- function(){

  # Locate source
  location <- "https://services8.arcgis.com/JdxivnCyd1rvJTrY/arcgis/rest/services/covid19_list_csv_EnglishView/FeatureServer/0/query?f=json&where=1%3D1&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&orderByFields=No%20desc%2C%E7%A2%BA%E5%AE%9A%E6%97%A5%20desc&resultOffset=0&cacheHint=true"
  # Set up cache
  ch <- memoise::cache_filesystem(".cache")
  mem_read <- memoise::memoise(jsonlite::fromJSON, cache = ch)
  # Read & clean daily cases
  data <- mem_read(location)
  data <- data$features$attributes
  colnames(data) <- c("pref_jp", "age_jp", "gender_jp", "diagnosis_date", "no", "cumulative", "pref_cases", "region", "age_en", "gender_en", "objectid")
  regions <- tibble::enframe(unique(data$region))
  data_df <- data %>%
    dplyr::mutate(date = as.numeric(diagnosis_date) / 1000,
                  date = lubridate::as_datetime(date, tz = "Japan")) %>%
    dplyr::select(region, date)  %>%
    dplyr::filter(date == max(date, na.rm=T)) %>%
    dplyr::group_by(region) %>%
    dplyr::summarise(cases = dplyr::n()) %>%
    dplyr::full_join(regions, by = c("region" = "value")) %>%
    dplyr::select(region, cases) %>%
    dplyr::mutate(cases = tidyr::replace_na(cases, 0))

  return(data_df)
}

