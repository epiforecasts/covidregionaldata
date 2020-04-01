#' Japan regional cases, daily

#' @description Extracts regional case counts for Japan.
#' [Source](https://mhlw-gis.maps.arcgis.com/apps/opsdashboard/index.html#/0c5d0502bbb54f9a8dddebca003631b8).
#' @importFrom jsonlite fromJSON
#' @importFrom rvest html_nodes html_text html_table
#' @importFrom tidyr replace_na
#' @importFrom dplyr mutate filter select group_by summarise n full_join
#' @importFrom lubridate as_datetime
#' @importFrom memoise cache_filesystem memoise
#' @export
#' @examples
#' get_japan_regional_cases()
#'
#' \dontrun{
#'
#' library(ggplot2)
#' library(dplyr)
#'
#'  # Mapping: set up
#'  data <- get_japan_regional_cases()
#'
#'  regions <- rnaturalearth::ne_states("Japan", returnclass = "sf")
#'
#'  regions_with_data <- regions %>%
#'    dplyr::left_join(data, by = c("name" = "region")) %>%
#'    dplyr::mutate(cases =  ifelse(is.na(cases), 0, cases)
#'
#'    # Map inset: Okinawa
#'    jp_okinawa <- dplyr::filter(regions_with_data, name == "Okinawa")
#'
#'    jp_okinawa <- ggplot2::ggplot(jp_okinawa) +
#'    ggplot2::geom_sf(ggplot2::aes(fill = cases)) +  ggplot2::coord_sf(datum = NA) +
#'    ggplot2::xlab(jp_okinawa$region) +
#'    ggplot2::theme_bw() +
#'    ggplot2::theme(legend.position = "none")
#'
#'    ## Map: mainland with insets
#'    jp_main <- dplyr::filter(regions_with_data, name != "Okinawa") %>%
#'    ggplot2::ggplot() +
#'    ggplot2::geom_sf(ggplot2::aes(fill = cases)) +
#'    ggplot2::coord_sf(crs = sf::st_crs(4326), xlim = c(127, 146), ylim = c(29, 46)) +
#'    ggplot2::theme_bw()
#'
#'    jp_main +
#'    ggplot2::annotation_custom(
#'    grob = ggplot2::ggplotGrob(jp_okinawa), xmin = 140,  xmax = 146,  ymin = 24,  ymax = 37)
#'
#' }

get_japan_regional_cases <- function(){
  
#  #Return error here. 
#  print('This data source has changed. We are currently working to fix it.')
#  return(tibble())
  
  # Locate source
  location <- "https://services8.arcgis.com/JdxivnCyd1rvJTrY/arcgis/rest/services/covid19_list_csv_EnglishView/FeatureServer/0/query?f=json&where=1%3D1&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&orderByFields=No%20desc%2C%E7%A2%BA%E5%AE%9A%E6%97%A5%20desc&resultOffset=0&resultRecordCount=2000&cacheHint=true"
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

