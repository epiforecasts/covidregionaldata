# Japan regional cases, daily
# Source: https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_Japan
# NB. Not up to date: e.g. as of 23/3/20, latest data from 21/3/20
# For an up to date map, see: https://mhlw-gis.maps.arcgis.com/apps/opsdashboard/index.html#/0c5d0502bbb54f9a8dddebca003631b8
#
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text html_table
#' @importFrom tidyr as_tibble
#' @importFrom dplyr mutate filter select
#' @importFrom stringr str_remove_all
#' @importFrom lubridate as_date
#' @importFrom memoise cache_filesystem memoise
#

get_japan_regional_cases <- function() {
  # Locate source
  location <- "https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_Japan"
  # Set up cache
  ch <- memoise::cache_filesystem(".cache")
  mem_read <- memoise::memoise(xml2::read_html, cache = ch)
  # Read webpage
  webpage <- mem_read(location)
  # Get regions
  region <- webpage %>%
    rvest::html_nodes("tr:nth-child(36) th") %>%
    rvest::html_text()
  region <- region[2:40]
  # Get case count
  cases <- webpage %>%
    rvest::html_nodes("table") %>%
    rvest::html_table(fill=T)
  cases <- suppressMessages(suppressWarnings(tidyr::as_tibble(cases[[5]], .name_repair = "universal")  %>%
                                               dplyr::mutate(Date = (lubridate::as_date(Date))) %>%
                                               dplyr::filter(Date == max(Date, na.rm=T))))
  # Show latest date
  message(paste0("Latest date found: ", cases$Date))
  # Combine and clean
  cases <- dplyr::select(cases, 2:40)
  cases <- t(cases)
  cases <- cbind(region, cases)
  rownames(cases) <- c()
  colnames(cases) <- c("region", "cases")
  cases <- tidyr::as_tibble(cases) %>%
    dplyr::mutate(cases = ifelse(cases == "", 0, cases),
                  cases = as.numeric(cases),
                  region = iconv(region, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
                  region = stringr::str_remove_all(region, "\\s"))

  return(cases)
}


# # Mapping: set up
# data <- get_japan_regional_cases()
# regions <- rnaturalearth::ne_states("Japan", returnclass = "sf")
# regions_with_data <- regions %>%
#   left_join(data, by = c("name" = "region")) %>%
#   mutate(cases =  ifelse(is.na(cases), 0, cases))
#
# # Map inset: Okinawa
# jp_okinawa <- dplyr::filter(regions_with_data, name == "Okinawa")
# jp_okinawa <- ggplot2::ggplot(jp_okinawa) +
#    geom_sf(aes(fill = cases)) +  coord_sf(datum = NA) +
#    xlab(jp_okinawa$region) +
#    theme_bw() +  theme(legend.position = "none")
#
# # Map: mainland with insets
# jp_main <- dplyr::filter(regions_with_data, name != "Okinawa") %>%
#   ggplot() +
#     geom_sf(aes(fill = cases)) +
#     coord_sf(crs = sf::st_crs(4326), xlim = c(127, 146), ylim = c(29, 46)) +
#     theme_bw()
#
# jp_main +
#   annotation_custom(
#    grob = ggplotGrob(jp_okinawa), xmin = 140,  xmax = 146,  ymin = 24,  ymax = 37)


