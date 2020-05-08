#' Get India daily cases
#'
#' @description Get Indian cases or deaths by state
#' @param data Character string specifying whether to return case or death count
#' @return A dataframe of case or death counts
#' @export
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate select filter rename left_join
#' @importFrom readr read_csv
#' @importFrom lubridate dmy
#' @examples
#'
#' \dontrun{
#' ## Mapping
#' # Filter to latest date
#' latest_cases <- dplyr::filter(india_cases, date == max(date))
#' # Note that basemaps are not updated to reflect Ladakh as a Union Territory, so joining separately
#' ladakh <- raster::getData('GADM', country='IND', level=2) %>%
#'   sf::st_as_sf() %>%
#'   dplyr::filter(NAME_2 == "Leh (Ladakh)" | NAME_2 == "Kargil") %>%
#'   dplyr::mutate(district = c("Leh", "Kargil"),
#'                 state = "Ladakh") %>%
#'   dplyr::group_by(state) %>%
#'   dplyr::summarise()
#' india_map <- raster::getData('GADM', country='IND', level=1) %>%
#'   sf::st_as_sf() %>%
#'   dplyr::select(state = NAME_1) %>%
#'   # Join  maps
#'   rbind(ladakh) %>%
#'   # Join data
#'   dplyr::left_join(latest_cases, by = c("state" = "name"))
#' # Plot
#' india_map %>%
#'   ggplot2::ggplot(ggplot2::aes(fill = cases)) +
#'   ggplot2::geom_sf()
#' }
#'
#'

get_india_regional_cases <- function(data = "cases") {

  if(!data %in% c("cases", "deaths")) {
    stop("Please specify data to return: cases (default) or deaths.")
  }

  # Set up state names
  state_names <- tibble::tibble(
    code = c("AN", 	"AP", 	"AR", 	"AS", 	"BR", 	"CH", 	"CT", 	"DN", 	"DD", 	"DL", 	"GA", 	"GJ", 	"HR", 	"HP", 	"JK", 	"JH", 	"KA", 	"KL", 	"LA", 	"LD", 	"MP", 	"MH", 	"MN", 	"ML", 	"MZ", 	"NL", 	"OR", 	"PY", 	"PB", 	"RJ", 	"SK", 	"TN", 	"TG", 	"TR", 	"UP", 	"UT", 	"WB"),
    name = c("Andaman and Nicobar", 	"Andhra Pradesh", 	"Arunachal Pradesh", 	"Assam", 	"Bihar", 	"Chandigarh", 	"Chhattisgarh", 	"Dadra and Nagar Haveli", 	"Daman and Diu", 	"NCT of Delhi", 	"Goa", 	"Gujarat", 	"Haryana", 	"Himachal Pradesh", 	"Jammu and Kashmir", 	"Jharkhand", 	"Karnataka", 	"Kerala", 	"Ladakh", 	"Lakshadweep", 	"Madhya Pradesh", 	"Maharashtra", 	"Manipur", 	"Meghalaya", 	"Mizoram", 	"Nagaland", 	"Odisha", 	"Puducherry", 	"Punjab", 	"Rajasthan", 	"Sikkim", 	"Tamil Nadu", 	"Telangana", 	"Tripura", 	"Uttar Pradesh", 	"Uttarakhand", 	"West Bengal"))

  # Set path and get data
  path <- "https://api.covid19india.org/csv/latest/state_wise_daily.csv"
  india_all <- suppressMessages(readr::read_csv(file = path))

  if (data == "cases"){

    india_cases <- india_all %>%
      dplyr::filter(Status == "Confirmed") %>%
      dplyr::select(-Status, -TT) %>%
      tidyr::pivot_longer(-Date, names_to = "state", values_to = "cases") %>%
      dplyr::mutate(Date = lubridate::dmy(Date)) %>%
      dplyr::rename(date = Date) %>%
      dplyr::left_join(state_names, by = c("state" = "code"))

    return(india_cases)

  } else if (data == "deaths"){

    india_deaths <- india_all %>%
      dplyr::filter(Status == "Deceased") %>%
      dplyr::select(-Status, -TT) %>%
      tidyr::pivot_longer(-Date, names_to = "state", values_to = "deaths") %>%
      dplyr::mutate(Date = lubridate::dmy(Date))%>%
      dplyr::rename(date = Date) %>%
      dplyr::left_join(state_names, by = c("state" = "code"))

    return(india_deaths)
  }

}

