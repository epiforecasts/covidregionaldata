#' Get German daily cases by Bundeslander or Landkreis
#'
#'
#' @description Fetches COVID case counts by region in Germany.
#' This data is sourced from the Robert Koch Institute:
#' https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0
#' @param geography Character string indicating the geographic level to return.
#' @return A dataframe of case and death counts in German regions
#' @export
#' @importFrom memoise cache_filesystem memoise
#' @importFrom dplyr select group_by mutate summarise
#' @importFrom readr read_csv
#' @importFrom lubridate ymd_hms
#' @examples
#'
#'
#'\dontrun{
#'country <- rnaturalearth::ne_countries(scale="large",
#'                                         country = "Germany",
#'                                         returnclass = 'sf')
#'
#'regions <- rnaturalearth::ne_states("Germany", returnclass = "sf")
#'
#'data <- get_germany_regional_cases() %>%
#'   dplyr::filter(date == max(date))
#'
#'regions_with_data <- regions %>%
#'   dplyr::left_join(data,
#'                   by = c("name" = "state"))
#'
#'ggplot2::ggplot(regions_with_data) +
#'   ggplot2::geom_sf(ggplot2::aes(fill = cases))
#'
#'}
#'

get_germany_regional_cases <- function(geography = "states") {

  if(!geography %in% c("states", "districts")) {
    stop('Please specify geography: "states" (Bundesland), "districts" (Landkreis). Default: "states".')
  }

  path <- "https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv"

  ## Set up cache
  ch <- memoise::cache_filesystem(".cache")

  mem_read <- memoise::memoise(readr::read_csv, cache = ch)

  germany <- mem_read(file = path) %>%
    dplyr::select(
      state_id = IdBundesland,
      state = Bundesland,
      district_id = IdLandkreis,
      district = Landkreis,
      date = Meldedatum,
      cases = AnzahlFall,
      deaths = AnzahlTodesfall
    ) %>%
    dplyr::mutate(date = lubridate::ymd_hms(date) %>%
                    as.Date())

  germany_state <- germany %>%
    dplyr::group_by(state, date) %>%
    dplyr::summarise(cases = sum(cases),
                     deaths = sum(deaths))
  germany_district <- germany %>%
    dplyr::group_by(district, date) %>%
    dplyr::summarise(cases = sum(cases),
                     deaths = sum(deaths))


  if (geography == "states"){
    return(germany_state)
  }else if (geography == "districts"){
    return(germany_district)
  }
}
