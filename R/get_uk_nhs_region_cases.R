#' Get English daily cases by NHS region
#'
#' @return A dataframe of case counts in English NHS regions
#' @export
#' @importFrom dplyr mutate select filter full_join arrange group_by ungroup n lag
#' @importFrom readr read_csv
#' @importFrom tidyr gather
#' @importFrom sf read_sf st_drop_geometry
#' @examples
#'
get_uk_nhs_region_cases <- function() {

  # Path to csv
  path <- "https://raw.githubusercontent.com/emmadoughty/Daily_COVID-19/master/Data/COVID19_cum.csv"

  cases <- readr::read_csv(file = path)

  # Filter to only NHS regions
  cases_nhs <- cases %>%
    dplyr::mutate(date = stringr::str_replace_all(Date,pattern = "\\.",replacement = "/")) %>%
    dplyr::select(-Date) %>%
    tidyr::gather("UTLA","confirm",-date) %>%
    dplyr::mutate(UTLA = stringr::str_replace_all(UTLA, "_", " ")) %>%
    dplyr::filter(UTLA %in% c("London","South East","South West","East of England","Midlands",
                              "North East and Yorkshire", "North West"))

  # Get area codes from NHS regions shape file and append to cases
  shp_nhs <- sf::read_sf(system.file("extdata",
                                     "NHS_England_Regions",
                                     "NHS_England_Regions_April_2019_EN_BUC.shp",
                                     package = "NCoVUtils")) %>%
    dplyr::select(UTLA = nhser19nm, nhser19cd) %>% sf::st_drop_geometry()

  cases_nhs <- dplyr::full_join(shp_nhs, cases_nhs, by = "UTLA")

  # Reformat to daily cases
  cases_nhs <- cases_nhs %>%
    dplyr::select(region = UTLA,
                  region_code = nhser19cd,
                  total_cases = confirm,
                  date) %>%
    dplyr::mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
    dplyr::arrange(date) %>%
    dplyr::group_by(region) %>%
    dplyr::mutate(
      index = 1:dplyr::n(),
      cases = total_cases - ifelse(index == 1, 0, dplyr::lag(total_cases))) %>%
    dplyr::ungroup() %>%
    dplyr::select(-index, -total_cases) %>%
    ## Adjust negative cases by setting to 0
    dplyr::mutate(cases = ifelse(cases < 0 , 0, cases))

  # Filter Scottish cases
  cases_scot <- cases %>%
    dplyr::mutate(date = stringr::str_replace_all(Date,pattern = "\\.",replacement = "/")) %>%
    dplyr::select(-Date) %>%
    tidyr::gather("UTLA","confirm",-date) %>%
    dplyr::mutate(UTLA = stringr::str_replace_all(UTLA, "_", " ")) %>%
    dplyr::filter(UTLA %in% c("Ayrshire and Arran", "Borders", "Dumfries and Gallow",
                              "Fife", "Forth Valley", "Grampian", "Greater Glasgow and Clyde",
                              "Highlands", "Lanarkshire", "Lothian", "Shetland", "Tayside")) %>%
    dplyr::mutate(UTLA = dplyr::recode(UTLA,"Dumfries and Gallow" = "Dumfries and Galloway",
                                       "Highlands" = "Highland"))

  shp_scot <- sf::read_sf(system.file("extdata",
                                      "SG_NHS_HealthBoards_2019",
                                      "SG_NHS_HealthBoards_2019.shp",
                                      package = "NCoVUtils")) %>%
    dplyr::select(UTLA = HBName, region_code = HBCode) %>% sf::st_drop_geometry()

  cases_scot <- dplyr::left_join(cases_scot,shp_scot, by = "UTLA")

  # Reformat to daily cases
  cases_scot <- cases_scot %>%
    dplyr::select(region = UTLA,
                  region_code,
                  total_cases = confirm,
                  date) %>%
    dplyr::mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
    dplyr::arrange(date) %>%
    dplyr::group_by(region) %>%
    dplyr::mutate(
      index = 1:dplyr::n(),
      cases = total_cases - ifelse(index == 1, 0, dplyr::lag(total_cases))) %>%
    dplyr::ungroup() %>%
    dplyr::select(-index, -total_cases) %>%
    ## Adjust negative cases by setting to 0
    dplyr::mutate(cases = ifelse(cases < 0 , 0, cases))


  return(rbind(cases_scot,cases_nhs))
}
