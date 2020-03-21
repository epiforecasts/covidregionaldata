#' Get English daily cases by NHS region
#'
#' @return A dataframe of case counts in English NHS regions
#' @export
#' @importFrom dplyr mutate select filter full_join arrange group_by ungroup n lag
#' @importFrom readr read_csv
#' @importFrom tidyr gather
#' @examples
#' get_uk_nhs_region_cases
get_uk_nhs_region_cases <- function() {

  # Path to csv
  path <- "https://raw.githubusercontent.com/emmadoughty/Daily_COVID-19/master/Data/COVID19_cum.csv"

  cases <- readr::read_csv(file = path)


  locations <- c("London","South East","South West","East of England","Midlands",
                 "North East and Yorkshire", "North West", "Ayrshire and Arran",
                 "Borders", "Dumfries and Gallow",
                 "Fife", "Forth Valley", "Grampian", "Greater Glasgow and Clyde",
                 "Highlands", "Lanarkshire", "Lothian", "Shetland", "Tayside",
                 "Wales", "Northern Ireland", "Orkney", "Western Isles")

  # Filter to only NHS regions
  cases_nhs <- cases %>%
    dplyr::mutate(date = stringr::str_replace_all(Date,pattern = "\\.",replacement = "/")) %>%
    dplyr::select(-Date) %>%
    tidyr::gather("UTLA","confirm",-date) %>%
    dplyr::mutate(UTLA = stringr::str_replace_all(UTLA, "_", " ")) %>%
    dplyr::filter(UTLA %in% c("London","South East","South West","East of England","Midlands",
                              "North East and Yorkshire", "North West", "Ayrshire and Arran",
                              "Borders", "Dumfries and Gallow",
                              "Fife", "Forth Valley", "Grampian", "Greater Glasgow and Clyde",
                              "Highlands", "Lanarkshire", "Lothian", "Shetland", "Tayside",
                              "Wales", "Northern Ireland")) %>%
    dplyr::mutate(UTLA = dplyr::recode(UTLA,"Dumfries and Gallow" = "Dumfries and Galloway",
                                       "Highlands" = "Highland"),
                  date = lubridate::dmy(date)) %>%
    tidyr::complete(UTLA = locations,
                    date = seq(min(date), max(date), by = "day")) %>%
    dplyr::mutate(confirm = tidyr::replace_na(confirm, 0))



  # Reformat to daily cases
  cases_nhs <- cases_nhs %>%
    dplyr::select(region = UTLA,
                  total_cases = confirm,
                  date) %>%
    # dplyr::mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
    dplyr::arrange(date) %>%
    dplyr::group_by(region) %>%
    dplyr::mutate(
      index = 1:dplyr::n(),
      cases = total_cases - ifelse(index == 1, 0, dplyr::lag(total_cases))) %>%
    dplyr::ungroup() %>%
    dplyr::select(-index, -total_cases) %>%
    ## Adjust negative cases by setting to 0
    dplyr::mutate(cases = ifelse(cases < 0 , 0, cases))


  return(cases_nhs)
}
