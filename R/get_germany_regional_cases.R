#' Get German daily cases by Bundeslander
#'
#'
#' @description Fetches COVID case counts by region in Germany.
#' This data was collated by Jan-Philip Gehrcke (gh: jgehrcke) and is available at:
#' https://github.com/jgehrcke/covid-19-germany-gae
#' @return A dataframe of case counts in German regions
#' @export
#' @importFrom memoise cache_filesystem memoise
#' @importFrom dplyr select group_by mutate ungroup n lag
#' @importFrom tidyr gather
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
#'                   by = c("iso_3166_2" = "region_code"))
#'
#'ggplot2::ggplot(regions_with_data) +
#'   ggplot2::geom_sf(ggplot2::aes(fill = cases))
#'
#'}
get_germany_regional_cases <- function() {

  path <- "https://raw.githubusercontent.com/jgehrcke/covid-19-germany-gae/master/data.csv"

  ## Set up cache
  ch <- memoise::cache_filesystem(".cache")


  mem_read <- memoise::memoise(readr::read_csv, cache = ch)

  cases <- mem_read(file = path)

  cases <- cases %>%
    dplyr::select(date = time_iso8601,
                  "DE-BB" = `DE-BB_cases`,
                  "DE-BW" = `DE-BW_cases`,
                  "DE-BY" = `DE-BY_cases`,
                  "DE-BE" = `DE-BE_cases`,
                  "DE-HB" = `DE-HB_cases`,
                  "DE-HH" = `DE-HH_cases`,
                  "DE-HE" = `DE-HE_cases`,
                  "DE-MV" = `DE-MV_cases`,
                  "DE-NI" = `DE-NI_cases`,
                  "DE-NW" = `DE-NW_cases`,
                  "DE-RP" = `DE-RP_cases`,
                  "DE-SL" = `DE-SL_cases`,
                  "DE-SN" = `DE-SN_cases`,
                  "DE-SH" = `DE-SH_cases`,
                  "DE-ST" = `DE-ST_cases`,
                  "DE-TH" = `DE-TH_cases`) %>%
    tidyr::gather(key = "region_code", value = "total_cases", -date) %>%
    dplyr::group_by(region_code) %>%
    dplyr::mutate(
      date = as.Date(date),
      index = 1:dplyr::n(),
      cases = total_cases - ifelse(index == 1, 0, dplyr::lag(total_cases))) %>%
    dplyr::ungroup() %>%
    dplyr::select(-index, -total_cases) %>%
    ## Adjust negative cases by setting to 0
    dplyr::mutate(cases = ifelse(cases < 0 , 0, cases),
                  region = dplyr::recode(region_code,
                                         "DE-BW" = "Baden-Wurttemberg",
                                         "DE-BY" = "Bavaria",
                                         "DE-BE" = "Berlin",
                                         "DE-BB" = "Brandenburg",
                                         "DE-HB" = "Bremen",
                                         "DE-HH" = "Hamburg",
                                         "DE-HE" = "Hesse",
                                         "DE-NI" = "Lower Saxony",
                                         "DE-MV" = "Mecklenburg-Vorpommern",
                                         "DE-NW" = "North Rhine-Westphalia",
                                         "DE-RP" = "Rhineland-Palatinate",
                                         "DE-SL" = "Saarland",
                                         "DE-SN" = "Saxony",
                                         "DE-ST" = "Saxony-Anhalt",
                                         "DE-SH" = "Schleswig-Holstein",
                                         "DE-TH" = "Thuringia"))



  return(cases)
}
