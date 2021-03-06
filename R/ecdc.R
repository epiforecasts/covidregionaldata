#' Download ECDC Daily COVID-19 Count Data
#'
#' @description Downloads ECDC Covid-19 data from the opendata 
#' portal: https://opendata.ecdc.europa.eu/covid19/casedistribution/csv
#' @export
#' @inheritParams download_regional
#' @method download_regional crd_ecdc_1
#' @author Sam Abbott
#' @examples
#' \dontrun{
#' ecdc <- new_covidregionaldata("ecdc")
#' ecdc <- download_data(ecdc)
#' ecdc$raw
#' }
download_regional.crd_ecdc_1 <- function(region, verbose = TRUE, ...) {
  url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
  region$raw <- csv_reader(file = url)
  return(region)
}

#' ECDC Specific Country Level Data Cleaning
#'
#' @description Clean downloaded ECDC data
#' @export
#' @inheritParams clean_regional
#' @method clean_regional crd_ecdc_1
#' @author Sam Abbott @seabbs
#' @author D. van Muijen @dmuijen
#' @author Kath Sherratt @kathsherratt
#' @importFrom dplyr mutate rename select arrange filter
#' @importFrom stringr str_replace_all
#' @importFrom countrycode countryname
#' @examples
#' \dontrun{
#' ecdc <- new_covidregionaldata("ecdc")
#' ecdc <- download_regional(ecdc)
#' clean_regional(ecdc)$clean
#' }
clean_regional.crd_ecdc_1 <- function(region, verbose = TRUE, ...) {
  region$clean <- region$raw %>%
    mutate(date = as.Date(.data$dateRep, format = "%d/%m/%Y")) %>%
    rename(
      iso_code = .data$geoId, country = .data$countriesAndTerritories,
      cases_new = .data$cases, deaths_new = .data$deaths,
      population_2019 = .data$popData2019
    ) %>%
    select(.data$date, .data$country, .data$iso_code,
           .data$population_2019, .data$cases_new, .data$deaths_new) %>%
    arrange(.data$date) %>%
    filter(.data$country != "Cases_on_an_international_conveyance_Japan") %>%
    mutate(
      cases_new = ifelse(.data$cases_new < 0, 0, .data$cases_new),
      country = str_replace_all(.data$country, "_", " "),
      country = countryname(.data$country,
                            destination = "country.name.en",
                            warn = FALSE),
      iso_code = ifelse(.data$country == "Namibia", "NA", .data$iso_code),
      un_region = countrycode(.data$iso_code, origin = "iso2c",
                              destination = "un.region.name",
                              warn = FALSE),
      un_region = ifelse(.data$iso_code == "XK", "Europe",
                         .data$un_region),
      un_region = ifelse(.data$iso_code == "UK", "Europe",
                         .data$un_region),
      un_region = ifelse(.data$iso_code == "EL", "Europe",
                         .data$un_region),
      un_region = ifelse(.data$iso_code == "TW", "Asia",
                         .data$un_region)) %>%
      rename(region_level_1 = .data$country,
             level_1_region_code = .data$iso_code)
  return(region)
}

#' WHO Return Changes
#'
#' @description Specifc return settings for the WHO dataset.
#' @export
#' @method return_regional crd_who_1
#' @inheritParams return_regional
#' @author Sam Abbott
#' @importFrom dplyr group_by ungroup select arrange
#' @importFrom tidyr fill
#' @examples
#' \dontrun{
#' ecdc <- new_covidregionaldata("ecdc")
#' ecdc <- download_regional(ecdc)
#' ecdc <- clean_regional(ecdc)
#' ecdc <- process_regional(ecdc)
#' return_regional(ecdc)
#' }
return_regional.crd_ecdc_1 <- function(region, steps = FALSE) {
  region$return <- region$processed %>%
    group_by(country) %>%
    fill(.data$population_2019, .data$un_region, .direction = "updown") %>%
    ungroup()

  region$return <- region$return %>%
    select(
      .data$date, .data$un_region, .data$country,
      .data$iso_code, .data$population_2019,
      .data$cases_new, .data$cases_total,
      .data$deaths_new, .data$deaths_total, .data$recovered_new,
      .data$recovered_total, .data$hosp_new, .data$hosp_total,
      .data$tested_new, .data$tested_total
    ) %>%
     arrange(.data$date, .data$country)

  if (steps) {
    return(region)
  } else {
    return(region$return)
  }
}
