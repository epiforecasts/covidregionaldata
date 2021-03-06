#' Download WHO Daily COVID-19 Count Data
#'
#' @description Downloads WHO Covid-19 data from the opendata 
#' portal: https://opendata.ecdc.europa.eu/covid19/casedistribution/csv
#' @export
#' @inheritParams download_regional
#' @method download_regional crd_who_1
#' @author Sam Abbott
#' @examples
#' \dontrun{
#' who <- new_covidregionaldata("who")
#' who <- download_data(who)
#' who$raw
#' }
download_regional.crd_who_1 <- function(region, verbose = TRUE, ...) {
  url <- "https://covid19.who.int/WHO-COVID-19-global-data.csv"
  region$raw <- csv_reader(url)
  return(region)
}

#' WHO Specific Country Level Data Cleaning
#'
#' @description Clean downloaded WHO data
#' @export
#' @inheritParams clean_regional
#' @method clean_regional crd_who_1
#' @author Sam Abbott @seabbs
#' @author Kath Sherratt @kathsherratt
#' @importFrom dplyr mutate rename
#' @importFrom countrycode countrycode
#' @examples
#' \dontrun{
#' who <- new_covidregionaldata("who")
#' who <- download_regional(who)
#' clean_regional(who)$clean
#' }
clean_regional.crd_who_1 <- function(region, verbose = TRUE, ...) {
  region$clean <- region$raw
  colnames(region$clean) <- c("date", "iso_code", "country", "who_region",
                              "cases_new", "cases_total", "deaths_new",
                              "deaths_total")
  region$clean <- region$clean %>%
    mutate(
      country = countrycode(.data$iso_code,
        origin = "iso2c", destination = "country.name.en", warn = FALSE
      ),
      un_region = countrycode(.data$iso_code,
        origin = "iso2c", destination = "un.region.name", warn = FALSE
      ),
      un_region = ifelse(.data$iso_code == "XK", "Europe", .data$un_region),
      country = ifelse(.data$iso_code == "XK", "Kosovo", .data$country)
    ) %>%
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
#' who <- new_covidregionaldata("who")
#' who <- download_regional(who)
#' who <- clean_regional(who)
#' who <- process_regional(who)
#' return_regional(who)
#' }
return_regional.crd_who_1 <- function(region, steps = FALSE) {
  region$return <- region$processed %>%
    group_by(.data$country) %>%
    fill(.data$who_region, .data$un_region, .direction = "updown") %>%
    ungroup()

    region$return <- region$return %>%
      select(
        .data$date, .data$un_region, .data$who_region, .data$country,
        .data$iso_code, .data$cases_new, .data$cases_total,
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