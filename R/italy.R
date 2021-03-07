#' Download Italian Regional Daily COVID-19 Count Data
#'
#' @description Extracts daily COVID-19 data for Italy, stratified by .
#' Data available at .
#' @inheritParams download_regional
#' @method download_regional crd_italy_1
#' @author Sam Abbott
#' @export
#' @examples
#' \dontrun{
#' italy <- new_covidregionaldata("italy")
#' italy <- download_regional(italy)
#' italy$raw
#' }
download_regional.crd_italy_1 <- function(region, verbose = TRUE, ...) {
  base_url <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/"
  url <- paste0(base_url, "dati-regioni/dpc-covid19-ita-regioni.csv")
  region$raw <- suppressWarnings(csv_reader(url))
  return(region)
}

#' Italy Specific State Level Data Cleaning
#'
#' @export
#' @inheritParams clean_regional
#' @method clean_regional crd_italy_1
#' @author Sam Abbott
#' @importFrom dplyr mutate select arrange recode group_by ungroup
#' @importFrom lubridate as_date ymd_hms
#' @examples
#' \dontrun{
#' italy <- new_covidregionaldata("italy")
#' italy <- download_regional(italy)
#' clean_regional(italy)$clean
#' }
clean_regional.crd_italy_1 <- function(region, verbose = TRUE, ...) {
  region$clean <- region$raw %>%
    mutate(
      date = as_date(ymd_hms(.data$data)),
      region_level_1 = as.character(.data$denominazione_regione),
      cases_total = .data$totale_casi,
      deaths_total = .data$deceduti,
      tested_total = .data$tamponi
    ) %>%
    arrange(.data$date) %>%
    mutate(region_level_1 = recode(.data$region_level_1,
      "P.A. Trento" = "Trentino-Alto Adige",
      "P.A. Bolzano" = "Trentino-Alto Adige"
    )) %>%
    group_by(.data$date, .data$region_level_1) %>%
    mutate(cases_total = sum(.data$cases_total, na.rm = TRUE)) %>%
    ungroup() %>%
    full_join(region$codes_lookup, by = c("region_level_1" = "region")) %>%
    select(.data$date, .data$region_level_1, level_1_region_code = .data$code,
           .data$cases_total, .data$deaths_total, .data$tested_total)
  return(region)
}
