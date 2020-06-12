#### These functions are exported and used to meet the Covid19R standard https://covid19r.github.io/documentation/standardized-package-functions.html
#### They are essentially wrappers around get_long_format_regional_covid_data

#' Get daily Afghan COVID-19 count data by Province (Wilåyat)
#' @description Fetches  COVID-19 count data, stratified by date and province.
#' Data sourced from https://docs.google.com/spreadsheets/d/1F-AMEDtqK78EA6LYME2oOsWQsgJi4CT3V_G4Uo-47Rg/export?format=csv.
#' @return A tibble of COVID cases by province in Afghanistan.
#' @export
refresh_covidregionaldata_afghanistan <- function() {
  data <- get_long_format_regional_covid_data("afghanistan")
  return(data)
}

#' Get daily Belgian COVID-19 count data by Region
#' @description Fetches COVID-19 count data, stratified by date and region
#' Data sourced from https://epistat.sciensano.be/Data (3 different datasets - MORT, AGE_SEX and HOSP used)
#' @return A tibble of COVID cases by province in Belgium.
#' @export
refresh_covidregionaldata_belgium <- function() {
  data <- get_long_format_regional_covid_data("belgium")
  return(data)
}

#' Get daily Brazilian COVID-19 count data by State (Estado)
#' @description Fetches COVID-19 count data, stratified by date and state
#' Data sourced from https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv.
#' @return A tibble of COVID cases by province in Brazil
#' @export
refresh_covidregionaldata_brazil <- function() {
  data <- get_long_format_regional_covid_data("brazil")
  return(data)
}

#' Get daily Canada COVID-19 count data by Province/Territory
#' @description Fetches COVID-19 count data, stratified by date and province.
#' Data sourced from https://health-infobase.canada.ca/src/data/covidLive/covid19.csv.
#' @return A tibble of COVID cases by province in Canada.
#' @export
refresh_covidregionaldata_canada<- function() {
  data <- get_long_format_regional_covid_data("canada")
  return(data)
}

#' Get daily German COVID-19 count data by State (Bundesland)
#' @description Fetches  COVID-19 count data, stratified by date and state.
#' Data sourced from https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv.
#' @return A tibble of COVID cases by province in Germany.
#' @export
refresh_covidregionaldata_germany <- function() {
  data <- get_long_format_regional_covid_data("germany")
  return(data)
}

#' Get daily Indian COVID-19 count data by State/Unified Territory
#' @description Fetches COVID-19 count data, stratified by date and state.
#' Data sourced from https://api.covid19india.org/csv/latest/state_wise_daily.csv.
#' @return A tibble of COVID cases by province in India
#' @export
refresh_covidregionaldata_india <- function() {
  data <- get_long_format_regional_covid_data("india")
  return(data)
}

#' Get daily Italian COVID-19 count data by Region (Regioni).
#' @description Fetches  COVID-19 count data, stratified by date and region.
#' Data sourced from https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni-<date>.csv.
#' @return A tibble of COVID cases by province in Italy.
#' @export
refresh_covidregionaldata_italy <- function() {
  data <- get_long_format_regional_covid_data("italy")
  return(data)
}

#' Get meta information about the covidregionaldata refresh_* data getters.
#' @description Find out function names, data returned, subregion level and raw data sources, alongside package info such as licence details.
#' @return A tibble of metadata about covidregionaldata
#' @export
get_info_covidregionaldata <- function() {
  tibble::tribble(
    ~data_set_name, ~package_name, ~function_to_get_data,
    ~data_details, ~data_url, ~license_url,
    ~data_types, ~location_types,
    ~spatial_extent, ~has_geospatial_info,

    "covidregionaldata_afghanistan",
    "covidregionaldata",
    "refresh_covidregionaldata_afghanistan",
    "Daily Covid-19 count data for Provinces (Wilåyat) of Afghanistan",
    "https://docs.google.com/spreadsheets/d/1F-AMEDtqK78EA6LYME2oOsWQsgJi4CT3V_G4Uo-47Rg/export?format=csv",
    "https://github.com/epiforecasts/covidregionaldata/blob/master/LICENSE",
    "cases_new, cases_total, deaths_new, deaths_total, recovered_new, recovered_total, hosp_new, tested_total",
    "state",
    "country",
    FALSE,

    "covidregionaldata_belgium",
    "covidregionaldata",
    "refresh_covidregionaldata_belgium",
    "Daily Covid-19 count data for Regions (Régions) of Belgium",
    "https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv,
    https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv,
    https://epistat.sciensano.be/Data/COVID19BE_MORT.csv",
    "https://github.com/epiforecasts/covidregionaldata/blob/master/LICENSE",
    "cases_new, cases_total, deaths_new, deaths_total, recovered_new, recovered_total, hosp_new, tested_total",
    "state",
    "country",
    FALSE,

    "covidregionaldata_brazil",
    "covidregionaldata",
    "refresh_covidregionaldata_brazil",
    "Daily Covid-19 count data for States (Estados) of Brazil",
    "https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv",
    "https://github.com/epiforecasts/covidregionaldata/blob/master/LICENSE",
    "cases_new, cases_total, deaths_new, deaths_total, recovered_new, recovered_total, hosp_new, tested_total",
    "state",
    "country",
    FALSE,

    "covidregionaldata_canada",
    "covidregionaldata",
    "refresh_covidregionaldata_canada",
    "Daily Covid-19 count data for Provinces/Territories of Canada",
    "https://health-infobase.canada.ca/src/data/covidLive/covid19.csv",
    "https://github.com/epiforecasts/covidregionaldata/blob/master/LICENSE",
    "cases_new, cases_total, deaths_new, deaths_total, recovered_new, recovered_total, hosp_new, tested_total",
    "state",
    "country",
    FALSE,

    "covidregionaldata_germany",
    "covidregionaldata",
    "refresh_covidregionaldata_germany",
    "Daily Covid-19 count data for states (Bundeslander) of Germany",
    "https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv",
    "https://github.com/epiforecasts/covidregionaldata/blob/master/LICENSE",
    "cases_new, cases_total, deaths_new, deaths_total, recovered_new, recovered_total, hosp_new, tested_total",
    "state",
    "country",
    FALSE,

    "covidregionaldata_india",
    "covidregionaldata",
    "refresh_covidregionaldata_india",
    "Daily Covid-19 count data for States/Union Territories of India",
    "https://api.covid19india.org/csv/latest/state_wise_daily.csv",
    "https://github.com/epiforecasts/covidregionaldata/blob/master/LICENSE",
    "cases_new, cases_total, deaths_new, deaths_total, recovered_new, recovered_total, hosp_new, tested_total",
    "state",
    "country",
    FALSE,

    "covidregionaldata_italy",
    "covidregionaldata",
    "refresh_covidregionaldata_italy",
    "Daily Covid-19 count data for regions (Regioni) of Italy",
    "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni-<date>.csv",
    "https://github.com/epiforecasts/covidregionaldata/blob/master/LICENSE",
    "cases_new, cases_total, deaths_new, deaths_total, recovered_new, recovered_total, hosp_new, tested_total",
    "state",
    "country",
    FALSE
  )
}
