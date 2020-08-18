#### These functions are exported and used to meet the Covid19R standard https://covid19r.github.io/documentation/standardized-package-functions.html
#### They are essentially wrappers around get_regional_data

#' Get daily Afghan COVID-19 count data by Province (Wilayat)
#' @description Fetches  COVID-19 count data, stratified by date and province.
#' Data sourced from https://docs.google.com/spreadsheets/d/1F-AMEDtqK78EA6LYME2oOsWQsgJi4CT3V_G4Uo-47Rg/export?format=csv.
#' @return A tibble of COVID cases by province in Afghanistan.
#' @export
refresh_covidregionaldata_afghanistan <- function() {  
  data <- get_regional_data("afghanistan", totals = FALSE, include_level_2_regions = FALSE)
  data <- convert_to_covid19R_format(data)
  return(tibble::tibble(data))
}

#' Get daily Belgian COVID-19 count data by Region
#' @description Fetches COVID-19 count data, stratified by date and region
#' Data sourced from https://epistat.sciensano.be/Data (3 different datasets - MORT, AGE_SEX and HOSP used)
#' @return A tibble of COVID cases by province in Belgium.
#' @export
refresh_covidregionaldata_belgium <- function() {
  data <- get_regional_data("belgium", totals = FALSE, include_level_2_regions = FALSE)
  data <- convert_to_covid19R_format(data)
  return(tibble::tibble(data))
}

#' Get daily Brazilian COVID-19 count data by State (Estado)
#' @description Fetches COVID-19 count data, stratified by date and state
#' Data sourced from https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv.
#' @return A tibble of COVID cases by province in Brazil
#' @export
refresh_covidregionaldata_brazil <- function() {
  data <- get_regional_data("brazil", totals = FALSE, include_level_2_regions = FALSE)
  data <- convert_to_covid19R_format(data)
  return(tibble::tibble(data))
}

#' Get daily Canada COVID-19 count data by Province/Territory
#' @description Fetches COVID-19 count data, stratified by date and province.
#' Data sourced from https://health-infobase.canada.ca/src/data/covidLive/covid19.csv.
#' @return A tibble of COVID cases by province in Canada.
#' @export
refresh_covidregionaldata_canada <- function() {
  data <- get_regional_data("canada", totals = FALSE, include_level_2_regions = FALSE)
  data <- convert_to_covid19R_format(data)
  return(tibble::tibble(data))
}

#' Get daily Colombian COVID-19 count data by Department (Departamento).
#' @description Fetches  COVID-19 count data, stratified by date and region.
#' Data sourced from https://raw.githubusercontent.com/ideascol/covid19/master/data/data_dptos_trend.csv.
#' @return A tibble of COVID cases by province in Colombia.
#' @export
refresh_covidregionaldata_colombia <- function() {
  data <- get_regional_data("colombia", totals = FALSE, include_level_2_regions = FALSE)
  data <- convert_to_covid19R_format(data)
  return(tibble::tibble(data))
}

#' Get daily German COVID-19 count data by State (Bundesland)
#' @description Fetches  COVID-19 count data, stratified by date and state.
#' Data sourced from https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv.
#' @return A tibble of COVID cases by province in Germany.
#' @export
refresh_covidregionaldata_germany <- function() {
  data <- get_regional_data("germany", totals = FALSE, include_level_2_regions = FALSE)
  data <- convert_to_covid19R_format(data)
  return(tibble::tibble(data))
}

#' Get daily Indian COVID-19 count data by State/Unified Territory
#' @description Fetches COVID-19 count data, stratified by date and state.
#' Data sourced from https://api.covid19india.org/csv/latest/state_wise_daily.csv.
#' @return A tibble of COVID cases by province in India
#' @export
refresh_covidregionaldata_india <- function() {
  data <- get_regional_data("india", totals = FALSE, include_level_2_regions = FALSE)
  data <- convert_to_covid19R_format(data)
  return(tibble::tibble(data))
}

#' Get daily Italian COVID-19 count data by Region (Regioni).
#' @description Fetches  COVID-19 count data, stratified by date and region.
#' Data sourced from https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni-___date___.csv.
#' @return A tibble of COVID cases by province in Italy.
#' @export
refresh_covidregionaldata_italy <- function() {
  data <- get_regional_data("italy", totals = FALSE, include_level_2_regions = FALSE)
  data <- convert_to_covid19R_format(data)
  return(tibble::tibble(data))
}

#' Get daily Russian COVID-19 count data by Russian region.
#' @description Fetches  COVID-19 count data, stratified by date and region.
#' Data sourced from https://raw.githubusercontent.com/grwlf/COVID-19_plus_Russia/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_RU.csv.
#' @return A tibble of COVID cases by province in Russia.
#' @export
refresh_covidregionaldata_russia <- function() {
  data <- get_regional_data("russia", totals = FALSE, include_level_2_regions = FALSE)
  data <- convert_to_covid19R_format(data)
  return(tibble::tibble(data))
}

#' Get daily UK COVID-19 count data by EU-defined region
#' @description Fetches  COVID-19 count data, stratified by date and region.
#' Data sourced from https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv and
#' https://raw.githubusercontent.com/tomwhite/covid-19-uk-data/master/data/covid-19-cases-uk.csv.
#' @return A tibble of COVID cases by EU region in UK.
#' @export
refresh_covidregionaldata_uk <- function() {
  data <- get_regional_data("uk", totals = FALSE, include_level_2_regions = FALSE)
  data <- convert_to_covid19R_format(data)
  return(tibble::tibble(data))
}

#' Get daily USA COVID-19 count data by state.
#' @description Fetches  COVID-19 count data, stratified by date and region.
#' Data sourced from https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv.
#' @return A tibble of COVID cases by state in USA
#' @export
refresh_covidregionaldata_usa <- function() {
  data <- get_regional_data("usa", totals = FALSE, include_level_2_regions = FALSE)
  data <- convert_to_covid19R_format(data)
  return(tibble::tibble(data))
}

#' Get meta information about the covidregionaldata refresh_* data getters.
#' @description Find out function names, data returned, subregion level and raw data sources, alongside package info such as licence details.
#' @return A tibble of metadata about covidregionaldata
#' @export
get_info_covidregionaldata <- function() {
  tibble::tribble(
    ~data_set_name, ~package_name, ~name_to_access_with,
    ~function_to_get_data,
    ~data_details, ~data_url, ~license_url,
    ~data_types, ~location_types,
    ~spatial_extent, ~has_geospatial_info,

    "covidregionaldata_afghanistan",
    "covidregionaldata",
    "afghanistan",
    "refresh_covidregionaldata_afghanistan",
    "Daily Covid-19 count data for Provinces (Wil\u0101yat) of Afghanistan",
    "https://docs.google.com/spreadsheets/d/1F-AMEDtqK78EA6LYME2oOsWQsgJi4CT3V_G4Uo-47Rg/export?format=csv",
    "https://github.com/epiforecasts/covidregionaldata/blob/master/LICENSE",
    "cases_new, cases_total, deaths_new, deaths_total, recovered_new, recovered_total, hosp_new, tested_total",
    "state",
    "country",
    FALSE,

    "covidregionaldata_belgium",
    "covidregionaldata",
    "belgium",
    "refresh_covidregionaldata_belgium",
    "Daily Covid-19 count data for Regions (R\u00E9gions) of Belgium",
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
    "brazil",
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
    "canada",
    "refresh_covidregionaldata_canada",
    "Daily Covid-19 count data for Provinces/Territories of Canada",
    "https://health-infobase.canada.ca/src/data/covidLive/covid19.csv",
    "https://github.com/epiforecasts/covidregionaldata/blob/master/LICENSE",
    "cases_new, cases_total, deaths_new, deaths_total, recovered_new, recovered_total, hosp_new, tested_total",
    "state",
    "country",
    FALSE,
    
    "covidregionaldata_colombia",
    "covidregionaldata",
    "colombia",
    "refresh_covidregionaldata_colombia",
    "Daily Covid-19 count data for Departments of Colombia",
    "https://raw.githubusercontent.com/ideascol/covid19/master/data/data_dptos_trend.csv",
    "https://github.com/epiforecasts/covidregionaldata/blob/master/LICENSE",
    "cases_new, cases_total, deaths_new, deaths_total, recovered_new, recovered_total, hosp_new, tested_total",
    "state",
    "country",
    FALSE,

    "covidregionaldata_germany",
    "covidregionaldata",
    "germany",
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
    "india",
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
    "italy",
    "refresh_covidregionaldata_italy",
    "Daily Covid-19 count data for regions (Regioni) of Italy",
    "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni-<date>.csv",
    "https://github.com/epiforecasts/covidregionaldata/blob/master/LICENSE",
    "cases_new, cases_total, deaths_new, deaths_total, recovered_new, recovered_total, hosp_new, tested_total",
    "state",
    "country",
    FALSE,
    
    "covidregionaldata_russia",
    "covidregionaldata",
    "russia",
    "refresh_covidregionaldata_russia",
    "Daily Covid-19 count data for Regions of Russia",
    "https://raw.githubusercontent.com/grwlf/COVID-19_plus_Russia/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_RU.csv",
    "https://github.com/epiforecasts/covidregionaldata/blob/master/LICENSE",
    "cases_new, cases_total, deaths_new, deaths_total, recovered_new, recovered_total, hosp_new, tested_total",
    "state",
    "country",
    FALSE,
    
    "covidregionaldata_uk",
    "covidregionaldata",
    "uk",
    "refresh_covidregionaldata_uk",
    "Daily Covid-19 count data for EU-defined regions of the UK",
    "https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv, 
    https://raw.githubusercontent.com/tomwhite/covid-19-uk-data/master/data/covid-19-cases-uk.csv",
    "https://github.com/epiforecasts/covidregionaldata/blob/master/LICENSE",
    "cases_new, cases_total, deaths_new, deaths_total, recovered_new, recovered_total, hosp_new, tested_total",
    "state",
    "country",
    FALSE,
    
    "covidregionaldata_usa",
    "covidregionaldata",
    "usa",
    "refresh_covidregionaldata_usa",
    "Daily Covid-19 count data for states of the US",
    "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv",
    "https://github.com/epiforecasts/covidregionaldata/blob/master/LICENSE",
    "cases_new, cases_total, deaths_new, deaths_total, recovered_new, recovered_total, hosp_new, tested_total",
    "state",
    "country",
    FALSE
  )
}
