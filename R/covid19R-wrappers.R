refresh_ncovutils_afghanistan <- function() {
  data <- get_long_format_regional_covid_data("afghanistan")
  return(data)
}

refresh_ncovutils_belgium <- function() {
  data <- get_long_format_regional_covid_data("belgium")
  return(data)
}

refresh_ncovutils_brazil <- function() {
  data <- get_long_format_regional_covid_data("brazil")
  return(data)
}

refresh_ncovutils_canada<- function() {
  data <- get_long_format_regional_covid_data("canada")
  return(data)
}

refresh_ncovutils_germany <- function() {
  data <- get_long_format_regional_covid_data("germany")
  return(data)
}

refresh_ncovutils_india <- function() {
  data <- get_long_format_regional_covid_data("india")
  return(data)
}

refresh_ncovutils_italy <- function() {
  data <- get_long_format_regional_covid_data("italy")
  return(data)
}


get_info_ncovutils <- function() {
  tibble::tribble(
    ~data_set_name, ~package_name, ~function_to_get_data,
    ~data_details, ~data_url, ~license_url,
    ~data_types, ~location_types,
    ~spatial_extent, ~has_geospatial_info,

    "ncovutils_afghanistan",
    "ncovutils",
    "refresh_ncovutils_afghanistan",
    "Daily Covid-19 count data for Provinces (Wilåyat) of Afghanistan",
    "https://docs.google.com/spreadsheets/d/1F-AMEDtqK78EA6LYME2oOsWQsgJi4CT3V_G4Uo-47Rg/export?format=csv",
    "https://github.com/epiforecasts/NCoVUtils/blob/master/LICENSE",
    "cases_new, cases_total, deaths_new, deaths_total, recovered_new, recovered_total, hosp_new, tested_total",
    "state",
    "country",
    FALSE,

    "ncovutils_belgium",
    "ncovutils",
    "refresh_ncovutils_belgium",
    "Daily Covid-19 count data for Regions (Régions) of Belgium",
    "https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv,
    https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv,
    https://epistat.sciensano.be/Data/COVID19BE_MORT.csv",
    "https://github.com/epiforecasts/NCoVUtils/blob/master/LICENSE",
    "cases_new, cases_total, deaths_new, deaths_total, recovered_new, recovered_total, hosp_new, tested_total",
    "state",
    "country",
    FALSE,

    "ncovutils_brazil",
    "ncovutils",
    "refresh_ncovutils_brazil",
    "Daily Covid-19 count data for States (Estados) of Brazil",
    "https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv",
    "https://github.com/epiforecasts/NCoVUtils/blob/master/LICENSE",
    "cases_new, cases_total, deaths_new, deaths_total, recovered_new, recovered_total, hosp_new, tested_total",
    "state",
    "country",
    FALSE,

    "ncovutils_canada",
    "ncovutils",
    "refresh_ncovutils_canada",
    "Daily Covid-19 count data for Provinces/Territories of Canada",
    "https://health-infobase.canada.ca/src/data/covidLive/covid19.csv",
    "https://github.com/epiforecasts/NCoVUtils/blob/master/LICENSE",
    "cases_new, cases_total, deaths_new, deaths_total, recovered_new, recovered_total, hosp_new, tested_total",
    "state",
    "country",
    FALSE,

    "ncovutils_germany",
    "ncovutils",
    "refresh_ncovutils_germany",
    "Daily Covid-19 count data for states (Bundeslander) of Germany",
    "https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv",
    "https://github.com/epiforecasts/NCoVUtils/blob/master/LICENSE",
    "cases_new, cases_total, deaths_new, deaths_total, recovered_new, recovered_total, hosp_new, tested_total",
    "state",
    "country",
    FALSE,

    "ncovutils_india",
    "ncovutils",
    "refresh_ncovutils_india",
    "Daily Covid-19 count data for States/Union Territories of India",
    "https://api.covid19india.org/csv/latest/state_wise_daily.csv",
    "https://github.com/epiforecasts/NCoVUtils/blob/master/LICENSE",
    "cases_new, cases_total, deaths_new, deaths_total, recovered_new, recovered_total, hosp_new, tested_total",
    "state",
    "country",
    FALSE,

    "ncovutils_italy",
    "ncovutils",
    "refresh_ncovutils_italy",
    "Daily Covid-19 count data for regions (Regioni) of Afghanistan",
    "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni-<date>.csv",
    "https://github.com/epiforecasts/NCoVUtils/blob/master/LICENSE",
    "cases_new, cases_total, deaths_new, deaths_total, recovered_new, recovered_total, hosp_new, tested_total",
    "state",
    "country",
    FALSE
  )
}
