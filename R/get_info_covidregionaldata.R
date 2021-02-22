#' Get meta information about the covidregionaldata datasets.
#' Use this also as a guide for setting up new country datasets
#' @description Find out function names, data returned, subregion level and raw data sources, alongside package info
#' @return A tibble of metadata about covidregionaldata
#' @export
get_info_covidregionaldata <- function() {
  tibble::tribble(
    ~get_data_function, # get_regional_data for country data
    ~country, # country name
    ~level_1_region, # local region name: see utils.R > rename_region_column
    ~level_2_region,
    ~function_options, # note if not a country dataset; extra options which are otherwise unseen
    ~data_url, # source data
    ~source_data_cols, # columns in raw data (to clarify which we have added in post-processing)

    "get_regional_data",
    "afghanistan",
    "provinces (Wil\u0101yat)",
    NA,
    NA,
    "https://docs.google.com/spreadsheets/d/1F-AMEDtqK78EA6LYME2oOsWQsgJi4CT3V_G4Uo-47Rg/export?format=csv",
    "cases_total, deaths_total, recovered_total",

    "get_regional_data",
    "belgium",
    "regions (R\u00E9gions)",
    "provinces",
    NA,
    "https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv, https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv, https://epistat.sciensano.be/Data/COVID19BE_MORT.csv",
    "cases_new, deaths_new, hosp_new",

    "get_regional_data",
    "brazil",
    "states (Estados)",
    "cities",
    NA,
    "https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv",
    "cases_new, cases_total, deaths_new, deaths_total",

    "get_regional_data",
    "canada",
    "provinces/territories",
    NA,
    NA,
    "https://health-infobase.canada.ca/src/data/covidLive/covid19.csv",
    "cases_new, cases_total, deaths_total, recovered_total, tested_total",
    
    "get_regional_data",
    "colombia",
    "departamentos",
    NA,
    NA,
    "https://raw.githubusercontent.com/ideascol/covid19/master/data/data_dptos_trend.csv",
    "cases_total",

    "get_regional_data",
    "cuba",
    "provinces",
    NA,
    NA,
    "https://covid19cubadata.github.io",
    "cases_new",
    
    "get_regional_data",
    "germany",
    "bundeslander",
    "landkreis",
    NA,
    "https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv",
    "cases_new, deaths_new",

    "get_regional_data",
    "india",
    "states and union territories",
    NA,
    NA,
    "https://api.covid19india.org/csv/latest/state_wise_daily.csv",
    "cases_new, deaths_new, recovered_new",

    "get_regional_data",
    "italy",
    "regioni",
    NA,
    NA,
    "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni-<date>.csv",
    "cases_total, deaths_total, tested_total",
    
    "get_regional_data",
    "france",
    "r\u00e9gion",
    "d\u00e9partement",
    NA,
    "https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-resultats-des-tests-virologiques-covid-19/",
    "cases_new, tested_new",
    
    "get_regional_data",
    "lithuania",
    "county",
    "municipality",
    NA,
    "https://opendata.arcgis.com/datasets/45b76303953d40e2996a3da255bf8fe8_0",
    "cases_new, deaths_new, recovered_new, cases_total, deaths_total, recovered_total",
    
    "get_regional_data",
    "russia",
    "Source no longer updated",
    NA,
    NA,
    "https://raw.githubusercontent.com/grwlf/COVID-19_plus_Russia/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_RU.csv",
    "cases_total",
    
    "get_regional_data",
    "south africa",
    "provinces",
    NA,
    NA,
    "https://github.com/dsfsi/covid19za",
    "cases_total, deaths_total",
    
    "get_regional_data",
    "uk",
    "ONS/NHS regions",
    "local authority",
    "get_regional_data('uk', nhsregions = TRUE); get_regional_data('uk', include_level_2_regions = TRUE, resolution = c('ltla', 'utla'))",
    "https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv, https://raw.githubusercontent.com/tomwhite/covid-19-uk-data/master/data/covid-19-cases-uk.csv",
    "cases_new, cases_total, deaths_new, deaths_total, recovered_new, recovered_total, hosp_new, tested_total",

    "get_regional_data",
    "usa",
    "states",
    "counties",
    NA,
    "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv",
    "cases_new, cases_total, deaths_new, deaths_total, hosp_new, tested_total",

    # Global data
    "get_national_data",
    "who",
    NA,
    NA,
    NA,
    "https://covid19.who.int",
    "cases_new, cases_total, deaths_new, deaths_total",
    
    "get_national_data",
    "ecdc",
    NA,
    NA,
    NA,
    "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
    "cases_new, deaths_new",
    
    # other
    "get_interventions_data",
    "acaps policy interventions",
    NA,
    NA,
    NA,
    "https://data.humdata.org/dataset/acaps-covid19-government-measures-dataset",
    NA,
    
    "get_linelist",
    "international public patient records",
    NA,
    NA,    
    "get_linelist(clean = TRUE)",
    "https://github.com/beoutbreakprepared/nCoV2019",
    "id, country, death, date_onset, date_confirm, date_admission_hospital, date_death_or_discharge"
    
    )
}
