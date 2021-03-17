#' Lithuania Regional Daily COVID-19 Count Data - County
#' @inherit get_lithuania_regional_cases_with_level_2
#'
#' @description Extracts daily COVID-19 data for Lithuania, stratified by county (apskritis)
#' and municipality (savivaldybe). Some Lithuanian municipalities share names, there being both a
#' Vilnius city municipality (m. sav.) and a Vilnius regional municipality (r. sav.)
#'
#' Data available at \url{https://opendata.arcgis.com/datasets/d49a63c934be4f65a93b6273785a8449_0}.
#'
#' It is loaded and then sanitised.
#'
#' @return A data frame of COVID cases by county in Lithuania, ready to be used by \code{get_regional_data()}.
#'
#' @seealso [get_lithuania_regional_cases_with_level_2()]
#' @md
#' @importFrom dplyr %>% across summarise group_by if_else ungroup mutate
#' @importFrom tidyselect vars_select_helpers
get_lithuania_regional_cases_only_level_1 <-
  function(
           national_data = FALSE,
           all_osp_fields = FALSE,
           death_definition = "of",
           recovered_definition = "official") {
    # Lithuania only publishes data at the municipality level. To provide
    # data for the level 1 regions (Counties, Apskritis) we get the municipality
    # level data and aggregate it according to municipality
    # level_1_lookup <- tibble::tibble(level_1_region_code = c("LT-AL", "LT-KU", "LT-KL", "LT-MR", "LT-PN",
    #                                        "LT-SA", "LT-TA", "LT-TE", "LT-UT", "LT-VL", NA_character_),
    #                region_level_1 = c("Alytaus apskritis",
    #                                   "Kauno apskritis", "Klaip\u0117dos apskritis", "Marijampol\u0117s apskritis",
    #                                   "Panev\u0117\u017eio apskritis", "\u0160iauli\u0173 apskritis", "Taurag\u0117s apskritis",
    #                                   "Tel\u0161i\u0173 apskritis", "Utenos apskritis", "Vilniaus apskritis", "nenustatyta"),
    #                region_level_1_en = c("Alytus County", "Kaunas County",
    #                                      "Klaip\u0117da County", "Marijampol\u0117 County", "Panev\u0117\u017eys County",
    #                                      "\u0160iauliai County", "Taurag\u0117 County", "Tel\u0161iai County", "Utena County",
    #                                      "Vilnius County", "unstated"))
    #
    county_data <- get_lithuania_regional_cases_with_level_2(
      national_data,
      all_osp_fields,
      death_definition
    ) %>%
      dplyr::group_by(date, region_level_1) %>%
      dplyr::summarise(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric), sum)) %>%
      dplyr::mutate(region_level_1 = if_else(is.na(.data$region_level_1),
        "Lietuva", .data$region_level_1
      )) %>%
      dplyr::ungroup()
    # Fix percentages, where necessary
    if (all_osp_fields) {
      # For each of the test percentage fields, recalculate the percentages,
      # first checking that the total number of checks is not zero.
      county_data <- county_data %>%
        dplyr::mutate(
          ab_prc_day =
            dplyr::if_else(
              .data$ab_tot_day == 0, 0,
              .data$ab_pos_day / .data$ab_tot_day
            ),
          ag_prc_day =
            dplyr::if_else(
              .data$ag_tot_day == 0, 0,
              .data$ag_pos_day / .data$ag_tot_day
            ),
          pcr_prc_day =
            dplyr::if_else(
              .data$pcr_tot_day == 0, 0,
              .data$pcr_pos_day / .data$pcr_tot_day
            ),
          dgn_prc_day =
            dplyr::if_else(
              .data$dgn_tot_day == 0, 0,
              .data$dgn_pos_day / .data$dgn_tot_day
            )
        )
    }
    return(county_data)
  }
#' Lithuanian Daily COVID-19 Count Data - Municipalities
#'
#' @description Extracts daily COVID-19 data for Lithuania, by municipality.
#'
#' Data available at
#' \url{https://opendata.arcgis.com/datasets/d49a63c934be4f65a93b6273785a8449_0}.
#' It is loaded and then sanitised.
#'
#'
#' @return A `data.frame` of COVID cases by municipality in Lithuania, ready to be used by `get_regional_data()`.
#'
#' This function returns `cases_new`, `cases_total`, `deaths_new` and
#' `tested_new`.
#'
#'  By default it uses the `daily_deaths_def1` vector from the
#' OSP for the `deaths_new`; this can be changed with the
#' `death_definition` parameter.
#'
#' @param national_data A logical scalar. Should national values be
#'   returned?  (Defaults `FALSE`)
#' @param all_osp_fields A logical scalar. Should all the meaningful
#'   data fields from the OSP source be returned? (Defaults `FALSE`)
#' @param death_definition A character string. Determines which criteria
#'   for attributing deaths to COVID is used. Should be `"of"`,
#'    `"with"`, or `"after"`. Can also be `"daily_deaths_def1"`,
#'    `"daily_deaths_def2"`, or `"daily_deaths_def3"`. (Defaults
#'    to `"of"`, the strictest definition.)
#' @param recovered_definition A character string. Determines whether
#'   the count of officially-recovered (*de jure*) cases is used, or
#'   the statistical estimate provided by OSP. Should be `"official"`
#'   or `"statistical"`. (Defaults to `"official"`.)
#'
#' @section OSP Data fields:
#'
#' The[Official Statistics Portal](https://osp.stat.gov.lt) provides
#' many data series in their table.
#'
#' The full range of these vectors can be returned by setting
#' `all_osp_fields` to `TRUE`.
#'
#' The following describes the data provided by the OSP.
#'
#'
#' | field | description |
#' | :------ | :------------ |
#' | `date` | the reporting day during which the events occurred or at the end of which the accounting was performed |
#' | `municipality_code` `*` | code of the municipality assigned to persons |
#' | `municipality_name` `+` | the name of the municipality assigned to the persons |
#' | `population` | population size according to the data of the beginning of 2021, according to the declared place of residence |
#' | `ab_pos_day` | Number of positive antibody test responses, days |
#' | `ab_neg_day` | Number of negative antibody test responses, days |
#' | `ab_tot_day` | Number of antibody tests, daily |
#' | `ab_prc_day` | Percentage of positive antibody test responses per day |
#' | `ag_pos_day` | Number of positive antigen test responses, daily |
#' | `ag_neg_day` | Number of negative antigen test responses, daily |
#' | `ag_tot_day` | Number of antigen tests, daily |
#' | `ag_prc_day` | Percentage of positive responses to antigen tests per day |
#' | `pcr_pos_day` | number of positive PCR test responses, daily |
#' | `pcr_neg_day` | Number of PCR test negative responses, daily |
#' | `pcr_tot_day` | number of PCR tests per day |
#' | `pcr_prc_day` | Percentage of positive PCR test responses per day |
#' | `dgn_pos_day` | Number of positive answers to diagnostic tests / tests, days |
#' | `dgn_neg_day` | Number of negative answers to diagnostic tests / tests, days |
#' | `dgn_prc_day` | Number of diagnostic examinations / tests, days |
#' | `dgn_tot_day` | Percentage of positive answers to diagnostic tests / tests per day |
#' | `dgn_tot_day_gmp` | Number of diagnostic examinations / tests of samples collected at mobile points, days |
#' | `daily_deaths_def1` | The number of new deaths per day according to the (narrowest) COVID death definition No. 1. `#` |
#' | `daily_deaths_def2` | Number of new deaths per day according to COVID death definition No. 2. `#`  |
#' | `daily_deaths_def3` | Number of new deaths per day according to COVID death definition No. 3. `#` |
#' | `daily_deaths_all` | Daily deaths in Lithuania (by date of death) |
#' | `incidence` + | Number of new COVID cases per day (laboratory or physician confirmed) |
#' | `cumulative_totals` + | Total number of COVID cases (laboratory or physician confirmed) |
#' | `active_de_jure` | Declared number of people with COVID |
#' | `active_sttstcl` | Statistical number of people with COVID |
#' | `dead_cases` | The number of dead persons who were ever diagnosed with COVID |
#' | `recovered_de_jure` | Declared number of recovered live persons |
#' | `recovered_sttstcl` | Statistical number of recovered live persons |
#' | `map_colors` `$` | The map colour-coding for the municipality, based on averages of test positivity and incidence per capita |
#'
#' `*` The `municipality_code` is discarded since it does not correspond
#' to ISO-3166:2 codes used elsewhere in the package.
#'
#' `+` These fields are renamed but returned unmodified.
#'
#' `#` Lithuania offers counts according to three
#' different definitions of whether a death is attributable to COVID-19.
#'
#'  `$` This field is not recalculated for counties and is deleted.
#'
#' @section Criteria for attributing deaths:
#'
#' Beginning in February 2021 the OSP publishes death counts according to
#' three different criteria, from most to least strictly attributed to
#' COVID-19.
#'
#' 1. *`of`* Number of deaths with COVID-19 (coronavirus infection) as
#'    the leading cause of death. The indicator is calculated by summing
#'    all registered records of medical form E106 (unique persons), in which
#'    the main cause of death is IPC disease codes U07.1 or U07.2. Deaths
#'    due to external causes are not included (ICD disease codes are V00-Y36,
#'     or Y85-Y87, or Y89, or S00-T79, or T89-T98).
#' 2. *`with`* Number of deaths with COVID-19 (coronavirus infection) of
#'    any cause of death.
#'    The indicator is calculated by summing all registered records of the
#'    medical form E106 (unique persons), in which the ICD disease codes
#'    U07.1, U07.2, U07.3, U07.4, U07.5 are indicated as the main, direct,
#'    intermediate cause of death or other important pathological condition,
#'    or identified as related to COVID-19 disease (coronavirus infection).
#'    Deaths due to external causes are not included (ICD disease codes
#'    are V00-Y36, or Y85-Y87, or Y89, or S00-T79, or T89-T98).
#' 3. *`after`* Number of deaths from any cause of COVID-19 or COVID-19
#'    deaths due to non-external causes within 28 days.
#'    The indicator is calculated by summing all registered records of the
#'    medical form E106 (unique persons), in which the ICD disease codes
#'    U07.1, U07.2, U07.3, U07.4, U07 are indicated as the main, direct,
#'    intermediate cause of death or other important pathological condition,
#'    or identified as related to COVID-19 disease (coronavirus infection)
#'    and all records of medical form E106 (unique individuals) where the
#'    person died within the last 28 days after receiving a positive
#'    diagnostic response to the SARS-CoV-2 test or had an entry in medical
#'    form E025 with ICD disease code U07.2 or U07.1. Deaths due to external
#'    causes are not included (ICD disease codes are V00-Y36, or Y85-Y87, or
#'    Y89, or S00-T79, or T89-T98).
#'
#'  The number of deaths reported in the last day is preliminary and
#'  increases by about 20-40% in a few days. Such a "delay" in the data is
#'  natural: for example, for those who died last night, a death certificate
#'  is likely to be issued as soon as this report is published this morning.
#'
#' @section De jure and statistical counts:
#'
#' Beginning in February 2021 the OSP makes statistical estimates
#' of the number of recovered and active cases, since review of the data
#' showed that some cases individuals still considered as active cases
#' had recovered, but not documented or registered as such.
#'
#' These are listed as by the OSP as `active_de_jure` and
#' `recovered_de_jure` (officially still considered sick),
#' and `active_sttstcl` and `recovered_sttstcl` (an estimate of how
#' many of these are still ill).
#'
#'
#' @seealso [get_lithuania_regional_cases_only_level_1()]
#' 
#' @md
#' 
#' @importFrom dplyr %>% filter select mutate full_join left_join rename bind_rows
#' @importFrom lubridate as_date ymd
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @importFrom tidyselect last_col
#' 
get_lithuania_regional_cases_with_level_2 <-
  function(national_data = FALSE,
           all_osp_fields = FALSE,
           death_definition = "of",
           recovered_definition = "official") {
    # The following code, adjusted from a version for France, was initially
    # used to create lookup tables of Lithuanian municipality and country
    # codes.
    # These were then adjusted to match the format used by the
    # Official Statistics Portal in their open data and are left as
    # hard-coded tibbles. These codes have not changed in ten years.
    # level_2_codes_url <- "https://en.wikipedia.org/wiki/ISO_3166-2:LT"
    # level_2_codes_table <- level_2_codes_url %>%
    #   xml2::read_html() %>%
    #   rvest::html_nodes(xpath = '//*[@id=\"mw-content-text\"]/div/table') %>%
    #   rvest::html_table(fill = TRUE)

    region_lookup_table <- tibble::tribble(
      ~region_level_2, ~level_2_region_code, ~region_level_1, ~level_1_region_code, ~region_level_1_en, ~region_level_2_type,
      "Alytaus m. sav.", "LT-02", "Alytaus apskritis", "LT-AL", "Alytus County", "city municipality",
      "Alytaus r. sav.", "LT-03", "Alytaus apskritis", "LT-AL", "Alytus County", "district municipality",
      "Druskinink\u0173 sav.", "LT-07", "Alytaus apskritis", "LT-AL", "Alytus County", "municipality",
      "Lazdij\u0173 r. sav.", "LT-24", "Alytaus apskritis", "LT-AL", "Alytus County", "district municipality",
      "Var\u0117nos r. sav.", "LT-55", "Alytaus apskritis", "LT-AL", "Alytus County", "district municipality",
      "Bir\u0161tono sav.", "LT-05", "Kauno apskritis", "LT-KU", "Kaunas County", "municipality",
      "Jonavos r. sav.", "LT-10", "Kauno apskritis", "LT-KU", "Kaunas County", "district municipality",
      "Kai\u0161iadori\u0173 r. sav.", "LT-13", "Kauno apskritis", "LT-KU", "Kaunas County", "district municipality",
      "Kauno r. sav.", "LT-16", "Kauno apskritis", "LT-KU", "Kaunas County", "district municipality",
      "Kauno m. sav.", "LT-15", "Kauno apskritis", "LT-KU", "Kaunas County", "city municipality",
      "K\u0117daini\u0173 r. sav.", "LT-18", "Kauno apskritis", "LT-KU", "Kaunas County", "district municipality",
      "Prien\u0173 r. sav.", "LT-36", "Kauno apskritis", "LT-KU", "Kaunas County", "district municipality",
      "Raseini\u0173 r. sav.", "LT-38", "Kauno apskritis", "LT-KU", "Kaunas County", "district municipality",
      "Klaip\u0117dos r. sav.", "LT-21", "Klaip\u0117dos apskritis", "LT-KL", "Klaip\u0117da County", "district municipality",
      "Klaip\u0117dos m. sav.", "LT-20", "Klaip\u0117dos apskritis", "LT-KL", "Klaip\u0117da County", "city municipality",
      "Kretingos r. sav.", "LT-22", "Klaip\u0117dos apskritis", "LT-KL", "Klaip\u0117da County", "district municipality",
      "Neringos sav.", "LT-28", "Klaip\u0117dos apskritis", "LT-KL", "Klaip\u0117da County", "municipality",
      "Palangos m. sav.", "LT-31", "Klaip\u0117dos apskritis", "LT-KL", "Klaip\u0117da County", "city municipality",
      "\u0160ilut\u0117s r. sav.", "LT-46", "Klaip\u0117dos apskritis", "LT-KL", "Klaip\u0117da County", "district municipality",
      "Skuodo r. sav.", "LT-48", "Klaip\u0117dos apskritis", "LT-KL", "Klaip\u0117da County", "district municipality",
      "Kalvarijos sav.", "LT-14", "Marijampol\u0117s apskritis", "LT-MR", "Marijampol\u0117 County", "municipality",
      "Kazl\u0173 R\u016bdos sav.", "LT-17", "Marijampol\u0117s apskritis", "LT-MR", "Marijampol\u0117 County", "municipality",
      "Marijampol\u0117s sav.", "LT-25", "Marijampol\u0117s apskritis", "LT-MR", "Marijampol\u0117 County", "district municipality",
      "\u0160aki\u0173 r. sav.", "LT-41", "Marijampol\u0117s apskritis", "LT-MR", "Marijampol\u0117 County", "district municipality",
      "Vilkavi\u0161kio r. sav.", "LT-56", "Marijampol\u0117s apskritis", "LT-MR", "Marijampol\u0117 County", "district municipality",
      "Bir\u017e\u0173 r. sav.", "LT-06", "Panev\u0117\u017eio apskritis", "LT-PN", "Panev\u0117\u017eys County", "district municipality",
      "Kupi\u0161kio r. sav.", "LT-23", "Panev\u0117\u017eio apskritis", "LT-PN", "Panev\u0117\u017eys County", "district municipality",
      "Panev\u0117\u017eio m. sav.", "LT-32", "Panev\u0117\u017eio apskritis", "LT-PN", "Panev\u0117\u017eys County", "city municipality",
      "Panev\u0117\u017eio r. sav.", "LT-33", "Panev\u0117\u017eio apskritis", "LT-PN", "Panev\u0117\u017eys County", "district municipality",
      "Pasvalio r. sav.", "LT-34", "Panev\u0117\u017eio apskritis", "LT-PN", "Panev\u0117\u017eys County", "district municipality",
      "Roki\u0161kio r. sav.", "LT-40", "Panev\u0117\u017eio apskritis", "LT-PN", "Panev\u0117\u017eys County", "district municipality",
      "Akmen\u0117s r. sav.", "LT-01", "\u0160iauli\u0173 apskritis", "LT-SA", "\u0160iauliai County", "district municipality",
      "Joni\u0161kio r. sav.", "LT-11", "\u0160iauli\u0173 apskritis", "LT-SA", "\u0160iauliai County", "district municipality",
      "Kelm\u0117s r. sav.", "LT-19", "\u0160iauli\u0173 apskritis", "LT-SA", "\u0160iauliai County", "district municipality",
      "Pakruojo r. sav.", "LT-30", "\u0160iauli\u0173 apskritis", "LT-SA", "\u0160iauliai County", "district municipality",
      "Radvili\u0161kio r. sav.", "LT-37", "\u0160iauli\u0173 apskritis", "LT-SA", "\u0160iauliai County", "district municipality",
      "\u0160iauli\u0173 r. sav.", "LT-44", "\u0160iauli\u0173 apskritis", "LT-SA", "\u0160iauliai County", "district municipality",
      "\u0160iauli\u0173 m. sav.", "LT-43", "\u0160iauli\u0173 apskritis", "LT-SA", "\u0160iauliai County", "city municipality",
      "Jurbarko r. sav.", "LT-12", "Taurag\u0117s apskritis", "LT-TA", "Taurag\u0117 County", "district municipality",
      "Pag\u0117gi\u0173 sav.", "LT-29", "Taurag\u0117s apskritis", "LT-TA", "Taurag\u0117 County", "municipality",
      "\u0160ilal\u0117s r. sav.", "LT-45", "Taurag\u0117s apskritis", "LT-TA", "Taurag\u0117 County", "district municipality",
      "Taurag\u0117s r. sav.", "LT-50", "Taurag\u0117s apskritis", "LT-TA", "Taurag\u0117 County", "district municipality",
      "Ma\u017eeiki\u0173 r. sav.", "LT-26", "Tel\u0161i\u0173 apskritis", "LT-TE", "Tel\u0161iai County", "district municipality",
      "Plung\u0117s r. sav.", "LT-35", "Tel\u0161i\u0173 apskritis", "LT-TE", "Tel\u0161iai County", "district municipality",
      "Rietavo sav.", "LT-39", "Tel\u0161i\u0173 apskritis", "LT-TE", "Tel\u0161iai County", "municipality",
      "Tel\u0161i\u0173 r. sav.", "LT-51", "Tel\u0161i\u0173 apskritis", "LT-TE", "Tel\u0161iai County", "district municipality",
      "Anyk\u0161\u010di\u0173 r. sav.", "LT-04", "Utenos apskritis", "LT-UT", "Utena County", "district municipality",
      "Ignalinos r. sav.", "LT-09", "Utenos apskritis", "LT-UT", "Utena County", "district municipality",
      "Mol\u0117t\u0173 r. sav.", "LT-27", "Utenos apskritis", "LT-UT", "Utena County", "district municipality",
      "Utenos r. sav.", "LT-54", "Utenos apskritis", "LT-UT", "Utena County", "district municipality",
      "Visagino sav.", "LT-59", "Utenos apskritis", "LT-UT", "Utena County", "municipality",
      "Zaras\u0173 r. sav.", "LT-60", "Utenos apskritis", "LT-UT", "Utena County", "district municipality",
      "Elektr\u0117n\u0173 sav.", "LT-08", "Vilniaus apskritis", "LT-VL", "Vilnius County", "municipality",
      "\u0160al\u010dinink\u0173 r. sav.", "LT-42", "Vilniaus apskritis", "LT-VL", "Vilnius County", "district municipality",
      "\u0160irvint\u0173 r. sav.", "LT-47", "Vilniaus apskritis", "LT-VL", "Vilnius County", "district municipality",
      "\u0160ven\u010dioni\u0173 r. sav.", "LT-49", "Vilniaus apskritis", "LT-VL", "Vilnius County", "district municipality",
      "Trak\u0173 r. sav.", "LT-52", "Vilniaus apskritis", "LT-VL", "Vilnius County", "district municipality",
      "Ukmerg\u0117s r. sav.", "LT-53", "Vilniaus apskritis", "LT-VL", "Vilnius County", "district municipality",
      "Vilniaus m. sav.", "LT-57", "Vilniaus apskritis", "LT-VL", "Vilnius County", "city municipality",
      "Vilniaus r. sav.", "LT-58", "Vilniaus apskritis", "LT-VL", "Vilnius County", "district municipality",
      "Unknown", NA, "Unknown", NA, NA, NA
    )

    # Advertise the fine documentation.
    message("Use ?get_lithuania_regional_cases_with_level_2() for more information on this dataset")

    # Read data --------------------------------------------------------------------
    cases_url <- "https://opendata.arcgis.com/datasets/d49a63c934be4f65a93b6273785a8449_0.csv"
    osp_data <- csv_reader(file = cases_url)

    # Process two params which let us switch what OSP fields are returned
    # for the number of deaths and the number of recovered cases.
    #
    # death_definition : default is "of"
    death_field <- switch(death_definition,
      of = "daily_deaths_def1",
      daily_deaths_def1 = "daily_deaths_def1",
      with = "daily_deaths_def2",
      daily_deaths_def2 = "daily_deaths_def2",
      after = "daily_deaths_def3",
      daily_deaths_def3 = "daily_deaths_def3",
      having_had = "daily_deaths_def3"
    )
    # If death_definition doesn't match one of our possibilities, it will
    # return NULL
    #
    if (is.null(death_field)) {
      message(paste0(
        "death_definition of \"", death_definition,
        "\" not recognised, defaulting to \"of\""
      ))
      death_field <- "daily_deaths_def1"
    }

    # recovered_definition : default is "official"
    recovered_field <- switch(recovered_definition,
      official = "recovered_de_jure",
      recovered_de_jure = "recovered_de_jure",
      de_jure = "recovered_de_jure",
      recovered_sttstcl = "recovered_sttstcl",
      statistical = "recovered_sttstcl",
      estimated = "recovered_sttstcl"
    )
    # If recovered_definition doesn't match one of our possibilities, it will
    # return NULL
    #
    if (is.null(death_field)) {
      message(paste0(
        "recovered_definition of \"", recovered_definition,
        "\" not recognised, defaulting to \"official\""
      ))
      recovered_field <- "recovered_de_jure"
    }

    # Build "unassigned" data by subtracting the aggregate from the countrywide
    # total provided for Lietuva

    # Get relevant column names for differences (i.e. not percentages
    # or qualitative)
    sum_cols <- names(select(osp_data, "population":tidyselect::last_col()))
    sum_cols <- sum_cols[!grepl("prc|map_colors", sum_cols)]

    # Take the difference between national and sum of counties' data
    unassigned <- osp_data %>%
      dplyr::mutate(national = ifelse(.data$municipality_name == "Lietuva", "national", "municipality")) %>%
      dplyr::group_by(date, .data$national) %>%
      dplyr::summarise(across(tidyselect::all_of(sum_cols), ~ sum(.x, na.rm = TRUE))) %>%
      dplyr::mutate(across(tidyselect::all_of(sum_cols), ~ dplyr::lead(.x, 1) - .x),
        municipality_name = "Unknown",
        ab_prc_day =
          dplyr::if_else(
            .data$ab_tot_day == 0, 0,
            .data$ab_pos_day / .data$ab_tot_day
          ),
        ag_prc_day =
          dplyr::if_else(
            .data$ag_tot_day == 0, 0,
            .data$ag_pos_day / .data$ag_tot_day
          ),
        pcr_prc_day =
          dplyr::if_else(
            .data$pcr_tot_day == 0, 0,
            .data$pcr_pos_day / .data$pcr_tot_day
          ),
        dgn_prc_day =
          dplyr::if_else(
            .data$dgn_tot_day == 0, 0,
            .data$dgn_pos_day / .data$dgn_tot_day
          ),
        map_colors = NA_character_
      ) %>%
      dplyr::filter(.data$national == "municipality") %>%
      dplyr::select(-.data$national)

    # Join unknown locations to main dataset
    osp_data_w_unassigned <- dplyr::bind_rows(
      osp_data %>% select(-.data$object_id, -.data$municipality_code), unassigned
    )

    # Exclude national data based on user param (default = FALSE)
    if (!national_data) {
      osp_data_w_unassigned <-
        dplyr::filter(
          osp_data_w_unassigned,
          !.data$municipality_name == "Lietuva"
        )
    }

    cases_data <- osp_data_w_unassigned %>%
      dplyr::mutate(
        date = lubridate::as_date(date),
        tested_new = .data$ab_tot_day + .data$ag_tot_day + .data$pcr_tot_day,
        deaths_new = .data[[death_field]],
        recovered_total = .data[[recovered_field]]
      ) %>%
      dplyr::rename(
        cases_new = .data$incidence,
        cases_total = .data$cumulative_totals,
        region_level_2 = .data$municipality_name
      ) %>%
      dplyr::left_join(region_lookup_table, by = c("region_level_2")) %>%
      dplyr::select(
        date, region_level_1, region_level_2,
        level_1_region_code, level_2_region_code, 
        cases_new, cases_total, deaths_new,
        tested_new, recovered_total,
        dplyr::everything()
      )
    # If we have not been asked to return all the OSP-provided data,
    # just select the core data sought by get_regional_data
    # (default = FALSE)
    if (!all_osp_fields) {
      cases_data <- cases_data %>%
        dplyr::select(
          date, region_level_1, region_level_2,
          level_1_region_code, level_2_region_code,
          cases_new, cases_total, deaths_new, tested_new, recovered_total
        )
    }
    ## This is the list of fields which we're trying to generate, copied from get_regional_data.R
    # date, region_level_2, level_2_region_code, region_level_1, level_1_region_code,
    # cases_new, cases_total, deaths_new, deaths_total,
    # recovered_new, recovered_total, hosp_new, hosp_total,
    # tested_new, tested_total, dplyr::everything())
    return(cases_data)
  }
