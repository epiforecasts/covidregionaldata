#' Lithuania Class for downloading, cleaning and processing notification data
#'
#' @description Information for downloading, cleaning
#'  and processing COVID-19 region level 1 and 2 data for Lithuania.
#'
#'
#' @section OSP Data fields:
#'
#' The [Official Statistics Portal](https://osp.stat.gov.lt) (OSP) provides
#' many data series in their table.
#'
#' The full range of these vectors can be returned by setting
#' `all_osp_fields` to `TRUE`.
#'
#' The following describes the data provided by the OSP.
#'
# nolint start
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
# nolint end
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
# nolint start
#' @source \url{https://hub.arcgis.com/datasets/d49a63c934be4f65a93b6273785a8449_0}
# nolint end
#' @examples
#' \dontrun{
#' region <- Lithuania$new(verbose = TRUE, steps = TRUE, get = TRUE)
#' }
#' @export
#' @family subnational
#' @concept dataset
Lithuania <- R6::R6Class("Lithuania",
  inherit = DataClass,
  public = list(

    # Core Attributes
    #' @field origin name of origin to fetch data for
    origin = "Lithuania",
    #' @field supported_levels A list of supported levels.
    supported_levels = list("1", "2"),
    #' @field supported_region_names A list of region names in order of level.
    supported_region_names = list(
      "1" = "county", # apskritis
      "2" = "municipality" # savivaldybe
    ),
    #' @field supported_region_codes A list of region codes in order of level.
    supported_region_codes = list(
      "1" = "iso_3166_2", # default is to just give county data
      "2" = "iso_3166_2_municipality" # if giving municipality data
    ),
    #' @field common_data_urls List of named links to raw data that are common
    #' across levels.
    # nolint start
    common_data_urls = list(
      "main" = "https://opendata.arcgis.com/datasets/d49a63c934be4f65a93b6273785a8449_0.csv"
    ),
    # nolint end
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c(
      "cases_new", "tested_new", "recovered_total", "deaths_new"
    ),
    #' @field source_text Plain text description of the source of the data
    source_text = "Lithuanian Statistics Department",
    #' @field source_url Website address for explanation/introduction of the
    #' data
    source_url = "https://hub.arcgis.com/datasets/d49a63c934be4f65a93b6273785a8449_0/about", # nolint

    # Additional attributes specific to the Lithuania functionality
    #' @field death_definition which criteria of deaths attributed to
    #' COVID to use
    death_definition = "of",
    #' @field recovered_definition whether to use the official counts of
    #' recovered cases or the statistical estimates provided by OSP
    recovered_definition = "official",
    #' @field all_osp_fields whether to return all the data vectors provided
    #' by OSP
    all_osp_fields = FALSE,
    #' @field national_data whether to return data rows for national results
    national_data = FALSE,

    #' @description Set up a table of region codes for clean data
    set_region_codes = function() {
      self$codes_lookup <- list(
        "1" = lithuania_codes,
        "2" = lithuania_codes
      )
    },

    #' @description Common data cleaning for both levels
    #'
    # nolint start
    #' @importFrom dplyr mutate group_by summarise if_else filter select bind_rows rename left_join everything across lead all_of
    #' @importFrom lubridate as_date
    # nolint end
    clean_common = function() {

      # Process two params which let us switch what OSP fields are returned
      # for the number of deaths and the number of recovered cases.
      #
      # death_definition : default is "of"
      death_field <- switch(self$death_definition,
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
        message_verbose(
          self$verbose,
          paste0(
            "death_definition of \"", self$death_definition,
            "\" not recognised, defaulting to \"of\""
          )
        )
        death_field <- "daily_deaths_def1"
      }

      # recovered_definition : default is "official"
      recovered_field <- switch(self$recovered_definition,
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
        message_verbose(self$verbose, paste0(
          "recovered_definition of \"", self$recovered_definition,
          "\" not recognised, defaulting to \"official\""
        ))
        recovered_field <- "recovered_de_jure"
      }

      # Build "unassigned" data by subtracting the aggregate from the
      # countrywide total provided for Lietuva

      # Get relevant column names for differences (i.e. not percentages
      # or qualitative)
      sum_cols <- names(select(
        self$data$raw$main,
        "population":dplyr::last_col()
      ))
      sum_cols <- sum_cols[!grepl("prc|map_colors", sum_cols)]

      # Take the difference between national and sum of counties' data
      unassigned <- self$data$raw$main %>%
        mutate(national = ifelse(.data$municipality_name == "Lietuva",
          "national", "municipality"
        )) %>%
        group_by(date, .data$national) %>%
        summarise(across(
          all_of(sum_cols),
          ~ sum(.x, na.rm = TRUE)
        )) %>%
        mutate(across(
          all_of(sum_cols),
          ~ lead(.x, 1) - .x
        ),
        municipality_name = "Unknown",
        ab_prc_day =
          if_else(
            .data$ab_tot_day == 0, 0,
            .data$ab_pos_day / .data$ab_tot_day
          ),
        ag_prc_day =
          if_else(
            .data$ag_tot_day == 0, 0,
            .data$ag_pos_day / .data$ag_tot_day
          ),
        pcr_prc_day =
          if_else(
            .data$pcr_tot_day == 0, 0,
            .data$pcr_pos_day / .data$pcr_tot_day
          ),
        dgn_prc_day =
          if_else(
            .data$dgn_tot_day == 0, 0,
            .data$dgn_pos_day / .data$dgn_tot_day
          ),
        map_colors = NA_character_
        ) %>%
        filter(.data$national == "municipality") %>%
        select(-.data$national)

      # Join unknown locations to main dataset
      osp_data_w_unassigned <- bind_rows(
        self$data$raw$main %>%
          select(-.data$object_id, -.data$municipality_code),
        unassigned
      )

      # Exclude national data based on user param (default = FALSE)
      if (!self$national_data) {
        osp_data_w_unassigned <-
          filter(
            osp_data_w_unassigned,
            !.data$municipality_name == "Lietuva"
          )
      }

      self$data$clean <- osp_data_w_unassigned %>%
        mutate(
          date = as_date(date),
          tested_new = .data$ab_tot_day + .data$ag_tot_day + .data$pcr_tot_day,
          deaths_new = .data[[death_field]],
          recovered_total = .data[[recovered_field]]
        ) %>%
        rename(
          cases_new = .data$incidence,
          cases_total = .data$cumulative_totals,
          level_2_region = .data$municipality_name
        ) %>%
        left_join(self$codes_lookup[["2"]],
          by = c("level_2_region"),
          copy = TRUE
        ) %>%
        select(
          date, level_1_region, level_2_region,
          cases_new, cases_total, deaths_new,
          tested_new, recovered_total,
          everything()
        )
      # If we have not been asked to return all the OSP-provided data,
      # just select the core data sought by get_regional_data
      # (default is FALSE)
      if (!self$all_osp_fields) {
        self$data$clean <- self$data$clean %>%
          select(
            date, level_1_region, level_2_region,
            level_1_region_code, level_2_region_code,
            cases_new, cases_total, deaths_new, tested_new, recovered_total
          )
      }
    },

    #' @description Lithuania Specific County Level Data Cleaning
    #'
    #' Aggregates data to the level 1 (county) regional level. Data is
    #' provided by the source at the level 2 (municipality) regional level.
    #'
    #' @importFrom dplyr group_by summarise ungroup full_join across if_else
    clean_level_1 = function() {
      self$data$clean <- self$data$clean %>%
        group_by(
          .data$date,
          .data$level_1_region, .data$level_1_region_code
        ) %>%
        summarise(
          across(
            where(is.numeric),
            sum
          )
        ) %>%
        mutate(
          level_1_region
          = if_else(is.na(.data$level_1_region),
            "Lietuva", .data$level_1_region
          )
        ) %>%
        ungroup()
      # Fix percentages, where necessary
      if (self$all_osp_fields) {
        # For each of the test percentage fields, recalculate the percentages,
        # first checking that the total number of checks is not zero.
        self$data$clean <- self$data$clean %>%
          mutate(
            ab_prc_day =
              if_else(
                .data$ab_tot_day == 0, 0,
                .data$ab_pos_day / .data$ab_tot_day
              ),
            ag_prc_day =
              if_else(
                .data$ag_tot_day == 0, 0,
                .data$ag_pos_day / .data$ag_tot_day
              ),
            pcr_prc_day =
              if_else(
                .data$pcr_tot_day == 0, 0,
                .data$pcr_pos_day / .data$pcr_tot_day
              ),
            dgn_prc_day =
              if_else(
                .data$dgn_tot_day == 0, 0,
                .data$dgn_pos_day / .data$dgn_tot_day
              )
          )
      }
    },

    #' @description Initialize the country
    #' @param death_definition A character string. Determines which criteria
    #'   for attributing deaths to COVID is used. Should be `"of"`,
    #'    `"with"`, or `"after"`. Can also be `"daily_deaths_def1"`,
    #'    `"daily_deaths_def2"`, or `"daily_deaths_def3"`. (Defaults
    #'    to `"of"`, the strictest definition.)
    #' @param recovered_definition A character string. Determines whether
    #'   the count of officially-recovered (*de jure*) cases is used, or
    #'   the statistical estimate provided by OSP. Should be `"official"`
    #'   or `"statistical"`. (Defaults to `"official"`.)
    #' @param national_data A logical scalar. Should national values be
    #'   returned?  (Defaults `FALSE`)
    #' @param all_osp_fields A logical scalar. Should all the meaningful
    #'   data fields from the OSP source be returned? (Defaults `FALSE`)
    #' @param ... Parameters passed to [DataClass()] initalize
    initialize = function(death_definition = "of",
                          recovered_definition = "official",
                          all_osp_fields = FALSE,
                          national_data = FALSE, ...) {
      self$death_definition <- death_definition
      self$recovered_definition <- recovered_definition
      self$all_osp_fields <- all_osp_fields
      self$national_data <- national_data
      super$initialize(...)
    }
  )
)
