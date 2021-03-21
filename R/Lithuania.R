#' R6 Class containing country specific attributes and methods
#'
#' @description Country specific information for downloading, cleaning
#'  and processing covid-19 region data for an example Country.
#'
#' @details Inherits from `DataClass`
#' @source https://opendata.arcgis.com/datasets/d49a63c934be4f65a93b6273785a8449_0
#' @examples
#' \dontrun{
#' region <- Lithuania$new(verbose = TRUE, steps = TRUE)
#' region$download()
#' region$clean()
#' region$process()
#' region$return()
#' }
Lithuania <- R6::R6Class("Lithuania",
  inherit = DataClass,
  public = list(

    # Core Attributes
    #' @field level_1_region the level 1 region name.
    level_1_region = "county", # apskritis
    #' @field level_1_region the level 1 region name.
    level_2_region = "municipality", # savivaldybe
    #' @field data_url link to raw data
    data_url = "https://opendata.arcgis.com/datasets/d49a63c934be4f65a93b6273785a8449_0.csv", # nolint
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c(
      "cases_new", "tested_new", "recovered_total",
      "deaths_new"
    ),

    #' @description *Lithuania* specific state level data cleaning
    #' @param ... pass additional arguments
    #'
    clean = function(...) {
      # function to clean the data (MUST BE CALLED clean)
      # modify the data variable 'region' in place and add using 'self'
      # e.g. self$region$clean <- something
      # No return statement is required
      # have a statement like this to indicate information to user if requested
      if (self$verbose) {
        message("Cleaning data")
      }

      ## TODO: put in params
      # - currently with the defaults and the logic transferred across

      death_definition <- "of"
      recovered_definition <- "official"
      all_osp_fields <- FALSE
      national_data <- FALSE

      # region_lookup_table is used to provide ISO 3166-2 codes for
      # municipalities and counties, but also to provide the hierarchical link
      # between municipalities and counties, telling the code which
      # municipalities are contained within which counties.
      # region_level_2 is written to match the data from the raw data source
      # nolint start
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
      # nolint end

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

      # Build "unassigned" data by subtracting the aggregate from the
      # countrywide total provided for Lietuva

      # Get relevant column names for differences (i.e. not percentages
      # or qualitative)
      sum_cols <- names(select(osp_data, "population":tidyselect::last_col()))
      sum_cols <- sum_cols[!grepl("prc|map_colors", sum_cols)]

      # Take the difference between national and sum of counties' data
      unassigned <- self$region$raw %>%
        dplyr::mutate(national = ifelse(.data$municipality_name == "Lietuva",
          "national", "municipality"
        )) %>%
        dplyr::group_by(date, .data$national) %>%
        dplyr::summarise(across(
          tidyselect::all_of(sum_cols),
          ~ sum(.x, na.rm = TRUE)
        )) %>%
        dplyr::mutate(across(
          tidyselect::all_of(sum_cols),
          ~ dplyr::lead(.x, 1) - .x
        ),
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
        osp_data %>% select(-.data$object_id, -.data$municipality_code),
        unassigned
      )

      # Exclude national data based on user param (default = FALSE)
      if (!national_data) {
        osp_data_w_unassigned <-
          dplyr::filter(
            osp_data_w_unassigned,
            !.data$municipality_name == "Lietuva"
          )
      }

      self$region$clean <- osp_data_w_unassigned %>%
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
          cases_new, cases_total, deaths_new,
          tested_new, recovered_total,
          dplyr::everything()
        )
      # If we have not been asked to return all the OSP-provided data,
      # just select the core data sought by get_regional_data
      # (default = FALSE)
      if (!all_osp_fields) {
        self$region$clean <- self$region$clean %>%
          dplyr::select(
            date, region_level_1, region_level_2,
            level_1_region_code, level_2_region_code,
            cases_new, cases_total, deaths_new, tested_new, recovered_total
          )
      }
      if (self$level == "1") {
        self$clean_level_1()
      } else if (self$level == "2") {
        self$clean_level_2()
      }
    },

    #' @description Lithuania Specific County Level Data Cleaning
    #' @importFrom dplyr group_by summarise ungroup full_join
    clean_level_1 = function() {
      self$region$clean <- self$region$clean %>%
        dplyr::group_by(.data$date, .data$region_level_1) %>%
        dplyr::summarise(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric), sum)) %>%
        dplyr::mutate(region_level_1 = if_else(is.na(.data$region_level_1),
          "Lietuva", .data$region_level_1
        )) %>%
        dplyr::ungroup()
      # Fix percentages, where necessary
      if (all_osp_fields) {
        # For each of the test percentage fields, recalculate the percentages,
        # first checking that the total number of checks is not zero.
        self$region$clean <- self$region$clean %>%
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
    },

    #' @description Lithuania Specific Municipality Level Data Cleaning
    #' @importFrom dplyr mutate group_by summarise ungroup full_join
    #'
    clean_level_2 = function() {
    },

    #' @description Initialize the country
    #' @param ... The args passed by [general_init]
    initialize = function(...) {
      general_init(self, ...)
      # Add custom fields here
    }
  )
)
