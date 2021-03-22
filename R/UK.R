#' United Kingdom Class for downloading, cleaning and processing notification
#' data.
#'
#' @description Country specific information for downloading, cleaning
#'  and processing covid-19 region data for an example Country.
#'
#' @details Inherits from `DataClass`
#' @source https://coronavirus.data.gov.uk/details/download #nolint
#' @examples
#' \dontrun{
#' region <- Italy$new(verbose = TRUE, steps = TRUE)
#' region$download()
#' region$clean()
#' region$process()
#' region$return()
#' }
Uk <- R6::R6Class("Uk", # rename to country name
  inherit = DataClass,
  public = list(
    # Core Attributes (amend each paramater for country specific infomation)
    #' @field level_1_region the level 1 region name.
    level_1_region = "region", # add more levels as needed
    #' @field level_2_region the level 2 region name
    level_2_region = "authority",
    #' @field data_url link to raw data
    data_url = "https://api.coronavirus.data.gov.uk/v2/data",
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = list(
      # Cases by date of specimen
      "newCasesBySpecimenDate", "cumCasesBySpecimenDate",
      # Cases by date of report
      "newCasesByPublishDate", "cumCasesByPublishDate",
      # deaths by date of report
      "newDeaths28DaysByPublishDate", "cumDeaths28DaysByPublishDate",
      # deaths by date of death
      "newDeaths28DaysByDeathDate", "cumDeaths28DaysByDeathDate",
      # Tests - all
      "newTestsByPublishDate", "cumTestsByPublishDate",
      # Hospital - admissions
      "newAdmissions", "cumAdmissions",
      #
      # --- Additional non-standard variables --- #
      # # # Hospital
      # "cumAdmissionsByAge", "covidOccupiedMVBeds",
      # "hospitalCases", "plannedCapacityByPublishDate",
      # Tests by pillar
      "newPillarOneTestsByPublishDate", "newPillarTwoTestsByPublishDate",
      "newPillarThreeTestsByPublishDate", "newPillarFourTestsByPublishDate"
    ),

    #' @description Specific function for getting region codes for UK .
    #' @rdname get_region_codes
    #' @importFrom rlang .data
    get_region_codes = function() {
      tar_level <- paste0("level_", self$level, "_region")
      tar_level_name <- self[[tar_level]]
      self$region <- list(country = self$country, level = tar_level_name)
    },

    download = function() {
      # set up filters
      private$set_filters()
      # build a list of download links as limited to 4 variables per request
      csv_links <- purrr::map(
        1:(ceiling(length(self$source_data_cols) / 4)),
        ~ paste0(
          self$data_url, "?", unlist(private$query_filters), "&",
          paste(paste0(
            "metric=",
            self$source_data_cols[(1 + 4 * (. - 1)):min(
              (4 * .), length(self$source_data_cols)
            )]
          ),
          collapse = "&"
          ),
          "&format=csv"
        )
      )
      # add in release data if defined
      print(self$release_date)
      if (!is.null(self$release_date)) {
        csv_links <- purrr::map(csv_links, ~ paste0(
          .,
          "&release=",
          self$release_date
        ))
      }
      # download and link all data into a single data frame
      safe_reader <- purrr::safely(csv_readr)
      csv <- purrr::map(csv_links, ~ safe_reader(.)[[1]])
      csv <- purrr::compact(csv)
      print(csv)
      csv <- purrr::reduce(csv, dplyr::full_join,
        by = c("date", "areaType", "areaCode", "areaName")
      )
      if (is.null(csv)) {
        stop("Data retrieval failed")
      }
      # add release date as variable if missing
      if (!is.null(self$release_date)) {
        csv <- dplyr::mutate(
          csv,
          release_date = as.Date(self$release_date)
        )
      }
      self$region$raw <- csv
    },

    #' @description UK specific cleaning, directs to level 1 or level 2
    #' @param ... pass additional arguments
    #'
    clean = function(...) {
      if (self$verbose) {
        message("Cleaning data")
      }
      if (self$level == "1") {
        self$clean_level_1()
      } else if (self$level == "2") {
        self$clean_level_2()
      }
    },

    clean_level_1 = function(nhsregions = FALSE, release_date = NULL) {
      self$region$clean <- dplyr::bind_rows(
        self$region$raw$nation, self$region$raw$region
      ) %>%
        dplyr::mutate(
          date = lubridate::ymd(date),
          # Cases and deaths by specimen date and date of death
          #   for all nations + regions
          cases_new = newCasesBySpecimenDate,
          cases_total = cumCasesBySpecimenDate,
          deaths_new = newDeaths28DaysByDeathDate,
          deaths_total = cumDeaths28DaysByDeathDate
        ) %>%
        # Hospitalisations and tested variables are only available for nations
        # (not regions)
        #   sub-national English regions are available in the NHS data below
        # (with arg nhsregions = TRUE)
        dplyr::rename(
          hosp_new = newAdmissions,
          hosp_total = cumAdmissions,
          tested_new = newTestsByPublishDate,
          tested_total = cumTestsByPublishDate,
          region_level_1 = areaName,
          level_1_region_code = areaCode
        )
      if (!is.null(release_date)) {
        self$region$clean <- dplyr::mutate(
          self$region$clean,
          release_date = release_date
        )
      }

      # get NHS data if requested
      if (nhsregions) {
        private$add_nhs_regions()
      }
    },


    #' @description Initialize the country
    #' @param ... The args passed by [general_init]
    initialize = function(...) {
      general_init(self, ...)
    }
  ),
  private = list(
    #' @field query_filters Set what filters to use to query the data
    query_filters = NULL,

    #' @description Set filters for UK data api query.
    #'
    set_filters = function(resolution = "ultra") {
      if (self$level == 1) {
        private$query_filters <- list(
          nation = "areaType=nation",
          region = "areaType=region"
        )
      } else if (self$level == 2) {
        resolution <- match.arg(resolution, choices = c("utla", "ltla"))
        private$query_filters <- list(
          paste("areaType", resolution, sep = "=")
        )
        names(private$query_filters) <- resolution
      } else {
        stop(paste("Uk data not supported for level", self$level))
      }
    },

    #' @description Add NHS data for level 1 regions
    #' Separate NHS data is available for "first" admissions, excluding
    #' readmissions. This is available for England + English regions only.
    #   See: https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/ # nolint
    #     Section 2, "2. Estimated new hospital cases"
    #'
    add_nhs_regions = function(release_date) {
      if (is.null(release_date)) {
        release_date <- Sys.Date() - 1
      }
      if (release_date < (Sys.Date() - 7)) {
        stop("Data by NHS regions is only available in archived form for the
             last 7 days")
      }
      message("Arranging data by NHS region. Also adding new variable:
              hosp_new_first_admissions. This is NHS data for first hospital
              admissions, which excludes readmissions. This is available for
              England and English regions only.")
      # Download NHS xlsx
      nhs_url <- paste0(
        "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/",
        lubridate::year(release_date), "/",
        ifelse(lubridate::month(release_date) < 10,
          paste0(0, lubridate::month(release_date)),
          lubridate::month(release_date)
        ),
        "/COVID-19-daily-admissions-and-beds-",
        gsub("-", "", as.character(release_date)),
        ".xlsx"
      )

      tmp <- file.path(tempdir(), "nhs.xlsx")
      download.file(nhs_url, destfile = tmp, mode = "wb")

      # Clean NHS data
      adm_new <- suppressMessages(
        readxl::read_excel(tmp,
          sheet = 1,
          range = readxl::cell_limits(c(28, 2), c(36, NA))
        ) %>%
          t()
      )
      colnames(adm_new) <- adm_new[1, ]
      adm_new <- adm_new[2:nrow(adm_new), ]
      adm_new <- adm_new %>%
        tibble::as_tibble() %>%
        dplyr::mutate(date = seq.Date(
          from = as.Date("2020-08-01"),
          by = 1,
          length.out = nrow(.)
        )) %>%
        tidyr::pivot_longer(-date,
          names_to = "region_level_1",
          values_to = "hosp_new_first_admissions"
        ) %>%
        dplyr::mutate(
          region_level_1 = ifelse(region_level_1 == "ENGLAND",
            "England", region_level_1
          ),
          hosp_new_first_admissions = as.numeric(hosp_new_first_admissions)
        )


      # Merge PHE data into NHS regions ---------------------------------------
      self$region$clean <- self$region$clean %>%
        dplyr::select(-level_1_region_code) %>%
        dplyr::mutate(
          region_level_1 = ifelse(
            region_level_1 == "East Midlands" | region_level_1 == "West Midlands", # nolint
            "Midlands",
            region_level_1
          ),
          region_level_1 = ifelse(
            region_level_1 == "Yorkshire and The Humber" | region_level_1 == "North East", # nolint
            "North East and Yorkshire", region_level_1
          )
        ) %>%
        dplyr::group_by(date, region_level_1) %>%
        dplyr::summarise(
          cases_new = sum(cases_new, na.rm = TRUE),
          cases_total = sum(cases_total, na.rm = TRUE),
          deaths_new = sum(deaths_new, na.rm = TRUE),
          deaths_total = sum(deaths_total, na.rm = TRUE),
          hosp_new = sum(hosp_new, na.rm = TRUE),
          hosp_total = sum(hosp_total, na.rm = TRUE),
          .groups = "drop"
        )

      # Merge PHE and NHS data
      self$region$clean <- dplyr::left_join(
        self$region$clean, adm_new,
        by = c("region_level_1", "date")
      ) %>%
        # Create a blended variable that uses "all" hospital admissions
        # (includes readmissions) for devolved nations and "first" hospital
        # admissions for England + English regions
        dplyr::mutate(
          hosp_new_blend = ifelse(
            region_level_1 %in% c("Wales", "Scotland", "Northern Ireland"),
            hosp_new, hosp_new_first_admissions
          ),
          level_1_region_code = NA,
          release_date = release_date
        )
    }
  )
)
