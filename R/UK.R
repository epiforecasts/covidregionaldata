#' United Kingdom Class for downloading, cleaning and processing notification
#' data.
#'
#' @description Extracts daily COVID-19 data for the UK, stratified by region
#' and nation. Contains additional options to other country class objects,
#' including options to return subnational English regions using NHS region
#' boundaries instead of PHE boundaries (nhsregions=TRUE), a release date to
#' download from (release_date) and a geographical resolution (resolution).
#'
#' @details Inherits from `DataClass`
#' @source https://coronavirus.data.gov.uk/details/download #nolint
#' @export
#' @examples
#' \dontrun{
#' region <- UK$new(level = "1", verbose = TRUE, steps = TRUE)
#' region$download()
#' region$clean()
#' region$process()
#' region$return()
#' }
UK <- R6::R6Class("UK", # rename to country name
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
    get_region_codes = function() {
      tar_level <- paste0("level_", self$level, "_region")
      tar_level_name <- self[[tar_level]]
      self$data <- list(country = self$country, level = tar_level_name)
      self$data$code <- "ons_region_code"
      self$data$codes_lookup <- NULL
    },

    #' @description UK specific download function
    #' @importFrom purrr map
    #' @importFrom dplyr bind_rows
    download = function() {
      # set up filters
      self$set_filters()
      message_verbose(self$verbose, "Downloading UK data.")
      self$data$raw <- map(self$query_filters, self$download_uk)

      if (self$level == "1") {
        # get NHS data if requested
        if (self$nhsregions) {
          self$download_nhs_regions()
        }
      } else if (self$level == "2") {
        self$download_authority_data()
      }
    },

    #' @description UK specific cleaning, directs to level 1 or level 2
    #'
    clean = function() {
      message_verbose(self$verbose, "Cleaning data")
      if (self$level == "1") {
        self$clean_level_1()
      } else if (self$level == "2") {
        self$clean_level_2()
      }
    },

    #' @description UK Specific Region Level Data Cleaning
    #' @importFrom dplyr bind_rows mutate rename %>%
    #' @importFrom lubridate ymd
    #' @importFrom rlang .data
    clean_level_1 = function() {
      self$data$clean <- bind_rows(
        self$data$raw$nation, self$data$raw$region
      )
      self$data$clean <- self$data$clean %>%
        mutate(
          date = ymd(.data$date),
          # Cases and deaths by specimen date and date of death
          #   for all nations + regions
          cases_new = .data$newCasesBySpecimenDate,
          cases_total = .data$cumCasesBySpecimenDate,
          deaths_new = .data$newDeaths28DaysByDeathDate,
          deaths_total = .data$cumDeaths28DaysByDeathDate
        ) %>%
        # Hospitalisations and tested variables are only available for nations
        # (not regions)
        #   sub-national English regions are available in the NHS data below
        # (with arg nhsregions = TRUE)
        rename(
          hosp_new = .data$newAdmissions,
          hosp_total = .data$cumAdmissions,
          tested_new = .data$newTestsByPublishDate,
          tested_total = .data$cumTestsByPublishDate,
          region_level_1 = .data$areaName,
          level_1_region_code = .data$areaCode
        )
      if (!is.null(self$release_date)) {
        self$data$clean <- mutate(
          self$data$clean,
          release_date <- self$release_date
        )
      }
      # get NHS data if requested
      if (self$nhsregions) {
        self$data$clean <- self$add_nhs_regions(self$data$clean, self$nhs_raw)
      }
    },

    #' @description UK Specific Level 2 Data Cleaning
    #' @importFrom dplyr mutate rename left_join select %>%
    #' @importFrom lubridate ymd
    #' @importFrom stringr str_detect
    #' @importFrom rlang .data
    clean_level_2 = function() {
      self$get_authority_lookup_table()
      self$data$clean <- self$data$raw[[1]] %>%
        mutate(
          date = ymd(.data$date),
          # Cases and deaths are by publish date for Scotland, Wales;
          #   but by specimen date and date of death for England and NI
          cases_new = ifelse(str_detect(.data$areaCode, "^[EN]"),
            .data$newCasesBySpecimenDate,
            .data$newCasesByPublishDate
          ),
          cases_total = ifelse(str_detect(.data$areaCode, "^[EN]"),
            .data$cumCasesBySpecimenDate,
            .data$cumCasesByPublishDate
          ),
          deaths_new = ifelse(str_detect(.data$areaCode, "^[EN]"),
            .data$newDeaths28DaysByDeathDate,
            .data$newDeaths28DaysByPublishDate
          ),
          deaths_total = ifelse(str_detect(.data$areaCode, "^[EN]"),
            .data$cumDeaths28DaysByDeathDate,
            .data$cumDeaths28DaysByPublishDate
          )
        ) %>%
        # Hospitalisations and tested variables are consistent across nations
        rename(
          region_level_2 = .data$areaName,
          level_2_region_code = .data$areaCode
        ) %>%
        # Join local authority codes to level 1 regions
        left_join(self$authority_lookup_table,
          by = "region_level_2"
        ) %>%
        rename(level_2_region_code = .data$level_2_region_code.x) %>%
        select(-.data$level_2_region_code.y) %>%
        mutate(
          region_level_1 = ifelse(grepl("^W", .data$level_2_region_code),
            "Wales",
            ifelse(grepl("^S", .data$level_2_region_code),
              "Scotland",
              ifelse(grepl("^N", .data$level_2_region_code),
                "Northern Ireland", .data$region_level_1
              )
            )
          ),
          level_1_region_code = ifelse(
            .data$region_level_1 == "Scotland", "S92000003",
            ifelse(.data$region_level_1 == "Wales", "W92000004",
              ifelse(.data$region_level_1 == "Northern Ireland",
                "N92000002", .data$level_1_region_code
              )
            )
          )
        )

      if (!is.null(self$release_date)) {
        self$data$clean <- dplyr::mutate(self$data$clean,
          release_date = self$release_date
        )
      }
    },

    #' @description UK specific function to processes regional data.
    #' Calls the parent process function but for level two regions does some
    #' column renaming.
    #' @importFrom dplyr rename %>%
    process = function() {
      # run through parent process
      super$process()
      # rename the region codes columuns for level 2
      if (self$level == "2") {
        self$data$processed <- self$data$processed %>%
          rename(
            ltla_code = ons_region_code,
            ons_region_code = level_1_region_code,
            region = region_level_1
          )
      }
    },

    #' @description Specific initalize function for UK providing extra
    #' arguments specific to the UK
    #' @param self The specific class object to attach values
    #' @param level A character string indicating the target administrative
    #' level of the data with the default being "1". Currently supported
    #' options are level 1 ("1) and level 2 ("2").
    #' Use `get_available_datasets` for supported options by dataset.
    #' @param totals Logical, defaults to FALSE. If TRUE, returns totalled
    #'  data per region up to today's date. If FALSE, returns the full dataset
    #'  stratified by date and region.
    #' @param localise Logical, defaults to TRUE. Should region names be
    #' localised.
    #' @param verbose Logical, defaults to `TRUE`. Should verbose processing
    #' messages and warnings be returned.
    #' @param steps Logical, defaults to FALSE. Should all processing and
    #' cleaning steps be kept and output in a list.
    #' @export
    #' @param nhsregions Return subnational English regions using NHS region
    #' boundaries instead of PHE boundaries.
    #' @param release_date Date data was released. Default is to extract
    #' latest release. Dates should be in the format "yyyy-mm-dd".
    #' @param resolution "utla" (default) or "ltla", depending on which
    #' geographical resolution is preferred
    #' @examples
    #' \dontrun{
    #' Uk$new(
    #'  level = 1, localise = TRUE,
    #'  verbose = True, steps = FALSE,
    #'  nhsregions = FALSE, release_date = NULL,
    #'  resolution = "utla"
    #' )
    #' }
    initialize = function(level = "1",
                          totals = FALSE, localise = TRUE,
                          verbose = TRUE, steps = FALSE,
                          nhsregions = FALSE, release_date = NULL,
                          resolution = "utla") {
      self$level <- level
      self$totals <- totals
      self$localise <- localise
      self$verbose <- verbose
      self$steps <- steps
      self$country <- tolower(class(self)[1])
      self$nhsregions <- nhsregions
      self$release_date <- release_date
      self$resolution <- resolution
      self$get_region_codes()
    },

    #' @field query_filters Set what filters to use to query the data
    query_filters = NA,
    #' @field authority_lookup_table Get table of authority structures
    authority_lookup_table = NA,
    #' @field nhsregions Whether to include NHS regions in the data
    nhsregions = FALSE,
    #' @field release_date The release date for the data
    release_date = NA,
    #' @field resolution The resolution of the data to return
    resolution = "utla",
    #' @field nhs_raw Raw NHS region data
    nhs_raw = NA,
    #' @field authority_data The raw data for creating authority lookup tables
    authority_data = NA,
    #' @field nhs_base_url Base url for nhs region data
    nhs_base_url = "https://www.england.nhs.uk/statistics", # nolint

    #' @description Helper function for downloading Uk data API
    #' @importFrom purrr map safely compact reduce
    #' @importFrom dplyr full_join mutate
    #' @param filter region filters
    download_uk = function(filter) {
      # build a list of download links as limited to 4 variables per request
      csv_links <- map(
        1:(ceiling(length(self$source_data_cols) / 4)),
        ~ paste0(
          self$data_url, "?", unlist(filter), "&",
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
      if (!is.null(self$release_date)) {
        csv_links <- map(csv_links, ~ paste0(
          .,
          "&release=",
          self$release_date
        ))
      }
      # download and link all data into a single data frame
      safe_reader <- safely(csv_reader)
      csv <- map(csv_links, ~ safe_reader(.)[[1]], verbose = self$verbose)
      csv <- compact(csv)
      csv <- reduce(csv, full_join,
        by = c("date", "areaType", "areaCode", "areaName")
      )
      if (is.null(csv)) {
        stop("Data retrieval failed")
      }
      # add release date as variable if missing
      if (!is.null(self$release_date)) {
        csv <- mutate(
          csv,
          release_date = as.Date(self$release_date)
        )
      }
      return(csv)
    },

    #' @description Set filters for UK data api query.
    set_filters = function() {
      if (self$level == 1) {
        self$query_filters <- list(
          nation = "areaType=nation",
          region = "areaType=region"
        )
      } else if (self$level == 2) {
        self$resolution <- match.arg(self$resolution,
          choices = c("utla", "ltla")
        )
        self$query_filters <- list(
          paste("areaType", self$resolution, sep = "=")
        )
        names(self$query_filters) <- self$resolution
      } else {
        stop(paste("UK data not supported for level", self$level))
      }
    },

    #' @description Download NHS data for level 1 regions
    #' Separate NHS data is available for "first" admissions, excluding
    #' readmissions. This is available for England + English regions only.
    #'   See: https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/ # nolint
    #'     Section 2, "2. Estimated new hospital cases"
    #' @source https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/ # nolint
    #' @importFrom lubridate year month
    #' @importFrom readxl read_excel cell_limits
    #' @importFrom dplyr %>%
    download_nhs_regions = function() {
      if (is.null(self$release_date)) {
        self$release_date <- Sys.Date() - 1
      }
      if (self$release_date < (Sys.Date() - 7)) {
        stop("Data by NHS regions is only available in archived form for the
             last 7 days")
      }
      message_verbose(
        self$verbose,
        "Arranging data by NHS region. Also adding new variable:
        hosp_new_first_admissions. This is NHS data for first hospital
        admissions, which excludes readmissions. This is available for
        England and English regions only."
      )
      nhs_url <- paste0(
        self$nhs_base_url,
        "/wp-content/uploads/sites/2/",
        year(self$release_date), "/",
        ifelse(month(self$release_date) < 10,
          paste0(0, month(self$release_date)),
          month(self$release_date)
        ),
        "/COVID-19-daily-admissions-and-beds-",
        gsub("-", "", as.character(self$release_date)),
        ".xlsx"
      )
      tmp <- file.path(tempdir(), "nhs.xlsx")
      download.file(nhs_url,
        destfile = tmp,
        mode = "wb", quiet = !(self$verbose)
      )
      self$nhs_raw <- suppressMessages(
        read_excel(tmp,
          sheet = 1,
          range = cell_limits(c(28, 2), c(36, NA))
        ) %>%
          t()
      )
      self$nhs_raw <- as.data.frame(self$nhs_raw)
    },

    #' @description Add NHS data for level 1 regions
    #' Separate NHS data is available for "first" admissions, excluding
    #' readmissions. This is available for England + English regions only.
    #'   See: https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/ # nolint
    #'     Section 2, "2. Estimated new hospital cases"
    #' @importFrom lubridate year month
    #' @importFrom readxl read_excel cell_limits
    #' @importFrom tibble as_tibble
    #' @importFrom dplyr mutate select %>% group_by summarise left_join
    #' @importFrom tidyr pivot_longer
    #' @param clean_data Cleaned UK covid-19 data
    #' @param nhs_data NHS region data
    add_nhs_regions = function(clean_data, nhs_data) {
      colnames(nhs_data) <- nhs_data[1, ]
      nhs_data <- nhs_data[2:nrow(nhs_data), ]
      nhs_data <- nhs_data %>%
        as_tibble() %>%
        mutate(date = seq.Date(
          from = as.Date("2020-08-01"),
          by = 1,
          length.out = nrow(.)
        )) %>%
        pivot_longer(-date,
          names_to = "region_level_1",
          values_to = "hosp_new_first_admissions"
        ) %>%
        mutate(
          region_level_1 = ifelse(region_level_1 == "ENGLAND",
            "England", region_level_1
          ),
          hosp_new_first_admissions = as.numeric(hosp_new_first_admissions)
        )

      # Merge PHE data into NHS regions ---------------------------------------
      clean_data <- clean_data %>%
        select(-.data$level_1_region_code) %>%
        mutate(
          region_level_1 = ifelse(
            .data$region_level_1 == "East Midlands" | .data$region_level_1 == "West Midlands", # nolint
            "Midlands",
            .data$region_level_1
          ),
          region_level_1 = ifelse(
            .data$region_level_1 == "Yorkshire and The Humber" | .data$region_level_1 == "North East", # nolint
            "North East and Yorkshire", .data$region_level_1
          )
        ) %>%
        group_by(date, .data$region_level_1) %>%
        summarise(
          cases_new = sum(.data$cases_new, na.rm = TRUE),
          cases_total = sum(.data$cases_total, na.rm = TRUE),
          deaths_new = sum(.data$deaths_new, na.rm = TRUE),
          deaths_total = sum(.data$deaths_total, na.rm = TRUE),
          hosp_new = sum(.data$hosp_new, na.rm = TRUE),
          hosp_total = sum(.data$hosp_total, na.rm = TRUE),
          .groups = "drop"
        )

      # Merge PHE and NHS data
      clean_data <- left_join(
        clean_data, nhs_data,
        by = c("region_level_1", "date")
      ) %>%
        # Create a blended variable that uses "all" hospital admissions
        # (includes readmissions) for devolved nations and "first" hospital
        # admissions for England + English regions
        mutate(
          hosp_new_blend = ifelse(
            .data$region_level_1 %in% c(
              "Wales",
              "Scotland",
              "Northern Ireland"
            ),
            .data$hosp_new, .data$hosp_new_first_admissions
          ),
          level_1_region_code = NA,
          release_date = self$release_date
        )
      return(clean_data)
    },

    #' @description Download lookup table for UK authorities
    #' @importFrom vroom vroom col_character
    download_authority_data = function() {
      self$authority_data <- vroom(
        "https://opendata.arcgis.com/datasets/72e57d3ab1054e169c55afff3c9c1aa4_0.csv", # nolint
        col_types = c(
          WD17NMW = col_character(),
          CTY17CD = col_character(),
          CTY17NM = col_character()
        )
      )
    },

    #' @description Get lookup table for UK authorities
    #' @importFrom vroom vroom col_character
    #' @importFrom dplyr select distinct filter bind_rows arrange
    #' @importFrom tidyr drop_na
    #' @importFrom tibble tibble
    get_authority_lookup_table = function() {
      unitary_auth <- self$authority_data %>%
        select(
          level_2_region_code = "CTY17CD",
          region_level_2 = "CTY17NM",
          level_1_region_code = "GOR10CD",
          region_level_1 = "GOR10NM"
        ) %>%
        distinct() %>%
        drop_na(.data$region_level_2)

      upper_tier_auth <- self$authority_data %>%
        select(
          level_2_region_code = "LAD17CD",
          region_level_2 = "LAD17NM",
          level_1_region_code = "GOR10CD",
          region_level_1 = "GOR10NM"
        ) %>%
        distinct() %>%
        drop_na(.data$region_level_2)

      country_auth <- self$authority_data %>%
        select(
          level_2_region_code = "LAD17CD",
          region_level_2 = "LAD17NM",
          level_1_region_code = "CTRY17CD",
          region_level_1 = "CTRY17NM"
        ) %>%
        filter(.data$region_level_1 %in% c(
          "Northern Ireland",
          "Scotland",
          "Wales"
        )) %>%
        distinct() %>%
        drop_na(.data$region_level_2)

      other_auths <- tibble(
        level_2_region_code = c("E06000058", "E06000052", "E09000012"),
        region_level_2 = c(
          "Bournemouth, Christchurch and Poole",
          "Cornwall and Isles of Scilly",
          "Hackney and City of London"
        ),
        level_1_region_code = c(rep("E92000001", 3)),
        region_level_1 = c("South West", "South West", "London")
      )

      # Join tables ---------------------------------------------------
      self$authority_lookup_table <- bind_rows(
        unitary_auth,
        upper_tier_auth,
        country_auth,
        other_auths
      )

      self$authority_lookup_table <- self$authority_lookup_table %>%
        arrange(.data$level_1_region_code) %>%
        distinct(.data$level_2_region_code,
          .data$region_level_2,
          .keep_all = TRUE
        )
    }
  )
)
