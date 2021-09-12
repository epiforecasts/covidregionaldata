#' United Kingdom Class for downloading, cleaning and processing notification
#' data.
#'
#' @description Extracts daily COVID-19 data for the UK, stratified by region
#' and nation. Additional options for this class are: to return subnational
#' English regions using NHS region boundaries instead of PHE boundaries
#' (nhsregions = TRUE), a release date to download from (release_date) and a
#' geographical resolution (resolution).
#'
# nolint start
#' @source \url{https://coronavirus.data.gov.uk/details/download}
# nolint end
#' @export
#' @concept dataset
#' @family subnational
#' @examples
#' \dontrun{
#' # setup a data cache
#' start_using_memoise()
#'
#' # download, clean and process level 1 UK data with hospital admissions
#' region <- UK$new(level = "1", nhsregions = TRUE)
#' region$return()
#'
#' # initialise level 2 data
#' utla <- UK$new(level = "2")
#'
#' # download UTLA data
#' utla$download()
#'
#' # clean UTLA data
#' utla$clean()
#'
#' # inspect available level 1 regions
#' utla$available_regions(level = "1")
#'
#' # filter data to the East of England
#' utla$filter("East of England")
#'
#' # process UTLA data
#' utla$process()
#'
#' # return processed and filtered data
#' utla$return()
#'
#' # inspect all data steps
#' utla$data
#' }
UK <- R6::R6Class("UK",
  inherit = DataClass,
  public = list(
    # Core Attributes (amend each paramater for origin specific infomation)
    #' @field origin name of origin to fetch data for
    origin = "United Kingdom (UK)",
    #' @field supported_levels A list of supported levels.
    supported_levels = list("1", "2"),
    #' @field supported_region_names A list of region names in order of level.
    supported_region_names = list("1" = "region", "2" = "authority"),
    #' @field supported_region_codes A list of region codes in order of level.
    supported_region_codes = list(
      "1" = "region_code",
      "2" = "local_authority_code"
    ),
    #' @field common_data_urls List of named links to raw data. The first, and
    #' only entry, is be named main.
    common_data_urls = list(
      "main" = "https://api.coronavirus.data.gov.uk/v2/data"
    ),
    #' @field level_data_urls List of named links to raw data that are level
    #' specific.
    level_data_urls = list(
      "1" = list(
        # A dynamic URL for NHS data from 7 April 2021 to now
        "nhs_recent_url" = "https://www.england.nhs.uk/statistics",
        # A stable URL for data from August 2020 - April 06 2021
        # nolint start
        "nhs_archive_url" = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/04/COVID-19-daily-admissions-and-beds-20210406-1.xlsx"
      )
      # nolint end
    ),
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
    #' @field source_text Plain text description of the source of the data
    source_text = "Public Health England",
    #' @field source_url Website address for explanation/introduction of the
    #' data
    source_url = "https://coronavirus.data.gov.uk/",

    #' @description Specific function for getting region codes for UK .
    set_region_codes = function() {
      self$codes_lookup$`2` <- covidregionaldata::uk_codes
    },

    #' @description UK specific `download()` function.
    #' @importFrom purrr map
    #' @importFrom dplyr bind_rows
    download = function() {
      # set up filters
      self$set_filters()
      message_verbose(self$verbose, "Downloading UK data.")
      self$data$raw <- map(self$query_filters, self$download_filter)

      if (self$level == "1") {
        # get NHS data if requested
        if (self$nhsregions) {
          self$data$raw$nhs <- self$download_nhs_regions()
        }
      }
    },

    #' @description Region Level Data Cleaning
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
        # sub-national English regions are available in the NHS data below
        # (with arg nhsregions = TRUE)
        rename(
          hosp_new = .data$newAdmissions,
          hosp_total = .data$cumAdmissions,
          tested_new = .data$newTestsByPublishDate,
          tested_total = .data$cumTestsByPublishDate,
          level_1_region = .data$areaName,
          level_1_region_code = .data$areaCode
        )
      if (!is.null(self$release_date)) {
        self$data$clean <- mutate(
          self$data$clean,
          release_date = self$release_date
        )
      }
      # get NHS data if requested
      if (self$nhsregions) {
        self$data$clean <- self$add_nhs_regions(
          self$data$clean,
          self$data$raw$nhs
        )
      }
    },

    #' @description Level 2 Data Cleaning
    #' @importFrom dplyr mutate rename left_join select %>%
    #' @importFrom lubridate ymd
    #' @importFrom stringr str_detect
    #' @importFrom rlang .data
    clean_level_2 = function() {
      self$data$clean <- self$data$raw[["utla"]] %>%
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
          level_2_region = .data$areaName,
          level_2_region_code = .data$areaCode
        ) %>%
        # Join local authority codes to level 1 regions
        left_join(self$codes_lookup[["2"]],
          by = "level_2_region"
        ) %>%
        rename(level_2_region_code = .data$level_2_region_code.x) %>%
        select(-.data$level_2_region_code.y) %>%
        mutate(
          level_1_region = ifelse(grepl("^W", .data$level_2_region_code),
            "Wales",
            ifelse(grepl("^S", .data$level_2_region_code),
              "Scotland",
              ifelse(grepl("^N", .data$level_2_region_code),
                "Northern Ireland", .data$level_1_region
              )
            )
          ),
          level_1_region_code = ifelse(
            .data$level_1_region == "Scotland", "S92000003",
            ifelse(.data$level_1_region == "Wales", "W92000004",
              ifelse(.data$level_1_region == "Northern Ireland",
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

    #' @description Initalize the UK Class
    #' @export
    #' @param nhsregions Return subnational English regions using NHS region
    #' boundaries instead of PHE boundaries.
    #' @param release_date Date data was released. Default is to extract
    #' latest release. Dates should be in the format "yyyy-mm-dd".
    #' @param resolution "utla" (default) or "ltla", depending on which
    #' geographical resolution is preferred
    #' @param ... Optional arguments passed to [DataClass()] initalize.
    #' @examples
    #' \dontrun{
    #' UK$new(
    #'  level = 1, localise = TRUE,
    #'  verbose = True, steps = FALSE,
    #'  nhsregions = FALSE, release_date = NULL,
    #'  resolution = "utla"
    #' )
    #' }
    initialize = function(nhsregions = FALSE, release_date = NULL,
                          resolution = "utla", ...) {
      self$nhsregions <- nhsregions
      self$release_date <- release_date
      self$resolution <- resolution
      super$initialize(...)
    },

    #' @field query_filters Set what filters to use to query the data
    query_filters = NA,
    #' @field nhsregions Whether to include NHS regions in the data
    nhsregions = FALSE,
    #' @field release_date The release date for the data
    release_date = NA,
    #' @field resolution The resolution of the data to return
    resolution = "utla",
    #' @field authority_data The raw data for creating authority lookup tables
    authority_data = NA,

    #' @description Helper function for downloading data API
    #' @importFrom purrr map safely compact reduce
    #' @importFrom dplyr full_join mutate
    #' @param filter region filters
    download_filter = function(filter) {
      # build a list of download links as limited to 4 variables per request
      csv_links <- map(
        1:(ceiling(length(self$source_data_cols) / 4)),
        ~ paste0(
          self$data_urls[["main"]], "?", unlist(filter), "&",
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
      csv <- map(csv_links, ~ safe_reader(., verbose = self$verbose)[[1]])
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
      if (self$level == "1") {
        self$query_filters <- list(
          nation = "areaType=nation",
          region = "areaType=region"
        )
      } else if (self$level == "2") {
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
    #' Data are available separately for the periods 2020-08-01 to 2021-04-06,
    #' and 2021-04-07 - present.
    # nolint start
    #'   See: \url{https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/}
    # nolint end
    #'     Section 2, "2. Estimated new hospital cases"
    #' @return nhs data.frame of nhs regions
    # nolint start
    #' @source \url{https://coronavirus.data.gov.uk/details/download}
    # nolint end
    #' @importFrom lubridate year month
    #' @importFrom httr GET status_code
    #' @importFrom purrr map_chr
    #' @importFrom readxl cell_limits
    #' @importFrom dplyr %>%
    download_nhs_regions = function() {

      # Some general set up
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

      # 1. Data from 7 April 2021 to now:
      # Data not always daily; set up to try urls for last 7 days
      try_date_seq <- seq.Date(self$release_date,
        by = -1, length.out = 7
      )
      try_urls <- map_chr(
        try_date_seq,
        ~ paste0(
          self$data_urls[["nhs_recent_url"]],
          "/wp-content/uploads/sites/2/",
          year(.x), "/",
          ifelse(month(.x) < 10,
            paste0(0, month(.x)),
            month(.x)
          ),
          "/COVID-19-daily-admissions-and-beds-",
          gsub("-", "", as.character(.x)),
          ".xlsx"
        )
      )

      names(try_urls) <- try_date_seq
      # Check for working urls
      url_status <- map_chr(
        try_urls,
        ~ GET(.x) %>%
          status_code()
      )
      # Keep latest working url
      url_status <- url_status[(url_status == 200)]
      names(url_status) <- as.Date(names(url_status))
      nhs_recent_url <- try_urls[as.character(max(names(url_status)))]
      # Get latest url download
      recent <- download_excel(nhs_recent_url,
        "nhs_recent.xlsx",
        verbose = self$verbose,
        transpose = TRUE,
        sheet = 1,
        range = cell_limits(c(28, 2), c(36, NA))
      )

      # 2. Data for August 2020 to 7 April 2021
      archive <- download_excel(
        as.character(self$data_urls[["nhs_archive_url"]]),
        "nhs_archive.xlsx",
        verbose = self$verbose,
        transpose = TRUE,
        sheet = 1,
        range = cell_limits(c(28, 2), c(36, NA))
      )


      # 3. Join archive and recent data, still in raw format
      dt <- rbind(archive, recent)
      dt <- dt[which(!rownames(dt) == "Name1"), ]

      return(dt)
    },

    #' @description Add NHS data for level 1 regions
    #' Separate NHS data is available for "first" admissions, excluding
    #' readmissions. This is available for England + English regions only.
    # nolint start
    #'   See: \url{https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/}
    # nolint end
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
          names_to = "level_1_region",
          values_to = "hosp_new_first_admissions"
        ) %>%
        mutate(
          level_1_region = ifelse(level_1_region == "ENGLAND",
            "England", level_1_region
          ),
          hosp_new_first_admissions = as.numeric(hosp_new_first_admissions)
        )

      # Merge PHE data into NHS regions ---------------------------------------
      clean_data <- clean_data %>%
        select(-.data$level_1_region_code) %>%
        mutate(
          level_1_region = ifelse(
            .data$level_1_region == "East Midlands" | .data$level_1_region == "West Midlands", # nolint
            "Midlands",
            .data$level_1_region
          ),
          level_1_region = ifelse(
            .data$level_1_region == "Yorkshire and The Humber" | .data$level_1_region == "North East", # nolint
            "North East and Yorkshire", .data$level_1_region
          )
        ) %>%
        group_by(date, .data$level_1_region) %>%
        summarise(
          cases_new = sum(.data$cases_new),
          cases_total = sum(.data$cases_total),
          deaths_new = sum(.data$deaths_new),
          deaths_total = sum(.data$deaths_total),
          hosp_new = sum(.data$hosp_new),
          hosp_total = sum(.data$hosp_total),
          .groups = "drop"
        )

      # Merge PHE and NHS data
      clean_data <- left_join(
        clean_data, nhs_data,
        by = c("level_1_region", "date")
      ) %>%
        # Create a blended variable that uses "all" hospital admissions
        # (includes readmissions) for devolved nations and "first" hospital
        # admissions for England + English regions
        mutate(
          hosp_new_blend = ifelse(
            .data$level_1_region %in% c(
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

    #' @description Specific tests for UK data. In addition to generic tests ran
    #' by `DataClass$test()` data for NHS regions are downloaded and ran through
    #' the same generic checks (test_cleaning, test_processing, test_return). If
    #' download = TRUE or a snapshot file is not found, the nhs data is
    #' downloaded and saved to the snapshot location provided. If an existing
    #' snapshot file is found then this data is used in the next tests.
    #' Tests data can be downloaded, cleaned, processed and returned. Designed
    #' to be ran from `test` and not ran directly.
    #' @param self_copy R6class the object to test.
    #' @param download logical. To download the data (TRUE) or use a snapshot
    #' (FALSE). Defaults to FALSE.
    #' @param all logical. Run tests with all settings (TRUE) or with those
    #' defined in the current class instance (FALSE). Defaults to FALSE.
    #' @param snapshot_path character_array the path to save the downloaded
    #' snapshot to. Works on the snapshot path constructed by `test` but adds
    # '_nhs' to the end.
    #' @param ... Additional parameters to pass to `specific_tests`
    #' @importFrom dplyr slice_tail
    specific_tests = function(self_copy, download = FALSE,
                              all = FALSE, snapshot_path = "", ...) {
      if (all == TRUE) {
        if (self_copy$level == "1") {
          self_copy$data_name <- "UK level 1 with 'nhsregions=TRUE'"
          self_copy$nhsregions <- TRUE
          snapshot_path <- gsub(".rds", "_nhs.rds", snapshot_path)
          if (!file.exists(snapshot_path)) {
            download <- TRUE
          }
          if (download) {
            test_that(paste(self_copy$data_name, " downloads sucessfully"), { # nolint
              self_copy$data$raw$nhs <- self_copy$download_nhs_regions()
              expect_s3_class(self_copy$data$raw$nhs, "data.frame")
              expect_true(nrow(self_copy$data$raw$nhs) > 0)
              expect_true(ncol(self_copy$data$raw$nhs) >= 2)
            })
            self_copy$data$raw$nhs <- slice_tail(
              self_copy$data$raw$nhs,
              n = 1000
            )
            saveRDS(self_copy$data$raw$nhs, snapshot_path)
          } else {
            self_copy$data$raw$nhs <- readRDS(snapshot_path)
          }
          test_cleaning(DataClass_obj = self_copy)
          test_processing(DataClass_obj = self_copy)
          test_return(DataClass_obj = self_copy)
        }
      }
    }
  )
)
