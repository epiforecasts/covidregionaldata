#' Initialise a child class of DataClass if it exists
#'
#' @description This function initialises classes based on the `DataClass()`
#'  which allows documented downloading, cleaning, and processing. See the
#'  examples for some potential use cases and the `DataClass()` documentation
#'  for more details.
#'
#' @param class A character string specifying the `DataClass()` to initialise.
#' Not case dependent and matching is based on either the class name or the its
#' country definition. For a list of options use `get_available_datasets()`.
#' @param level A character string indicating the target administrative level
#' of the data with the default being "1". Currently supported options are
#' level 1 ("1) and level 2 ("2"). Use `get_available_datasets()` for supported
#' options by dataset.
#' @param steps Logical, defaults to FALSE. Should all processing and cleaning
#' steps be kept and output in a list.
#' @param regions A character vector of target regions to be assigned to the
#' `target_regions` field and used to filter the returned data.
#' @param totals Logical, defaults to FALSE. If TRUE, returns totalled
#'  data per region up to today's date. If FALSE, returns the full dataset
#'  stratified by date and region.
#' @param localise Logical, defaults to TRUE. Should region names be localised.
#' @param get Logical, defaults to FALSE. Should the class `get` method be
#' called (this will download, clean, and process data at initialisation).
#' @param ... Additional arguments to pass to class specific functionality.
#' @return An initialised version of the target class if available,
#' e.g. `Italy()`
#' @family interface
#' @inheritParams message_verbose
#' @inheritParams get_available_datasets
#' @rdname initialise_dataclass
#' @importFrom stringr str_to_title str_replace_all str_detect
#' @importFrom dplyr bind_rows filter distinct
#' @importFrom purrr map_lgl
#' @export
#' @examples
#' \dontrun{
#' # set up a cache to store data to avoid downloading repeatedly
#' start_using_memoise()
#'
#' # check currently available datasets
#' get_available_datasets()
#'
#' # initialise a data set in the United Kingdom
#' # at the UTLA level
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
#'
#' # initialise Italian data, download, clean and process it
#' italy <- initialise_dataclass("Italy", get = TRUE)
#' italy$return()
#'
#' # initialise ECDC data, fully process it, and return totals
#' ecdc <- initialise_dataclass("ecdc", get = TRUE, totals = TRUE)
#' ecdc$return()
#' }
initialise_dataclass <- function(class = character(), level = "1",
                                 totals = FALSE, localise = TRUE,
                                 regions, verbose = TRUE, steps = FALSE,
                                 get = FALSE, type = c("national", "regional"),
                                 ...) {
  stopifnot(is.character(class))
  level <- as.character(level)

  # construct short hand options
  title_class <- str_to_title(class)
  nospace <- str_replace_all(title_class, " ", "")
  targets <- c(
    title_class, toupper(title_class), nospace, toupper(nospace)
  )

  # check we have data for desired class
  datasets <- covidregionaldata::get_available_datasets(type)
  target_class <- bind_rows(
    filter(datasets, map_lgl(.data$class, ~ any(str_detect(., targets)))),
    filter(datasets, map_lgl(.data$origin, ~ any(str_detect(., targets))))
  ) %>%
    distinct()

  if (nrow(target_class) == 0) {
    stop(
      "No data available for ", class,
      " see get_available_datasets(type = c(",
      paste(type, collapse = ", "), ")) for supported datasets"
    )
  }

  regionClass <- get(target_class$class[1])
  region_class <- regionClass$new(
    level = level, totals = totals,
    localise = localise, verbose = verbose,
    steps = steps, regions = regions, get = get, ...
  )

  return(region_class)
}

#' R6 Class containing non-dataset specific methods
#'
#' @description A parent class containing non-dataset specific methods.
#'
#' @details All data sets have shared methods for extracting geographic codes,
#' downloading, processing, and returning data. These functions are contained
#' within this parent class and so are accessible by all data sets which
#' inherit from here. Individual data sets can overwrite any functions or
#' fields providing they define a method with the same name, and can be
#' extended with additional functionality. See the individual method
#' documentaion for further details.
#' @family interface
#' @importFrom R6 R6Class
DataClass <- R6::R6Class(
  "DataClass",
  public = list(
    #' @field origin the origin of the data source. For regional data sources
    #' this will usually be the name of the country.
    origin = "",
    #' @field data Once initialised, a list of named data frames: raw
    #' (list of named raw data frames) clean (cleaned data) and processed
    #' (processed data). Data is accessed using `$data`.
    data = NULL,
    #' @field supported_levels A list of supported levels.
    supported_levels = list("1"),
    #' @field supported_region_names A list of region names in order of level.
    supported_region_names = list("1" = NA),
    #' @field supported_region_codes A list of region codes in order of level.
    supported_region_codes = list("1" = NA),
    #' @field region_name string Name for the region column, e.g. 'region'.
    #' This field is filled at initialisation with the region name for the
    #' specified level (supported_region_names$level).
    region_name = NULL,
    #' @field code_name string Name for the codes column, e.g. 'iso_3166_2'
    #' Filled at initialisation with the code name associated with the
    #' requested level (supported_region_codes$level).
    code_name = NULL,
    #' @field codes_lookup string or tibble Region codes for the target origin
    #' filled by origin specific codes in
    #' \href{#method-set_region_codes}{\code{set_region_codes()}}
    codes_lookup = list(),
    #' @field data_urls List of named common and shared url links to raw data.
    #' Prefers shared if there is a name conflict.
    data_urls = list(),
    #' @field common_data_urls List of named links to raw data that are common
    #' across levels. The first entry should be named main.
    common_data_urls = list(),
    #' @field level_data_urls List of named links to raw data that are level
    #' specific. Any urls that share a name with a url from
    #' `common_data_urls` will be selected preferentially. Each top level
    #' list should be named after a supported level.
    level_data_urls = list(),
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c(),
    #' @field level target region level. This field is filled at initialisation
    #' using user inputs or defaults in `$new()`
    level = NULL,
    #' @field data_name string. The country name followed by the level. E.g.
    #' "Italy at level 1"
    data_name = NULL,
    #' @field totals Boolean. If TRUE, returns totalled data per region
    #' up to today's date. This field is filled at initialisation using user
    #' inputs or defaults in `$new()`
    totals = NULL,
    #' @field localise Boolean. Should region names be localised.
    #' This field is filled at initialisation using user inputs or defaults
    #' in `$new()`
    localise = NULL,
    #' @field verbose Boolean. Display information at various stages.
    #' This field is filled at initialisation. using user inputs or defaults
    #' in `$new()`
    verbose = NULL,
    #' @field steps Boolean. Keep data from each processing step.
    #' This field is filled at initialisation.using user inputs or defaults
    #' in `$new()`
    steps = NULL,
    #' @field target_regions A character vector of regions to filter for. Used
    #' by the `filter method`.
    target_regions = NULL,
    #' @field process_fns array, additional, user supplied functions to process
    #' the data.
    process_fns = c(set_negative_values_to_zero),
    #' @description Place holder for custom country specific function to load
    #' region codes.
    set_region_codes = function() {
    },
    #' @field filter_level Character The level of the data to filter at.
    #' Defaults to the target level.
    filter_level = NA,

    #' @description Initialize function used by all `DataClass` objects.
    #' Set up the `DataClass` class with attributes set to input parameters.
    #' Should only be called by a `DataClass` class object.
    #' @param level A character string indicating the target administrative
    #' level of the data with the default being "1". Currently supported
    #' options are level 1 ("1) and level 2 ("2").
    #' @param filter_level A character string indicating the level to filter at.
    #' Defaults to the level of the data if not specified and if not otherwise
    #' defined in the class.
    #' Use `get_available_datasets()` for supported options by dataset.
    #' @param regions A character vector of target regions to be assigned to
    #' the`target_regions` field if present.
    #' @param totals Logical, defaults to FALSE. If TRUE, returns totalled
    #'  data per region up to today's date. If FALSE, returns the full dataset
    #'  stratified by date and region.
    #' @param localise Logical, defaults to TRUE. Should region names be
    #' localised.
    #' @param verbose Logical, defaults to TRUE. Should verbose processing
    #' @param steps Logical, defaults to FALSE. Should all processing and
    #' cleaning steps be kept and output in a list.
    #' @param get Logical, defaults to FALSE. Should the class `get` method be
    #' called (this will download, clean, and process data at initialisation).
    #' @param process_fns Array, additional functions to process the data.
    #' Users can supply their own functions here which would act on clean data
    #' and they will be called alongside our default processing functions.
    #' The default optional function added is `set_negative_values_to_zero`.
    #' if process_fns is not set (see `process_fns` field for all defaults).
    #' If you want to keep this when supplying your own processing functions
    #' remember to add it to your list also. If you feel you have created a
    #' cool processing function that others could benefit from please submit a
    # nolint start
    #' Pull Request to our \href{https://github.com/epiforecasts/covidregionaldata}{github repository}
    # nolint end
    #' and we will consider adding it to the package.
    #' @export
    initialize = function(level = "1", filter_level, regions,
                          totals = FALSE, localise = TRUE,
                          verbose = TRUE, steps = FALSE, get = FALSE,
                          process_fns) {
      level <- as.character(level)
      self$level <- level
      if (is.na(self$filter_level)) {
        self$filter_level <- level
      }
      if (!missing(filter_level)) {
        self$filter_level <- filter_level
      }
      check_level(self$level, self$supported_levels)
      check_level(self$filter_level, self$supported_levels)

      self$data_name <- paste0(class(self)[1], " at level ", self$level)
      self$totals <- totals
      self$localise <- localise
      self$verbose <- verbose
      self$steps <- steps
      self$region_name <- self$supported_region_names[[self$level]]
      self$code_name <- self$supported_region_codes[[self$level]]
      self$set_region_codes()
      if (!missing(process_fns)) {
        self$process_fns <- process_fns
      }

      if (!missing(regions)) {
        self$target_regions <- regions
      }

      if (!is.null(self$level_data_urls[[self$level]])) {
        if (length(self$common_data_urls) > 0) {
          self$data_urls <- unlist(merge(
            self$level_data_urls[[self$level]],
            self$common_data_urls
          ))
        } else {
          self$data_urls <- self$level_data_urls[[self$level]]
        }
      } else {
        self$data_urls <- self$common_data_urls
      }

      if (get) {
        self$get()
      }
    },

    #' @description Download raw data from `data_urls`, stores a named list
    #' of the `data_url` name and the corresponding raw data table in
    #' `data$raw`
    #' @importFrom purrr map
    download = function() {
      if (length(self$data_urls) == 0) {
        stop("No data to download as data_urls is empty")
      }
      self$data$raw <- map(self$data_urls, csv_reader,
        verbose = self$verbose
      )
    },

    #' @description Download raw data from `data_urls`, stores a named list
    #' of the `data_url` name and the corresponding raw data table in
    #' `data$raw`. Designed as a drop-in replacement for `download` so
    #' it can be used in sub-classes.
    #' @importFrom purrr map
    download_JSON = function() {
      if (length(self$data_urls) == 0) {
        stop("No data to download as data_urls is empty")
      }
      self$data$raw <- map(self$data_urls, json_reader,
        verbose = self$verbose
      )
    },

    #' @description Cleans raw data (corrects format, converts column types,
    #' etc). Works on raw data and so should be called after
    #' \href{#method-download}{\code{download()}}
    #' Calls the specific class specific cleaning method (`clean_common`)
    #' followed by level specific cleaning methods.
    #' `clean_level_[1/2]`. Cleaned data is stored in `data$clean`
    clean = function() {
      if (is.null(self$data$raw)) {
        stop("Data must first be downloaded using the download method")
      }
      message_verbose(self$verbose, "Cleaning data")
      self$clean_common()

      check_level(self$level, self$supported_levels)
      specific <- paste0("clean_level_", self$level)

      if (any(names(self) %in% specific)) {
        specific <- paste0("self$", specific, "()")
        eval(parse(text = specific))
      }
    },

    #' @description Cleaning methods that are common across a class.
    #' By default this method is empty as if any code is required it should be
    #' defined in a child class specific `clean_common` method.
    clean_common = function() {

    },

    #' @description Show regions that are available to be used for
    #' filtering operations. Can only be called once `clean()` has been
    #' called. Filtering level is determined by checking the `filter_level`
    #' field.
    #' @param level A character string indicating the level to filter at.
    #' Defaults to using the `filter_level` field if not specified
    #' @importFrom tidyselect all_of
    #' @importFrom dplyr select filter pull
    available_regions = function(level) {
      if (is.null(self$data$clean)) {
        stop("Data must first be cleaned using the clean method")
      }
      if (!missing(level)) {
        self$filter_level <- level
      }
      check_level(self$filter_level, self$supported_levels)
      check_level(self$level, self$supported_levels)

      filter_level <- glue_level(self$filter_level)
      target_level <- glue_level(self$level)

      regions <- self$data$clean %>%
        select(all_of(c(filter_level, target_level))) %>%
        filter(!is.na(.data[[target_level]])) %>%
        filter(!(.data[[target_level]] %in% "Unknown")) %>%
        pull(.data[[filter_level]]) %>%
        unique()
      return(regions)
    },
    #' @description Filter cleaned data for a specific region  To be called
    #' after \href{#method-clean}{\code{clean()}}
    #' @param regions A character vector of target regions. Overrides the
    #' current class setting for `target_regions`.
    #' @param level Character The level of the data to filter at. Defaults to
    #' the lowest level in the data.
    #' @importFrom dplyr filter
    filter = function(regions, level) {
      if (is.null(self$data$clean)) {
        stop("Data must first be cleaned using the clean method")
      }

      if (!missing(regions)) {
        self$target_regions <- regions
      }

      if (!missing(level)) {
        check_level(level, self$supported_levels)
        self$filter_level <- level
      }
      check_level(self$filter_level, self$supported_levels)
      check_level(self$level, self$supported_levels)


      if (!is.null(self$target_regions)) {
        message_verbose(
          self$verbose,
          "Filtering data to: ", paste(self$target_regions, collapse = ", ")
        )
        dt <- self$data$clean %>%
          filter(
            .data[[glue_level(self$filter_level)]] %in% self$target_regions
          )
        if (nrow(dt) == 0) {
          stop("No data found for target regions")
        } else {
          self$data$filtered <- dt
        }
      }
    },

    #' @description Processes data by adding and calculating absent columns.
    #' Called on clean data (after \href{#method-clean}{\code{clean()}}).
    #' Some countries may have data as new events (e.g. number of
    #' new cases for that day) whilst others have a running total up to that
    #' date. Processing calculates these based on what the data comes with
    #' via the functions `region_dispatch()` and `process_internal()`,
    #' which does the following:
    #' \itemize{
    #' \item{Adds columns not present in the data `add_extra_na_cols()`}
    #' \item{Ensures there are no negative values
    #' `set_negative_values_to_zero()`}
    #' \item{Removes NA dates `fill_empty_dates_with_na()`}
    #' \item{Calculates cumulative data `complete_cumulative_columns()`}
    #' \item{Calculates missing columns from existing ones
    #' `calculate_columns_from_existing_data()`}
    #' }
    #' @param process_fns Array, additional functions to process the data.
    #' Users can supply their own functions here which would act on clean data
    #' and they will be called alongside our default processing functions.
    #' The default optional function added is `set_negative_values_to_zero`.
    #' if process_fns is not set (see `process_fns` field for all defaults).
    process = function(process_fns) {
      if (is.null(self$data$clean)) {
        stop("Data must first be cleaned using the clean method")
      }

      if (is.null(self$data$filtered)) {
        self$data$filtered <- self$data$clean
      }

      if (!missing(process_fns)) {
        self$process_fns <- process_fns
      }

      message_verbose(self$verbose, "Processing data")
      region_vars <- region_dispatch(
        level = self$level,
        all_levels = self$supported_levels,
        region_names = self$supported_region_names,
        region_codes = self$supported_region_codes
      )

      self$data$processed <- process_internal(
        clean_data = self$data$filtered,
        level = glue_level(self$level),
        group_vars = region_vars,
        totals = self$totals,
        localise = self$localise,
        verbose = self$verbose,
        process_fns = self$process_fns
      )
    },

    #' @description Get data related to the data class. This runs each distinct
    #' step in the workflow in order.
    #' Internally calls \href{#method-download}{\code{download()}},
    #' \href{#method-clean}{\code{clean()}},
    #' \href{#method-filter}{\code{filter()}} and
    #' \href{#method-process}{\code{process()}}
    #' `download`, `clean`, `filter` and `process` methods.
    get = function() {
      self$download()
      self$clean()
      self$filter()
      self$process()
    },

    #' @description Return data. Designed to be called after
    #' \href{#method-process}{\code{process()}}
    #' this uses the steps argument to return either a
    #' list of all the data preserved at each step or just the processed data.
    #' For most datasets a custom method should not be needed.
    return = function() {
      if (is.null(self$data)) {
        stop("Data must first be downloaded (download), cleaned (clean) or
             processed (process), and optionally filtered (filter).")
      }
      self$data$return <- NA
      if (self$steps) {
        return(self$data)
      } else {
        if (is.null(self$data$processed)) {
          stop("Data must first be proccessed using the process method")
        }
        return(self$data$processed)
      }
    },

    #' @description Create a table of summary information for the data set
    #' being processed.
    #' @importFrom tibble tibble
    #' @return Returns a single row summary tibble containing the origin of the
    #' data source, class, level 1 and 2 region names, the type of data,
    #' the urls of the raw data and the columns present in the raw data.
    summary = function() {
      sum_df <- tibble(
        origin = self$origin,
        class = class(self)[1],
        level_1_region = self$supported_region_names[["1"]],
        level_2_region = ifelse(is.null(self$supported_region_names[["2"]]),
          NA, self$supported_region_names[["2"]]
        ),
        level_3_region = ifelse(is.null(self$supported_region_names[["3"]]),
          NA, self$supported_region_names[["3"]]
        ),
        type = ifelse(any(class(self) %in% "CountryDataClass"),
          "national", "regional"
        ),
        data_urls = paste(unlist(self$data_urls), collapse = ", "),
        source_data_cols = paste(unlist(self$source_data_cols),
          collapse = ", "
        ),
        source_text = ifelse(is.null(self$source_text), NA, self$source_text),
        source_url = ifelse(is.null(self$source_url), NA, self$source_url)
      )
      return(sum_df)
    },

    #' @description Run tests on a country class instance. Calling `test()` on a
    #' class instance runs tests with the settings in use. For example, if you
    #' set `level = "1"` and `localise = FALSE` the tests will be run on level 1
    #' data which is not localised. Rather than downloading data for a test
    #' users can provide a path to a snapshot file of data to test instead.
    #' Tests are run on a clone of the class. This method calls generic tests
    #' for all country class objects. It also calls country specific tests
    #' which can be defined in an individual country class method called
    #' `specific_tests()`. The snapshots contain the first 1000 rows of data.
    #' For more details see the
    # nolint start
    #' \href{https://github.com/epiforecasts/covidregionaldata/blob/master/vignettes/testing.Rmd}{'testing' vignette}: \code{vignette(testing)}.
    # nolint end
    #' @param download logical. To download the data (TRUE) or use a snapshot
    #' (FALSE). Defaults to FALSE.
    #' @param snapshot_dir character_array the name of a directory to save the
    #' downloaded data or read from. If not defined a directory called
    #' 'snapshots' will be created in the temp directory. Snapshots are saved as
    #' rds files with the class name and level: e.g. `Italy_level_1.rds`.
    #' @param all logical. Run tests with all settings (TRUE) or with those
    #' defined in the current class instance (FALSE). Defaults to FALSE.
    #' @param ... Additional parameters to pass to `specific_tests`
    test = function(download = FALSE,
                    snapshot_dir = paste0(tempdir(), "/snapshots"),
                    all = FALSE, ...) {
      snapshot_file_name <- paste0(
        class(self)[1], "_level_",
        self$level, ".rds"
      )
      dir.create(snapshot_dir, showWarnings = FALSE)
      message_verbose(
        verbose = self$verbose,
        paste("snapshot to be saved at", snapshot_dir)
      )
      snapshot_path <- file.path(snapshot_dir, snapshot_file_name)
      self_copy <- self$clone()
      test_download(
        DataClass_obj = self_copy,
        download = download,
        snapshot_path = snapshot_path
      )
      test_cleaning(
        DataClass_obj = self_copy
      )
      test_processing(
        DataClass_obj = self_copy,
        all = all
      )
      test_return(
        DataClass_obj = self_copy
      )

      if ("specific_tests" %in% names(self_copy)) {
        specific <- paste0(
          "self$specific_tests(
            self_copy = self_copy,
            download = download,
            all = all,
            snapshot_path = snapshot_path,
            ...
          )"
        )
        eval(parse(text = specific))
      }
    }
  )
)

#' R6 Class containing national level methods
#'
#' @description Acts as parent class for national data classes, allowing them
#'  to access general methods defined in [DataClass()] but with additional
#   features tuned to national level functionality.
#' @details On top of the methods documented in [DataClass()], this class
#' implements a custom filter function that supports partial matching to
#' English country names using the `countrycode` package.
#' @export
#' @family interface
#' @importFrom R6 R6Class
CountryDataClass <- R6::R6Class("CountryDataClass",
  inherit = DataClass,
  public = list(
    #' @field filter_level Character The level of the data to filter at.
    #' Defaults to the country level of the data.
    filter_level = "1",
    #' @description Filter method for country level data. Uses `countryname`
    #' to match input countries with known names.
    #' @param countries A character vector of target countries. Overrides the
    #' current class setting for `target_regions`. If the `filter_level` field
    #' `level` argument is set to anything other than level 1 this is passed
    #' directly to the parent `DataClass()` `filter()` method with no
    #' alteration.
    #' @param level Character The level of the data to filter at. Defaults to
    #' the conuntry level if not specified.
    #' @importFrom countrycode countryname
    filter = function(countries, level) {
      if (!missing(level)) {
        check_level(level, self$supported_levels)
        self$filter_level <- level
      }
      if (self$filter_level == "1") {
        if (!missing(countries)) {
          self$target_regions <- countries
        }

        if (!is.null(self$target_regions)) {
          self$target_regions <- countryname(
            self$target_regions,
            destination = "country.name.en"
          )
          if (all(is.na(self$target_regions))) {
            stop("No countries found with target names")
          }
        }
      }
      super$filter()
    }
  )
)
