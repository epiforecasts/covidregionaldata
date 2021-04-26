#' Initialise a child class of DataClass if it exists
#'
#' @description Initialise a child class of DataClass if it exists.
#' @param class A character string specifying the `DataClass` to initialise.
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
#' @inheritParams message_verbose
#' @inheritParams get_available_datasets
#' @rdname initialise_dataclass
#' @importFrom stringr str_to_title str_replace_all str_detect
#' @importFrom dplyr bind_rows filter distinct
#' @importFrom purrr map_lgl
#' @export
#' @examples
#' # Initialise Italian data
#' italy <- initialise_dataclass("Italy")
#'
#' # Initialise UK data with a partial name match
#' uk <- initialise_dataclass("United Kingdom")
#'
#' # Initialise ECDC data
#' ecdc <- initialise_dataclass("ecdc")
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
      "No data available for ", class, " see get_available_datasets(type = c(", paste(type, collapse = ", "), ")) for supported datasets"
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

#' R6 Class containing non-origin specific methods
#'
#' @description Acts as parent class for individual origin objects,
#' allowing them to access general methods.
#'
#' @details All countries have shared methods for extracting region codes,
#' downloading, processing, and returning data. These functions are contained
#' within this parent class and so are accessible by all countries which
#' inherit from here. Individual countries can overwrite any functions or
#' fields providing they define a method with the same name.
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
    #' @description Place holder for custom country specific function to load
    #' region codes.
    set_region_codes = function() {
    },

    #' @description Initialize function used by all `DataClass` objects.
    #' Set up the `DataClass` class with attributes set to input parameters.
    #' Should only be called by a `DataClass` class object.
    #' @param level A character string indicating the target administrative
    #' level of the data with the default being "1". Currently supported
    #' options are level 1 ("1) and level 2 ("2").
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
    #' @export
    initialize = function(level = "1", regions,
                          totals = FALSE, localise = TRUE,
                          verbose = TRUE, steps = FALSE, get = FALSE) {
      if (any(self$supported_levels %in% level)) {
        self$level <- level
      } else {
        stop(
          level,
          " is not a supported level check supported_levels for options"
        )
      }
      self$totals <- totals
      self$localise <- localise
      self$verbose <- verbose
      self$steps <- steps
      self$region_name <- self$supported_region_names[[self$level]]
      self$code_name <- self$supported_region_codes[[self$level]]
      self$set_region_codes()

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

    #' @description Filter cleaned data for a specific region  To be called
    #' after \href{#method-clean}{\code{clean()}}
    #' @param regions A character vector of target regions. Overrides the
    #' current class setting for `target_regions`. By default filters at the
    #' current spatial level of interest.
    #' @importFrom dplyr filter
    #' @importFrom rlang !!
    filter = function(regions) {
      if (is.null(self$data$clean)) {
        stop("Data must first be cleaned using the clean method")
      }

      if (!missing(regions)) {
        self$target_regions <- regions
      }

      if (!is.null(self$target_regions)) {
        message_verbose(
          self$verbose,
          "Filtering data to: ", paste(self$target_regions, collapse = ", ")
        )
        condition <- paste0("level_", self$level, "_region")
        dt <- self$data$clean %>%
          filter(
            eval(parse(text = condition)) %in% self$target_regions
          )
        if (nrow(dt) == 0) {
          stop("No data found for target regions")
        } else {
          self$data$clean <- dt
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
    #' Dynamically works for level 1 and level 2 regions.
    process = function() {
      if (is.null(self$data$clean)) {
        stop("Data must first be cleaned using the clean method")
      }

      message_verbose(self$verbose, "Processing data")
      region_vars <- region_dispatch(
        level = self$level,
        all_levels = self$supported_levels,
        region_names = self$supported_region_names,
        region_codes = self$supported_region_codes
      )

      self$data$processed <- process_internal(
        clean_data = self$data$clean,
        level = paste0("level_", self$level, "_region"),
        group_vars = region_vars,
        totals = self$totals,
        localise = self$localise,
        verbose = self$verbose
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
             processed (process)")
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
        type = ifelse(any(class(self) %in% "CountryDataClass"),
          "national", "regional"
        ),
        data_urls = paste(unlist(self$data_urls), collapse = ", "),
        source_data_cols = paste(unlist(self$source_data_cols), collapse = ", ")
      )
      return(sum_df)
    }
  )
)

#' R6 Class containing  national level methods
#' @description Acts as parent class for national data classes, (`WHO()` and
#' `ECDC()`) allowing them to access general methods defined in [DataClass()].
#' Adds filters to get the target country from national data sources.
#'
#' @details Inherits from `DataClass`
#' @export
CountryDataClass <- R6::R6Class("CountryDataClass",
  inherit = DataClass,
  public = list(
    #' @description Filter method for country level data. Uses `countryname`
    #' to match input countries with known names.
    #' @param countries A character vector of target countries. Overrides the
    #' current class setting for `target_regions`.
    #' @importFrom countrycode countryname
    filter = function(countries) {
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
      super$filter()
    }
  )
)
