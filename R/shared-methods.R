#' Check country and regions are available and set up country class
#'
#' @description Check data for the requested country and region
#' is available and return an initialised region class for that country.
#' @inheritParams get_regional_data
#' @return The target countries specific object if available, e.g. [Italy()]
#' @rdname check_country_available
#' @importFrom stringr str_to_title str_replace_all str_detect
#' @importFrom dplyr bind_rows filter distinct
#' @importFrom purrr map_lgl
#' @export
#' @examples
#' # Check for Italian data
#' italy <- check_country_available("Italy")
#'
#' # Check for UK data with a partial name match
#' uk <- check_country_available("United Kingdom")
#'
#' # Check for ECDC data
#' ecdc <- check_country_available("ecdc")
check_country_available <- function(country = character(), level = 1,
                                    totals = FALSE, localise = TRUE,
                                    regions, verbose = TRUE, steps = FALSE,
                                    ...) {
  stopifnot(is.character(country))
  level <- as.character(level)

  # construct short hand options
  title_country <- str_to_title(country)
  nospace <- str_replace_all(title_country, " ", "")
  targets <- c(
    title_country, toupper(title_country), nospace, toupper(nospace)
  )

  # check we have data for desired country
  datasets <- covidregionaldata::get_available_datasets()
  target_class <- bind_rows(
    filter(datasets, map_lgl(.data$class, ~ any(str_detect(., targets)))),
    filter(datasets, map_lgl(.data$country, ~ any(str_detect(., targets))))
  ) %>%
    distinct()

  if (nrow(target_class) == 0) {
    stop("No data available for ", country, " see get_available_datasets() for
    supported datasets")
  }

  regionClass <- get(target_class$class[1])
  region_class <- regionClass$new(
    level = level, totals = totals,
    localise = localise, verbose = verbose,
    steps = steps, regions = regions, ...
  )

  return(region_class)
}

#' initialize function used by all `Country` class objects.
#' @description Set up the country class with attributes set to input
#' parameters. Should only be called by a `Country` class object.
#' @param self The specific class object to attach values
#' @param level A character string indicating the target administrative level
#' of the data with the default being "1". Currently supported options are
#' level 1 ("1) and level 2 ("2"). Use `get_available_datasets` for supported
#' options by dataset.
#' @param regions A character vector of target regions to be assigned to the
#' `target_regions` field if present.
#' @param totals Logical, defaults to FALSE. If TRUE, returns totalled
#'  data per region up to today's date. If FALSE, returns the full dataset
#'  stratified by date and region.
#' @param localise Logical, defaults to TRUE. Should region names be localised.
#' @param verbose Logical, defaults to TRUE. Should verbose processing
#' messages and warnings be returned.
#' @param steps Logical, defaults to FALSE. Should all processing and cleaning
#' steps be kept and output in a list.
#' @param get Logical, defaults to FALSE. Should the class `get` method be
#' called (this will download, clean, and process data at initialisation).
#' @export
initialise_dataclass <- function(self, level = "1", regions,
                                 totals = FALSE, localise = TRUE,
                                 verbose = TRUE, steps = FALSE, get = FALSE) {
  if (any(self$supported_levels %in% level)) {
    self$level <- level
  } else {
    stop(level, " is not a supported level check supported_levels for options")
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
    if (length(self$common_data_urls)>0) {
      self$data_urls <- merge(
        self$common_data_urls,
        self$level_data_urls[[self$level]]
      )
    } else {
      self$data_urls <- self$level_data_urls[[self$level]]
    }
  } else {
    self$data_urls <- self$common_data_urls
  }

  if (get) {
    self$get()
  }
}

#' R6 Class containing non-country specific methods
#'
#' @description Acts as parent class for individual country objects,
#' allowing them to access general methods.
#'
#' @details All countries have shared methods for extracting region codes,
#' downloading, processing, and returning data.
DataClass <- R6::R6Class(
  "DataClass",
  public = list(
    #' @field country name of country to fetch data for
    country = "",
    #' @field data data frame for requested region
    data = NULL,
    #' @field supported_levels A list of supported levels.
    supported_levels = list("1"),
    #' @field supported_region_names A list of region names in order of level.
    supported_region_names = list("1" = NA),
    #' @field supported_region_codes A list of region codes in order of level.
    supported_region_codes = list("1" = NA),
    #' @field region_name string Name for the codes column, e.g. 'iso_3166_2'
    region_name = NULL,
    #' @field code_name string Name for the codes column, e.g. 'iso_3166_2'
    code_name = NULL,
    #' @field codes_lookup string or tibble Region codes for the target country
    codes_lookup = list(),
    #' @field data_urls List of named links to raw data.
    data_urls = list(),
    #' @field common_data_urls List of named links to raw data that are common
    #' across levels. The first entry should be named main.
    common_data_urls = list(),
    #' @field level_data_urls List of named lists of named links to raw data
    #' that are level specific. Any urls that share a name with a url from
    #' `common_data_urls`. Each top level list should be named after a supported
    #' supported level.
    #' will be selected preferentially.
    level_data_urls = list(),
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c(),
    #' @field level target region level
    level = NULL,
    #' @field totals Boolean. If TRUE, returns totalled data per region
    #' up to today's date.
    totals = NULL,
    #' @field localise Boolean. Should region names be localised.
    localise = NULL,
    #' @field verbose Boolean. Display information at various stages.
    verbose = NULL,
    #' @field steps Boolean. Keep data from each processing step.
    steps = NULL,
    #' @field target_regions A character vector of regions to filter for. Used
    #' by the `filter method`.
    target_regions = NULL,
    #' @description Place holder for custom country specific function to load
    #' region codes.
    set_region_codes = function() {
    },

    #' @description Initialize the country
    #' @param ... Parameters passed to `initialise_dataclass`.
    initialize = function(...) {
      initialise_dataclass(self, ...)
    },

    #' @description Download raw data using the list
    #' of supplied data_urls.
    #' @importFrom purrr map
    download = function() {

      self$data$raw <- map(self$data_urls, csv_reader,
        verbose = self$verbose
      )
    },

    #' @description Dispatch cleaning methods to `clean_common` and level
    #' specific cleaning
    clean = function() {
      message_verbose(self$verbose, "Cleaning data")
      self$clean_common()

      specific <- paste0("clean_level_", self$level)

      if (any(names(get(class(self)[1])$public_methods) %in% specific)) {
        specific <- paste0("self$", specific, "()")
        eval(parse(text = specific))
      }
    },

    #' @description Cleaning methods that are common across a class.
    #' By default this method is empty
    clean_common = function() {

    },

    #' @description Filter method for a class
    #' @param regions A character vector of target regions. Overrides the
    #' current class setting for `target_regions`. By default filters at the
    #' current spatial level of interest.
    #' @importFrom dplyr filter
    #' @importFrom rlang !!
    filter = function(regions) {
      if (!missing(regions)) {
        self$target_regions <- regions
      }

      if (!is.null(self$target_regions)) {
        message_verbose(
          self$verbose,
          "Filtering data to: ", paste(self$target_regions, collapse = ", ")
        )
        condition <- paste0("level_", self$level, "_region")
        self$data$clean <- self$data$clean %>%
          filter(
            eval(parse(text = condition)) %in% self$target_regions
          )
        if (nrow(self$data$clean) == 0) {
          stop("No data found for target regions")
        }
      }
    },

    #' @description Processes data.
    #' Dynamically works for level 1 and level 2 regions.
    process = function() {
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

    #' @description Get data related to the data class.
    #' Internally calls `download`, `clean`, and `process` methods.
    get = function() {
      self$download()
      self$clean()
      self$filter()
      self$process()
    },

    #' @description Return data
    #' Designed to be called after `process`. For most datasets a
    #' custom method should not be needed.
    return = function() {
      self$data$return <- NA
      if (self$steps) {
        return(self$data)
      } else {
        return(self$data$processed)
      }
    },

    #' @description Class summary information
    #' @importFrom tibble tibble
    #' @return Returns a single row summary tibble
    summary = function() {
      sum_df <- tibble(
        country = self$country,
        class = class(self)[1],
        level_1_region = self$supported_region_names[["1"]],
        level_2_region = ifelse(is.null(self$supported_region_names[["2"]]),
          NA, self$supported_region_names[["2"]]
        ),
        get_data_function = ifelse(any(c("WHO", "ECDC") %in% class(self)[1]),
          "get_national_data", "get_regional_data"
        ),
        data_url = paste(unlist(self$data_url), collapse = ", "),
        source_data_cols = paste(unlist(self$source_data_cols), collapse = ", ")
      )
      return(sum_df)
    }
  )
)

#' R6 Class containing  national level methods
#' @description Acts as parent class for individual across country objects,
#' allowing them to access general methods.
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
