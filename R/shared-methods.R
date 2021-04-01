#' Check country and regions are available and set up country class
#'
#' @description Check data for the requested country and region
#' is available and return an initialised region class for that country.
#' @inheritParams get_regional_data
#' @return The target countries specific object if available, e.g. [Italy()]
#' @rdname check_country_available
#' @export
#' @examples
#' check_country_available(country = "Italy")
check_country_available <- function(country = character(), level = 1,
                                    totals = FALSE, localise = TRUE,
                                    verbose = TRUE, steps = FALSE, ...) {
  stopifnot(is.character(country))
  level <- as.character(level)

  # if country is UK whole name must be upper case
  if (country == "Uk") {
    country <- toupper(country)
  }

  # check we have data for desired country
  available_sources <- covidregionaldata::get_available_datasets()
  available_sources <- available_sources$country
  if (!(country %in% available_sources)) {
    stop(
      paste0("No data available for country '", country, "'.")
    )
  }
  regionClass <- get(country)
  region_class <- regionClass$new(
    level = level, totals = totals,
    localise = localise, verbose = verbose,
    steps = steps, ...
  )

  level <- match.arg(level, choices = c("1", "2"), several.ok = FALSE)
  tar_level <- paste0("level_", level, "_region")

  if (is.null(region_class[["localise_regions"]][[tar_level]])) {
    stop("Target spatial level not supported in the selected country.
               use get_available_datasets() to see supported options")
  }

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
#' @param totals Logical, defaults to FALSE. If TRUE, returns totalled
#'  data per region up to today's date. If FALSE, returns the full dataset
#'  stratified by date and region.
#' @param localise Logical, defaults to TRUE. Should region names be localised.
#' @param verbose Logical, defaults to `TRUE`. Should verbose processing
#' messages and warnings be returned.
#' @param steps Logical, defaults to FALSE. Should all processing and cleaning
#' steps be kept and output in a list.
#' #' @export
#'
general_init <- function(self, level = "1",
                         totals = FALSE, localise = TRUE,
                         verbose = TRUE, steps = FALSE) {
  if (!is.null(self$supported_levels[[level]])) {
    self$level <- level
  }else{
    stop(level, " is not a supported level check supported_levels for options")
  }
  self$totals <- totals
  self$localise <- localise
  self$verbose <- verbose
  self$steps <- steps
  self$country <- tolower(class(self)[1])
  self$region_name <- self$supported_region_names[[self$level]]
  self$code_name <- self$supported_region_codes[[self$level]]
  self$set_region_codes()
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
    #' @field region_name A list of region names in order of level.
    supported_region_names = list("1" = NA),
    #' @field region_code A list of region codes in order of level.
    supported_region_codes = list("1" = NA),
    #' @field region_name string Name for the codes column, e.g. 'iso_3166_2'
    region_name = NULL,
    #' @field code_name string Name for the codes column, e.g. 'iso_3166_2'
    code_name = NULL,
    #' @field codes_lookup string or tibble Region codes for the target country
    codes_lookup = list(),
    #' @field data_url List of named links to raw data. The first, and
    #' sometimes only entry, should be named main
    data_url = list(),
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
    #' @description Place holder for custom country specific function to load
    #' region codes.
    set_region_codes = function() {
      self$code_name 
    },

    #' @description General function for downloading raw data.
    #' @importFrom purrr map
    download = function() {
      self$data$raw <- map(self$data_url, csv_reader,
        verbose = self$verbose
      )
    },

    #' @description General cleaning function
    clean = function() {
      warning("Custom cleaning method not defined. 'clean' set as 'raw'.")
      self$data$clean <- self$data$raw[["main"]]
    },

    #' Shared regional dataset processing
    #'
    #' @description General function to processes regional data.
    #' Dynamically works for level 1 and level 2 regions.
    process = function() {
      message_verbose(self$verbose, "Processing data")
      region_vars <- switch(self$level,
        "1" = c("level_1_region" = self$localise_regions$level_1_region, 
                "level_1_region_code" = self$localise_regions$level_1_region_code), # nolint
        "2" = c("level_2_region" = self$localise_regions$level_2_region, 
                "level_2_region_code" = self$localise_regions$level_2_region_code,
                "level_1_region" = self$localise_regions$level_1_region, 
                "level_1_region_code" = self$localise_regions$level_1_region_code)
      )
      tar_level <- paste0("level_", self$level, "_region")
      self$data$processed <- process_internal(
        clean_data = self$data$clean, 
        level = tar_level,
        group_vars = region_vars, 
        totals = self$totals,
        localise = self$localise, 
        verbose = self$verbose
      )
    },

    #' @description Optional region specific return changes.
    #' Designed to be called after `process`. For most datasets a
    #' custom method should not be needed.
    return = function() {
      self$data$return <- NA
      if (self$steps) {
        return(self$data)
      } else {
        return(self$data$processed)
      }
    }
  )
)
