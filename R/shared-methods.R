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

  # check we have data for desired country
  available_sources <- covidregionaldata::region_codes$country
  if (!(tolower(country) %in% available_sources)) {
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

  if (is.null(region_class[[tar_level]])) {
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
  self$level <- level
  self$totals <- totals
  self$localise <- localise
  self$verbose <- verbose
  self$steps <- steps
  self$country <- tolower(class(self)[1])
  self$get_region_codes()
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
    #' @field region data frame for requested region
    data = NULL,
    #' @field data_url link to raw data
    data_url = "",
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

    #' @description General function for getting region codes for given region.
    #' @rdname get_region_codes
    #' @importFrom rlang .data
    get_region_codes = function() {
      tar_level <- paste0("level_", self$level, "_region")
      tar_level_name <- self[[tar_level]]
      codes <- covidregionaldata::region_codes %>%
        filter(
          .data$country %in% self$country,
          .data$level %in% tar_level
        )

      if (self$verbose) {
        message(
          "Processing data for ", self$country,
          " by ", tar_level_name
        )
      }

      self$data <- list(country = self$country, level = tar_level_name)

      if (nrow(codes) == 1) {
        self$data$code <- codes$name[[1]]
        self$data$codes_lookup <- codes$codes[[1]]
      }

      self$data <- structure(
        self$data
      )
    },

    #' @description General function for downloading raw data.
    download = function() {
      if (self$verbose) {
        message("Downloading data")
        self$data$raw <- suppressWarnings(
          csv_reader(self$data_url)
        )
      } else {
        self$data$raw <- suppressMessages(
          suppressWarnings(
            csv_reader(self$data_url)
          )
        )
      }
    },

    #' Shared regional dataset processing
    #'
    #' @description General function to processes regional data.
    #' Dynamically works for level 1 and level 2 regions.
    process = function() {
      if (self$verbose) {
        message("Processing data")
      }
      region_vars <- switch(self$level,
        "1" = c("region_level_1", "level_1_region_code"),
        "2" = c(
          "region_level_2", "level_2_region_code",
          "region_level_1", "level_1_region_code"
        )
      )
      self$data <- process_internal(
        self$data,
        group_vars = region_vars, totals = self$totals,
        localise = self$localise, verbose = self$verbose
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
