#' Check country and regions are avaliable and set up
#'
#' @description Check data for the requested country and region
#' is avaliable and return an initialised region class for that country.
#' @param country A character string specifying the country to get data from.
#' Not case dependent. Name should be the English name. For a list of
#' options see `available_datasets`.
#' @param level A character string indicating the target administrative level
#' of the data with the default being "1". Currently supported options are
#' level 1 ("1) and level 2 ("2"). See `available_datasets` for supported
#' options by dataset.
#' @param verbose Logical, defaults to `TRUE`. Should verbose processing
#' messages and warnings be returned.
#' @return The target countries specific object if avaliable, e.g. `Italy`
#' @rdname download_data
check_country_avaliable <- function(country = character(), level = level,
                                    totals = totals, localise = localise,
                                    verbose = verbose, steps = steps, ...) {
  stopifnot(is.character(country))
  stopifnot(is.character(level))

  # format country to find the correct class
  country <- paste0(
    toupper(substr(country, 1, 1)),
    tolower(substr(country, 2, nchar(country)))
  )
  tryCatch(
    regionClass <- get(country),
    error = function(e) {
      stop(
        paste("No data avaliable for country'", country, "'.")
      )
    }
  )
  region_class <- regionClass$new(
    level = level, totals = totals,
    localise = localise, verbose = verbose,
    steps = steps, ...
  )

  level <- match.arg(level, choices = c("1", "2"), several.ok = FALSE)
  tar_level <- paste0("level_", level, "_region")

  if (is.na(region_class[[tar_level]])) {
    stop("Target spatial level not supported in the selected country.
               See available_datasets for supported options")
  }

  return(region_class)
}

#' R6 Class containing non-country specific methods
#'
#' @description Acts as parent class for individual country objects,
#' allowing them to access general methods.
#'
#' @details All countries have shared methods for extracting region codes,
#' downloading, processing, and returning data.
dataClass <- R6::R6Class("Generic methods",
  public = list(
    #' @field country name of country to fetch data for
    country = "",
    #' @field region data frame for requested region
    region = NULL,
    #' @field data_url link to raw data
    data_url = "",
    #' @field level target region level
    level = NULL,
    #' @field totals Boolean. If TRUE, returns totalled data per region up to today's date.
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
          "Getting data for ", self$country,
          " at ", tar_level_name, " administrative region"
        )
      }

      self$region <- list(country = self$country, level = tar_level_name)

      if (nrow(codes) == 1) {
        self$region$code <- codes$name[[1]]
        self$region$codes_lookup <- codes$codes[[1]]
      }

      self$region <- structure(
        self$region,
        class = c(
          paste0("crd_", self$country, "_", self$level),
          paste0("crd_level_", self$level)
        )
      )
    },

    #' @description General function for downloading raw data.
    #' @param ... pass additional arguments
    download_data = function(...) {
      self$region$raw <- suppressWarnings(csv_reader(self$data_url))
    },

    #' Shared regional dataset processing
    #'
    #' @description General function to processes regional data.
    #' Dynamically works for level 1 and level 2 regions.
    #' @param ... pass additional arguments
    process_regional = function(...) {
      region_vars <- switch(self$level,
        "1" = c("region_level_1", "level_1_region_code"),
        "2" = c(
          "region_level_2", "level_2_region_code",
          "region_level_1", "level_1_region_code"
        )
      )
      self$region <- process_regional_internal(
        self$region,
        group_vars = region_vars, totals = self$totals,
        localise = self$localise, verbose = self$verbose
      )
    },

    #' @description Optional region specific return changes. Designed to be called
    #' after `process_regional`. For most datasets a custom method should not be
    #' needed.
    #' @author Sam Abbott
    return_regional = function() {
      self$region$return <- NA
      if (self$steps) {
        return(self$region)
      } else {
        return(self$region$processed)
      }
    }
  )
)
