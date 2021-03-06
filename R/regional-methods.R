#' Construct a covidregionaldata class object
#'
#' @description Constructs a `covidregionaldata` class object for the
#' target dataset of interest (see return). Returns an error if the requested
#' dataset is not supported.
#' @param country A character string specifying the country to get data from.
#' Not case dependent. Name should be the English name. For a list of
#' options see `available_datasets`.
#' @param level A character string indicating the target administrative level
#' of the data with the default being "1". Currently supported options are
#' level 1 ("1) and level 2 ("2"). See `available_datasets` for supported
#' options by dataset.
#' @param verbose Logical, defaults to `TRUE`. Should verbose processing
#' messages and warnings be returned.
#' @return A list containing target country name, administrative level of
#' the data, and a list of information about the region codes. The list
#' well be assigned a class start "crd_" and containing the target country
#' and administrative level to control method dispatch.
#' @importFrom dplyr filter
#' @author Sam Abbott
#' @export
#' @examples
#' # initialise data downloading in the UK
#' new_covidregionaldata("uk", "1")
#' # initialise data downloading for mexico
#' new_covidregionaldata("mexico", "1")
new_covidregionaldata <- function(country = character(), level = "1",
                                  verbose = TRUE) {
  stopifnot(is.character(country))
  stopifnot(is.character(level))

  countries <- covidregionaldata::available_datasets %>%
    filter(.data$get_data_function %in% "get_regional_data")

  tar_country <- match.arg(
    tolower(country),
    choices = countries$country, several.ok = FALSE
  )

  target <- countries %>%
    filter(.data$country %in% tar_country)

  level <- match.arg(level, choices = c("1", "2"), several.ok = FALSE)
  tar_level <- paste0("level_", level, "_region")

  if (is.na(target[[tar_level]])) {
    stop("Target spatial level not supported in the selected country.
               See available_datasets for supported options")
  }

  codes <- covidregionaldata::region_codes %>%
    filter(
      .data$country %in% tar_country,
      .data$level %in% tar_level
    )

  if (verbose) {
    message(
      "Getting data for the ", tar_country,
      " at ", target[[tar_level]], " administrative region"
    )
  }

  x <- list(country = tar_country, level = target[[tar_level]])

  if (nrow(codes) == 1) {
    x$code <- codes$name[[1]]
    x$codes_lookup <- codes$codes[[1]]
  }

  x <- structure(
    x,
    class = c(
      paste0("crd_", tar_country, "_", level),
      paste0("crd_level_", level)
    )
  )
  return(x)
}

#' Region specific data downloading
#'
#' @description Region specific raw data download function. See
#' region specific methods for details.
#' @export
#' @param region A list as produced by `new_covidregionaldata`.
#' @param ... Additional arguments to pass to dataset specific methods
#' @inheritParams new_covidregionaldata
#' @rdname download_regional
#' @author Sam Abbott
download_regional <- function(region, verbose = TRUE, ...) {
  UseMethod("download_regional")
}

#' Default regional data downloading
#'
#' @description Default download method. Causes an error if called.
#' @export
#' @inheritParams download_regional
#' @method download_regional default
#' @author Sam Abbott
#' @examples
#' region <- new_covidregionaldata("uk", "1")
#' class(region) <- "list"
#' download_regional(region, verbose = FALSE)
download_regional.default <- function(region, verbose = TRUE, ...) {
  if (verbose) {
    warning(
      "A download function has not been implemented for this data source"
    )
  }
  region$raw <- NA
  return(region)
}

#' Region specific data cleaning
#'
#' @description Region specific data cleaning. See the underlying
#' regional methods for details. Designed to be called after
#' `download_regional`
#' @export
#' @inheritParams download_regional
#' @rdname clean_regional
#' @author Sam Abbott
clean_regional <- function(region, verbose = TRUE, ...) {
  UseMethod("clean_regional")
}

#' Default region specific data cleaning
#'
#' @description Default regional cleaning method. Highlights that no custom
#' cleaning is happening.
#' @export
#' @inheritParams clean_regional
#' @method clean_regional default
#' @author Sam Abbott
#' region <- new_covidregionaldata("uk", "1")
#' class(region) <- "list"
#' region <- download_regional(region, verbose = FALSE)
#' clean_regional(region, verbose = FALSE)
clean_regional.default <- function(region, verbose = TRUE, ...) {
  if (verbose) {
    warning(
      "A cleaning function has not been implemented for this data source"
    )
  }
  region$clean <- region$raw
  return(region)
}

#' Shared regional dataset processing
#'
#' @description Shared regional data cleaning designed to be called
#' after `clean_regional`.
#' @param totals Logical, defaults to FALSE. If TRUE, returns totalled
#'  data per region up to today's date. If FALSE, returns the full dataset
#'  stratified by date and region.
#' @param localise Logical, defaults to TRUE. Should region names be localised.
#' @export
#' @inheritParams clean_regional
#' @rdname process_regional
#' @author Sam Abbott
process_regional <- function(region, totals = FALSE, localise = TRUE,
                             verbose = TRUE) {
  UseMethod("process_regional")
}

#' Shared Regional Dataset Processing for Level 1 Data
#'
#' @description Shared regional data cleaning designed to be called
#' after `clean_regional` for level 1 regions.
#' @export
#' @inheritParams process_regional
#' @method process_regional crd_level_1
#' @author Sam Abbott
#' @examples
#' \dontrun{
#' mexico <- new_covidregionaldata("mexico")
#' mexico <- download_regional(mexico)
#' mexico <- clean_regional(mexico)
#' process_regional(mexico)$processed
#' }
process_regional.crd_level_1 <- function(region, totals = FALSE,
                                         localise = TRUE, verbose = TRUE) {
  region_vars <- c("region_level_1", "level_1_region_code")
  region <- process_regional_internal(
    region,
    group_vars = region_vars, totals = totals,
    localise = localise, verbose = verbose
  )
  return(region)
}

#' Shared Regional Dataset Processing for Level 2 Data
#'
#' @description Shared regional data cleaning designed to be called
#' after `clean_regional` for level 2 regions.
#' @export
#' @inheritParams process_regional
#' @method process_regional crd_level_2
#' @author Sam Abbott
#' @examples
#' \dontrun{
#' mexico <- new_covidregionaldata("mexico", level = "2")
#' mexico <- download_regional(mexico)
#' mexico <- clean_regional(mexico)
#' process_regional(mexico)$processed
#' }
process_regional.crd_level_2 <- function(region, totals = FALSE,
                                         localise = TRUE, verbose = TRUE) {
  region_vars <- c(
    "region_level_2", "level_2_region_code",
    "region_level_1", "level_1_region_code"
  )

  region <- process_regional_internal(
    region,
    group_vars = region_vars, totals = totals,
    localise = localise, verbose = verbose
  )
  return(region)
}
