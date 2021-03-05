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

  countries <- available_datasets %>%
    filter(get_data_function %in% "get_regional_data")

  tar_country <- match.arg(
    country,
    choices = countries$country, several.ok = FALSE
  )

  target <- countries %>%
    filter(country %in% tar_country)

  level <- match.arg(level, choices = c("1", "2"), several.ok = FALSE)
  tar_level <- paste0("level_", level, "_region")

  if (is.na(target[[tar_level]])) {
    stop("Target spatial level not supported in the selected country.
               See available_datasets for supported options")
  }

  codes <- region_codes %>%
    filter(country %in% tar_country,
           level %in% tar_level)

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

#' Shared regional dataset processing
#'
#' @description Shared regional data cleaning designed to be called
#' after `clean_regional`.
#' @export
#' @inheritParams clean_regional
#' @rdname process_regional
#' @author Sam Abbott
process_regional <- function(region, verbose = TRUE, ...) {
  UseMethod("process_regional")
}
