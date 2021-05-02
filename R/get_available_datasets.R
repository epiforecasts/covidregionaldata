#' Get the countries data is available for
#'
#' @description Show what countries have what level data available.
#' The function searches the environment for R6 class objects and
#' extracts the summary information from the available classes using
#' their `summary` methods.
#' @param type A character vector indicating the types of data to
#' return. Current options include "national" (which are datasets at the
#' national level which inherit from `CountryDataClass`) and
#' "regional" (which are datasets at the regional level which inherit
#' directly from `DataClass`).
#' @return A list of available countries and the region level
#' data is available for
#' @importFrom rlang .data
#' @importFrom dplyr select bind_rows filter
#' @importFrom tibble as_tibble
#' @export
#' @examples
#' # see all available datasets
#' get_available_datasets()
#'
#' # see only national level datasets
#' get_available_datasets("national")
#'
#' # see only regional level datasets
#' get_available_datasets("regional")
get_available_datasets <- function(type) {
  envi <- ls(getNamespace("covidregionaldata"), all.names = TRUE)
  # regional data
  starts_with_capitals_idx <- grep("^[A-Z]", envi)
  starts_with_capitals <- envi[starts_with_capitals_idx]
  exclude <- c("DataClass", "CountryDataClass")
  valid_country_objects <- lapply(
    starts_with_capitals,
    function(x) {
      country_obj <- get(x)
      if (class(country_obj) == "R6ClassGenerator" & !(x %in% c(exclude))) {
        dat <- get(x)$new()
        dat <- dat$summary()
        return(dat)
      }
    }
  )
  available_country_data <- valid_country_objects %>%
    bind_rows()

  if (!missing(type)) {
    target_type <- match.arg(
      type,
      choices = c("national", "regional"),
      several.ok = TRUE
    )
    available_country_data <- available_country_data %>%
      filter(type %in% target_type)
  }
  return(available_country_data)
}
