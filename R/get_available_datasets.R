#' Get supported data sets
#'
#' @description Returns data on what countries are available from
#' the data provided with this package either using a cached dataset or built
#' by searching the target namespace.
#' @param type A character vector indicating the types of data to
#' return. Current options include "national" (which are datasets at the
#' national level which inherit from `CountryDataClass`) and
#' "regional" (which are datasets at the regional level which inherit
#' directly from `DataClass()`).
#' @param render Logical If TRUE the supported data set table is built from the
#' available classes using `summary` methods. If FALSE the supported
#' data set table is taken from package data. Defaults to FALSE.
#' @param namespace Character string The name of the namespace to search for
#'  class objects. Defaults to "covidregionaldata" as the package.
#' @return A list of available data sets and the spatial aggregation data is
#' available for.
#' @family interface
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
#'
#' # render the data
#' get_available_datasets(render = TRUE)
get_available_datasets <- function(type, render = FALSE,
                                   namespace = "covidregionaldata") {
  if (render) {
    envi <- ls(getNamespace(namespace), all.names = TRUE)
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
    country_data <- available_country_data
  } else {
    country_data <- covidregionaldata::all_country_data
  }
  if (!missing(type)) {
    target_type <- match.arg(
      type,
      choices = c("national", "regional"),
      several.ok = TRUE
    )
    country_data <- country_data %>%
      filter(type %in% target_type)
  }
  return(country_data)
}
