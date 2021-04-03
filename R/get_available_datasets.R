#' Get the countries data is available for
#'
#' @description Show what countries have what level data available.
#' The function searches the environment for R6 class objects and
#' extracts the country name and what level it has from the object.
#' @return A list of available countries and the region level
#' data is available for
#' @importFrom rlang .data
#' @importFrom dplyr select bind_rows
#' @importFrom tibble as_tibble
#' @export
#' @examples
#' get_available_datasets()
get_available_datasets <- function() {
  envi <- ls(getNamespace("covidregionaldata"), all.names = TRUE)
  # regional data
  starts_with_capitals_idx <- grep("^[A-Z]", envi)
  starts_with_capitals <- envi[starts_with_capitals_idx]
  exclude <- c("DataClass", "CountryTemplate")
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
  return(available_country_data)
}
