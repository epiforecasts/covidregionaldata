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
        public_fields <- get(x)$public_fields
        public_fields$level_1_region <- public_fields$localise_regions$level_1_region 
        public_fields$level_2_region <- public_fields$localise_regions$level_2_region 
        public_fields$localise_regions <- NULL
        public_fields$source_data_cols <- paste(
          unlist(public_fields$source_data_cols),
          collapse = " "
        )
        dat <- as_tibble(public_fields)
        dat["country"] <- x
        if (x %in% c("WHO", "ECDC")) {
          dat["get_data_function"] <- "get_national_data"
        } else {
          dat["get_data_function"] <- "get_regional_data"
        }
        return(dat)
      }
    }
  )
  available_country_data <- valid_country_objects %>%
    bind_rows() %>%
    select(
      .data$country,
      .data$level_1_region,
      .data$level_2_region,
      .data$get_data_function,
      .data$data_url,
      .data$source_data_cols
    )
  return(available_country_data)
}
