#' Construct a covidregionaldata class object
#'
#' @description Constructs a `covidregionaldata` class object for the
#' target dataset of interest (see return). Returns an error if the requested
#' dataset is not supported.
#' @return A list containing target country name, administrative level of
#' the data, and a list of information about the region codes. The list
#' well be assigned a class start "crd_" and containing the target country
#' and administrative level to control method dispatch.
#' @importFrom dplyr filter
#' @examples
#' new_covidregionaldata("uk", "1")
new_covidregionaldata <- function(country = character(), level = character(),
                                  verbose = TRUE) {
    stopifnot(is.character(country))
    stopifnot(is.character(level))

    countries <- available_datasets %>%
        filter(get_data_function %in% "get_regional_data")

    tar_country <- match.arg(
        country, choices = countries$country, several.ok = FALSE
        )

    target <- countries %>%
        filter(country %in% tar_country)

    level <- match.arg(level, choices = c("1", "2"), several.ok = FALSE)
    tar_level <- paste0("level_", level, "_region")

    if (is.na(target[[tar_level]])) {
        stop("Target spatial level not supported in the selected country.
               See available_datasets for supported options")
    }

    if (verbose) {
        message("Getting data for the ", toupper(tar_country), 
                " at ", target[[tar_level]], " administrative region")
    }

    x <- list(country = tar_country, level = target[[tar_level]], 
              location_codes = NULL)
    structure(x, class = paste0("crd_", tar_country, "_", level))
    return(x)
}