#' Get national-level data for countries globally, sourced from the ECDC or WHO.
#'
#' @description Gets raw data using the source-specific function. Includes ISO
#'  country codes. Then adds columns which were missing from the raw data
#'  (calculating cumulative counts from new dailies and vice versa), cleans
#'  and sanitises further. Adds rows and columns of NA values so that data is
#'  in a standard format.
#'
#' @param countries A character vector specifying country names of interest.
#' Used to filter the data.
#' @param country `r lifecycle::badge("deprecated")` A character string
#'  specifying a country to filter for.
#' @param source A character string specifying the data source (not case
#'  dependent). Defaults to WHO (the World Health Organisation). See
#' `get_available_datasets("natioanl")` for all options.
#' @return A tibble with data related to cases, deaths, hospitalisations,
#'  recoveries and testing.
#' @inheritParams get_regional_data
#' @importFrom lifecycle deprecated is_present deprecate_warn
#' @export
#' @examples
#' \dontrun{
#' # set up a data cache
#' start_using_memoise()
#'
#' # download data for Canada keeping all processing steps
#' get_national_data(countries = "canada", source = "ecdc", steps = TRUE)
#' }
get_national_data <- function(countries, source = "who", totals = FALSE,
                              steps = FALSE, class = FALSE, verbose = TRUE,
                              country = deprecated(),
                              ...) {
  if (is_present(country)) {
    deprecate_warn(
      "0.9.0",
      "covidregionaldata::get_national_data(country = )", "covidregionaldata::get_national_data(countries = )"
    )
    countries <- country
  }

  # check data availability and initiate country class if available
  nation_class <- initialise_dataclass(
    class = source, level = "1",
    totals = totals, localise = TRUE,
    verbose = verbose, steps = steps,
    regions = countries, get = TRUE,
    type = "national", ...
  )

  return(
    return_data(nation_class, class = class)
  )
}
