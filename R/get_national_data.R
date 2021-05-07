#' Get national-level data for countries globally from a range of sources
#'
#' @description Provides an interface to source specific classes which
#' support national level data. For simple use cases this allows downloading
#' clean, standardised, national-level COVID-19 data sets. Internally this uses
#' the `CountryDataClass()` parent class which allows documented downloading,
#' cleaning, and processing. Optionally all steps of data processing can be
#' returned along with the functions used for processing but by default just
#' the finalised processed data is returned. See the examples for some
#' potential use cases and the links to lower level functions for more details
#' and options.
#'
#' @param countries A character vector specifying country names of interest.
#' Used to filter the data.
#' @param country `r lifecycle::badge("deprecated")` A character string
#'  specifying a country to filter for.
#' @param source A character string specifying the data source (not case
#'  dependent). Defaults to WHO (the World Health Organisation). See
#' `get_available_datasets("national")` for all options.
#' @return A tibble with data related to cases, deaths, hospitalisations,
#'  recoveries and testing.
#' @inheritParams get_regional_data
#' @importFrom lifecycle deprecated is_present deprecate_warn
#' @family interface
#' @seealso [WHO()], [ECDC()], [JHU()], [Google()]
#' @export
#' @examples
#' \dontrun{
#' # set up a data cache
#' start_using_memoise()
#'
#' # download all national data from the WHO
#' get_national_data(source = "who")
#'
#' # download data for Canada keeping all processing steps
#' get_national_data(countries = "canada", source = "ecdc")
#'
#' # download data for Canada from the JHU and return the full class
#' jhu <- get_national_data(countries = "canada", source = "jhu", class = TRUE)
#' jhu
#'
#' # return the JHU data for canada
#' jhu$return()
#'
#' # check which regions the JHU supports national data for
#' jhu$available_regions()
#'
#' # filter instead for France (and then reprocess)
#' jhu$filter("France")
#' jhu$process()
#'
#' # explore the structure of the stored JHU data
#' jhu$data
#' }
get_national_data <- function(countries, source = "who", level = "1", totals = FALSE,
                              steps = FALSE, class = FALSE, verbose = TRUE,
                              country = deprecated(),
                              ...) {
  if (is_present(country)) {
    deprecate_warn(
      "0.9.0",
      "covidregionaldata::get_national_data(country = )",
      "covidregionaldata::get_national_data(countries = )"
    )
    countries <- country
  }

  # check data availability and initiate country class if available
  nation_class <- initialise_dataclass(
    class = source, level = level,
    totals = totals, localise = TRUE,
    verbose = verbose, steps = steps,
    regions = countries, get = TRUE,
    type = "national", ...
  )

  return(
    return_data(nation_class, class = class)
  )
}
