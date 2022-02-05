#'  Get regional-level data
#'
#' @description Provides an interface to source specific classes which
#' support regional level data. For simple use cases this allows downloading
#' clean, standardised, regional-level COVID-19 data sets. Internally this uses
#' the `DataClass()` parent class which allows documented downloading, cleaning,
#' and processing. Optionally all steps of data processing can be returned
#' along with the functions used for processing but by default just the
#' finalised processed data is returned. See the examples for some potential
#' use cases and the links to lower level functions for more details and
#' options.
#'
#' @param country A character string specifying the country to get data from.
#' Not case dependent. Name should be the English name. For a list of
#' options use `get_available_datasets()`.
#' @inheritParams return_data
#' @inheritParams initialise_dataclass
#' @return A tibble with data related to cases, deaths, hospitalisations,
#' recoveries and testing stratified by regions within the given country.
#' @family interface
#' @seealso [Italy()], [UK()]
#' @export
#' @examples
#' \dontrun{
#' # set up a data cache
#' start_using_memoise()
#'
#' # download data for Italy
#' get_regional_data("italy")
#'
#' # return totals for Italy with no localisation
#' get_regional_data("italy", localise = FALSE, totals = TRUE)
#'
#' # download data for the UK but return the class
#' uk <- get_regional_data("United Kingdom", class = TRUE)
#' uk
#'
#' # return UK data from the class object]
#' uk$return()
#' }
get_regional_data <- function(country, level = "1", totals = FALSE,
                              localise = TRUE, steps = FALSE,
                              class = FALSE, verbose = TRUE, regions,
                              ...) {

  # check data availability and initiate country class if available
  region_class <- initialise_dataclass(
    class = country, level = level, regions = regions,
    totals = totals, localise = localise,
    verbose = verbose, steps = steps, get = TRUE,
    type = "regional", ...
  )

  return(
    return_data(region_class, class = class)
  )
}
