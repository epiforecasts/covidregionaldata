#' Fetch standardised sub-national Covid-19 data
#'
#' @description Downloads, cleans and processes regional data
#' for Covid-19.
#' @param country A character string specifying the country to get data from.
#' Not case dependent. Name should be the English name. For a list of
#' options use `get_available_datasets`.
#' @param level A character string indicating the target administrative level
#' of the data with the default being "1". Currently supported options are
#' level 1 ("1) and level 2 ("2"). Use `get_available_datasets` for supported
#' options by dataset.
#' @param verbose Logical, defaults to `TRUE`. Should verbose processing
#' messages and warnings be returned.
#' @param steps Logical, defaults to FALSE. Should all processing and cleaning
#' steps be kept and output in a list.
#' @param return_class Logical, defaults to FALSE. If TRUE returns the
#' `Country` class object.
#' @inheritParams process_internal
#' @param ... additional arguments to pass to country specific functionality.
#' @return A tibble with data related to cases, deaths, hospitalisations,
#' recoveries and testing stratified by regions within the given country.
#' @export
#' @examples
#' \dontrun{
#' # set up a data cache
#' start_using_memoise()
#'
#' # download data for Italy keeping all processing steps
#' get_national_data(country = "italy", verbose = TRUE, steps = TRUE)
#' }
get_regional_data <- function(country, level = "1", totals = FALSE,
                              localise = TRUE, verbose = TRUE,
                              steps = FALSE, return_class = FALSE,
                              ...) {
  # format country string
  country <- paste0(
    toupper(substr(country, 1, 1)),
    tolower(substr(country, 2, nchar(country)))
  )

  # check data availability and initiate country class if avaliable
  region_class <- check_country_available(
    country = country, level = level,
    totals = totals, localise = localise,
    verbose = verbose, steps = steps, ...
  )

  # download and cache raw data
  region_class$download()

  # dataset specifc cleaning
  region_class$clean()

  # non-specific cleaning and checks
  region_class$process()

  if (return_class) {
    return(region_class)
  }
  region <- region_class$return()
  return(region)
}
