#' Fetch standardised sub-national Covid-19 data
#'
#' @description Downloads, cleans and processes regional data
#' for Covid-19.
#' @inheritParams new_covidregionaldata
#' @inheritParams process_regional
#' @inheritParams return_regional
#' @param ... pass additional arguments to `download_regional`
#' @return A tibble with data related to cases, deaths, hospitalisations,
#' recoveries and testing stratified by regions within the given country.
#' @export
#' @examples
#' \dontrun{
#' # set up a data cache
#' start_using_memoise()
#'
#' # download the data (and keep all processing steps)
#' get_regional_data(country = "Mexico", steps = TRUE)
#' }
#'
get_regional_data <- function(country, level = "1", totals = FALSE,
                              localise = TRUE, verbose = TRUE,
                              steps = FALSE, ...) {
  # check data availability and define list
  region <- new_covidregionaldata(country, level = level, verbose = verbose)

  # download and cache raw data
  region <- download_regional(region, verbose = verbose, ...)

  # dataset specifc cleaning
  region <- clean_regional(region, verbose = verbose)

  # non-specific cleaning and checks
  region <- process_regional(region, totals = totals, 
                             localise = localise, verbose = verbose)

  region <- return_regional(region, steps = steps)
  return(region)
}
