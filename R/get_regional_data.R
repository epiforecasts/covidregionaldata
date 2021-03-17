#' Fetch standardised sub-national Covid-19 data
#'
#' @description Downloads, cleans and processes regional data
#' for Covid-19.
#' @param country A character string specifying the country to get data from.
#' Not case dependent. Name should be the English name. For a list of
#' options see `available_datasets`.
#' @param level A character string indicating the target administrative level
#' of the data with the default being "1". Currently supported options are
#' level 1 ("1) and level 2 ("2"). See `available_datasets` for supported
#' options by dataset.
#' @param totals Logical, defaults to FALSE. If TRUE, returns totalled
#'  data per region up to today's date. If FALSE, returns the full dataset
#'  stratified by date and region.
#' @param localise Logical, defaults to TRUE. Should region names be localised.
#' @param verbose Logical, defaults to `TRUE`. Should verbose processing
#' messages and warnings be returned.
#' @param steps Logical, defaults to FALSE. Should all processing and cleaning
#' steps be kept and output in a list.
#' @param ... additional arguments to pass to Country classes.
#' @return A tibble with data related to cases, deaths, hospitalisations,
#' recoveries and testing stratified by regions within the given country.
#' @export
#'
get_regional_data <- function(country, level = "1", totals = FALSE,
                              localise = TRUE, verbose = TRUE,
                              steps = FALSE, ...) {
  # check data availability and initiate country class if avaliable
  region_class <- check_country_avaliable(
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

  region <- region_class$return()
  return(region)
}
