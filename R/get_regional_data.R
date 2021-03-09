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
  region_class$download_data()

  # dataset specifc cleaning
  region_class$clean_regional()

  # non-specific cleaning and checks
  region_class$process_regional()

  region <- region_class$return_regional()
  return(region)
}
