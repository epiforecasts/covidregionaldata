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
#' @param include_level_2_regions `r lifecycle::badge("deprecated")` Boolean. If TRUE, returns data stratified by
#'  level 2 regions. If FALSE, stratified by Level 1. Note that Level 2 region
#'  data is not always available. In these cases the user will get a warning
#'  and the Level 1 data will be returned.
#' @param localise_regions `r lifecycle::badge("deprecated")` Logical, defaults to TRUE. Should region names be localised.
#' @inheritParams return_data
#' @inheritParams process_internal
#' @param ... additional arguments to pass to country specific functionality.
#' @return A tibble with data related to cases, deaths, hospitalisations,
#' recoveries and testing stratified by regions within the given country.
#' @importFrom lifecycle deprecated is_present deprecate_warn
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
                              localise = TRUE, steps = FALSE,
                              class = FALSE, verbose = TRUE,
                              include_level_2_regions = deprecated(),
                              localise_regions = deprecated(),
                              ...) {
  if (is_present(include_level_2_regions)) {
    deprecate_warn(
      "0.9.0",
      "covidregionaldata::get_regional_data(include_level_2_regions = )", "covidregionaldata::get_regional_data(level = )"
    )
    if (include_level_2_regions) {
      level <- "2"
    } else {
      level <- "1"
    }
  }

  if (is_present(localise_regions)) {
    deprecate_warn(
      "0.9.0",
      "covidregionaldata::get_regional_data(localise_regions = )", "covidregionaldata::get_regional_data(localise = )"
    )
    localise <- localise_regions
  }

  # format country string
  country <- paste0(
    toupper(substr(country, 1, 1)),
    tolower(substr(country, 2, nchar(country)))
  )

  # check data availability and initiate country class if available
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

  return(return_data(region_class,
    class = class
  ))
}
