#' Get national-level data for countries globally, sourced from the ECDC or WHO.
#'
#' @description Gets raw data using the source-specific function. Includes ISO
#'  country codes. Then adds columns which were missing from the raw data
#'  (calculating cumulative counts from new dailies and vice versa), cleans
#'  and sanitises further. Adds rows and columns of NA values so that data is
#'  in a standard format.
#'
#' @param country A character string specifying the country to get data from.
#' Not case dependent. Name should be the English name. For a list of
#' options use `get_available_datasets`.
#' @param source A character string specifying the data source: "WHO", or
#'  "ECDC". Not case dependent. Defaults to WHO.
#' @param ... additional arguments to pass to individual sources.
#' @return A tibble with data related to cases, deaths, hospitalisations,
#'  recoveries and testing.
#' @inheritParams get_regional_data
#' @importFrom dplyr group_by arrange select ungroup do everything
#' @importFrom tidyr drop_na fill
#' @importFrom countrycode countryname
#' @export
#' @examples
#' \dontrun{
#' # set up a data cache
#' start_using_memoise()
#'
#' # download data for Canada keeping all processing steps
#' get_national_data(country = "canada", source = "ecdc", steps = TRUE)
#' }
get_national_data <- function(country, source = "who", steps = FALSE,
                              verbose = TRUE, return_class = FALSE,
                              ...) {

  # format source name
  source <- toupper(source)

  # check data availability and initiate country class if avaliable
  nation_class <- check_country_available(
    country = source, level = "1",
    totals = FALSE, localise = TRUE,
    verbose = verbose, steps = steps, ...
  )

  # download and cache raw data
  nation_class$download()

  # dataset specifc cleaning
  nation_class$clean()

  # filter for country of interest
  if (!missing(country)) {
    tar_country <- country
    tar_country <- countryname(tar_country, destination = "country.name.en")
    if (is.na(tar_country)) {
      stop("Country name not recognised. Please enter a character string, with
            no abbreviation.")
    }
    nation_class$region$clean <- filter(
      nation_class$region$clean,
      .data$region_level_1 %in% tar_country
    )
  }

  # non-specific cleaning and checks
  nation_class$process()

  if (return_class) {
    return(nation_class)
  } else {
    nation <- nation_class$return()
    return(nation)
  }
}
