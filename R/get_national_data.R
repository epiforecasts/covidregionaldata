#' Get national-level data for countries globally, sourced from the ECDC or WHO.
#'
#' @description Gets raw data using the source-specific function. Includes ISO
#'  country codes. Then adds columns which were missing from the raw data
#'  (calculating cumulative counts from new dailies and vice versa), cleans
#'  and sanitises further. Adds rows and columns of NA values so that data is
#'  in a standard format.
#'
#' @param country A character string specifying the country to get data from.
#'  Not case or language dependent. Defaults to all countries.
#' @param source A character string specifying the data source: "WHO", or
#'  "ECDC". Not case dependent. Defaults to WHO.
#' @return A tibble with data related to cases, deaths, hospitalisations,
#'  recoveries and testing.
#' @inheritParams get_regional_data
#' @importFrom dplyr group_by arrange select ungroup do everything
#' @importFrom tidyr drop_na fill
#' @importFrom countrycode countryname
#' @export
#' @examples
#' \dontrun{
#' get_national_data(country = "canada", source = "WHO", totals = TRUE)
#' }
#'
get_national_data <- function(country, source = "who", totals = FALSE,
                              steps = FALSE, verbose = TRUE) {

  # check data availability and define list
  source <- new_covidregionaldata(source, level = "1", verbose = verbose)

  # download and cache raw data
  source <- download_regional(source, verbose = verbose)

  # filter for country of interest
  if (!missing(country)) {
    tar_country <- country
    tar_country <- countryname(tar_country, destination = "country.name.en")
    if (is.na(tar_country)) {
      stop("Country name not recognised. Please enter a character string, with
            no abbreviation.")
    }
    source$raw <- filter(source$raw,
                         .data$region_level_1 %in% tar_country)
  }

  # dataset specifc cleaning
  source <- clean_regional(source, verbose = verbose)

  # non-specific cleaning and checks
  source <- process_regional(source, totals = totals,
                             localise = TRUE, verbose = verbose)

  source <- return_regional(source, steps = steps)
  return(region)
  }
  