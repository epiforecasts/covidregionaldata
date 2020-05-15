#' Afghan Provincial Daily Case Counts
#'
#' @description Data from HDX https://data.humdata.org/dataset/afghanistan-covid-19-statistics-per-province
#' The cumulative data is stored in a google sheet, which is read as a csv and de-cumulated.
#' 
#' 
#' @author Flavio Finger @ffinger
#'
#'
#' @return A dataframe of daily Afghan provincial cases and deaths
#' @importFrom purrr map_dfr map_chr
#' @importFrom dplyr transmute arrange mutate group_by ungroup
#' @importFrom tidyr complete full_seq fill
#' @importFrom stringr str_remove_all
#' @export
#' @examples
#'
#' ## Code
#' get_afghan_regional_cases()

get_afghan_regional_cases <- function() {
  
  url <- "https://docs.google.com/spreadsheets/d/1F-AMEDtqK78EA6LYME2oOsWQsgJi4CT3V_G4Uo-47Rg/export?format=csv"

  #download
  cls <- c(
      Province = "character",
      Cases = "character",
      Deaths = "character",
      Recoveries = "character",
      Active.Cases = "character",
      Date = "Date"
  )
  data <- read.csv(url, stringsAsFactors = FALSE, colClass = cls)

  #reformat
  data <- dplyr::transmute(data,
    date = Date,
    country = "Afghanistan",
    province = stringr::str_replace(Province, " Province", ""),
    cases = Cases,
    deaths = Deaths,
    recovered = Recoveries
    )

  #transform (remove commas in numbers)
  data <- dplyr::mutate(data,
    cases = stringr::str_remove_all(cases, ","),
    cases = as.integer(cases),
    deaths = stringr::str_remove_all(deaths, ","),
    deaths = as.integer(deaths),
    recovered = stringr::str_remove_all(recovered, ","),
    recovered = as.integer(recovered)
  )

  # put NA where gaps
  data <- tidyr::complete(data,
    date = tidyr::full_seq(date, period  = 1),
    country,
    province,
    fill = list(
        cases = NA_integer_,
        deaths = NA_integer_,
        recovered = NA_integer_
        )
    )

  # fill NA with previous values
  data <- dplyr::ungroup(tidyr::fill(
      dplyr::group_by(data, country, province),
      cases,
      deaths,
      recovered
    ))

  # de-cumulate
  data <- dplyr::ungroup(dplyr::mutate(
      dplyr::group_by(data, country, province),
      cases = c(0, diff(cases)),
      deaths = c(0, diff(deaths)),
      recovered = c(0, diff(recovered))
    ))

  # arrange
  data <- dplyr::arrange(data, date, country, province)

  return(data)
}
