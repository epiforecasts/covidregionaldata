#' Afghan Provincial Daily Case Counts
#'
#' @description Data from HDX https://data.humdata.org/dataset/afghanistan-covid-19-statistics-per-province
#' The data is stored in a google sheet, which is read as a csv.
#' 
#' 
#' @author Flavio Finger @ffinger
#'
#'
#' @return A dataframe of daily Afghan provincial cases and deaths
#' @importFrom purrr map_dfr map_chr
#' @importFrom dplyr transmute arrange
#' @importFrom tidyr complete full_seq
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
      Cases = "integer",
      Deaths = "integer",
      Recoveries = "integer",
      Active.Cases = "integer",
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

  # fill gaps with NA
  data <- tidyr::complete(data,
    date = tidyr::full_seq(date, period  = 1),
    country,
    province,
    fill = list(
        cases = NA_integer_,
        deaths = NA_integer_,
        recoveries = NA_integer_
        )
    )

  # arrange
  data <- dplyr::arrange(data, date, country, province)

  return(data)
}