#' Afghan Provincial Daily Case Counts
#'
#' @description Data from HDX https://data.humdata.org/dataset/afghanistan-covid-19-statistics-per-province
#' The data is stored in a google sheet. Requires authentication through google account to work.
#' 
#' 
#' @author Flavio Finger @ffinger
#'
#'
#' @return A dataframe of daily Afghan provincial cases and deaths
#' @importFrom googlesheets4 read_sheet
#' @importFrom purrr map_dfr map_chr
#' @importFrom dplyr transmute arrange
#' @importFrom tidyr complete full_seq
#' @export
#' @examples
#'
#' ## Code
#' get_afghan_regional_cases()

get_afghan_regional_cases <- function() {
  
  url <- "https://docs.google.com/spreadsheets/d/1F-AMEDtqK78EA6LYME2oOsWQsgJi4CT3V_G4Uo-47Rg"

  #download
  data <- googlesheets4::read_sheet(
    url,
    sheet = 1
    )

  #reformat
  data <- dplyr::transmute(data,
    date = as.Date(Date),
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
        cases = NA_real_,
        deaths = NA_real_,
        recoveries = NA_real_
        )
    )

  # arrange
  data <- dplyr::arrange(data, date, country, province)

  return(data)
}