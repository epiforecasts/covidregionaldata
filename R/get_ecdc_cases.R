#' ECDC International Case Counts
#'
#' @return A dataframe of International case counts published by ECDC.
#' @export
#' @inheritParams get_international_linelist
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate select filter arrange
#' @importFrom lubridate is.POSIXt
#' @examples
#'
#'
#' ## Get data for france
#' get_ecdc_cases(countries = "France")
#'
#' ## Code
#' get_ecdc_cases
get_ecdc_cases <- function(countries = NULL){

  # url to ecdc api (note typo in "distribution" is from ECDC)
  base_url <- 'http://www.ecdc.europa.eu/sites/default/files/documents/'
  base_file <- "COVID-19-geographic-disbtribution-worldwide-"

  # temporary directory to download files
  temp <- tempdir()

  # api may provide file for yesterday's or today's date - it depends on what time it is being accessed
  try_dates <- c(as.Date(Sys.time()), as.Date(Sys.time()) - 1)

  # try each combination of date/extension until successful
  for (i in 1:length(try_dates)) {

    date <- try_dates[i]
    filename <- paste0(base_file, try_dates[i], ".xls")
    url <- paste0(base_url, filename)
    dl <- suppressWarnings(try(download.file(url, file.path(temp, filename)), silent = TRUE))

    # if try-error, try again with extension .xlsx
    if (class(dl) == "try-error") {
      filename <- paste0(base_file, try_dates[i], ".xlsx")
      url <- paste0(base_url, filename)
      dl <- suppressWarnings(try(download.file(url, file.path(temp, filename)), silent = TRUE))
    }

    if (class(dl) != "try-error") { break }
  }

  if (class(dl) == "try-error") {
    stop(paste0('No data found at: ', url,
                '\n Tried dates: ', try_dates[1], ', ', try_dates[2]))
  }

  d <- readxl::read_excel(file.path(temp, filename)) %>%
    dplyr::mutate_if(lubridate::is.POSIXt, as.Date) %>%
    dplyr::mutate(date = as.Date(DateRep),
                  cases = Cases,
                  deaths = Deaths,
                  country = `Countries and territories`,
                  geoid = GeoId) %>%
    dplyr::select(date, cases, deaths, country, geoid) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(cases = ifelse(cases < 0, 0, cases))

  if (!is.null(countries)) {
    d <- d %>%
      dplyr::filter(country %in% countries)
  }

  return(d)

}
