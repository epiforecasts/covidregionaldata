#' ECDC International Case Counts
#'
#' @return A dataframe of International case counts published by ECDC.
#' @export
#' @inheritParams get_international_linelist
#' @importFrom gdata read.xls
#' @importFrom memoise cache_filesystem memoise
#' @importFrom dplyr mutate select filter arrange
#' @importFrom tibble as_tibble
#' @examples
#'
#'
#' ## Get data for france
#' get_ecdc_cases(countries = "France")
#'
#' ## Code
#' get_ecdc_cases
get_ecdc_cases <- function(countries = NULL){

  #url to ecdc api
  base_url = 'http://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-'

  #set up cache
  ch <- memoise::cache_filesystem(".cache")

  mem_read <- memoise::memoise(gdata::read.xls, cache = ch)

  #api may provide file for yesterday's or today's date - it depends on what time it is being accessed
  try_dates <- c(as.Date(Sys.time()) - 1, as.Date(Sys.time()))

  for (i in 1:length(try_dates)) {

    date <- try_dates[i]
    url <- paste0(base_url, date, '.xls')
    d <- suppressWarnings(try(tibble::as_tibble(mem_read(url)), silent = TRUE))

    # if try-error, try again with extension .xlsx
    if ("try-error" %in% class(d)) {
      url <- paste0(base_url, date, '.xlsx')
      d <- suppressWarnings(try(tibble::as_tibble(mem_read(url)), silent = TRUE))
    }

    if (!"try-error" %in% class(d)) { break }

  }

  if (length(d) == 1) {
    stop(paste0('No data found at: ', url,
                '\n Tried dates: ', try_dates[1], ', ', try_dates[2]))
  }

  d <- d %>%
    dplyr::mutate(date = as.Date(DateRep),
                  cases = Cases,
                  deaths = Deaths,
                  country = Countries.and.territories,
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
