#' Download the most recent WHO case data
#'
#' @description Downloads the latest WHO case data.It uses `memoise` to cache the
#' results locally. To clear the cache and redownload the data use `reset_cache`. The cache will be located
#' in the directory at which the function is run.
#' @param country Character string of current name
#' @param daily Logical, defaults to `FALSE`. Should cases
#' @param cache A `memoise` cache.
#' counts be daily (`TRUE`) or cumulative (`FALSE`).
#'
#' @return A datatable of either all WHO data by date or cases in a specific country by date
#' @export
#' @importFrom data.table fread
#' @importFrom purrr safely
#' @importFrom dplyr mutate_at lag mutate
#' @importFrom memoise memoise cache_filesystem
#' @examples
#'
#' get_who_cases
get_who_cases <- function(country = NULL, daily = FALSE, cache = NULL) {

  ch <- memoise::cache_filesystem(".cache")

  mem_fread <- memoise::memoise(data.table::fread, cache = ch)

  who_cases <- mem_fread("https://raw.githubusercontent.com/eebrown/data2019nCoV/master/data-raw/WHO_SR.csv")

  ## Make sure the date is correctly identified as a date
  who_cases <- dplyr::mutate(who_cases, Date = as.Date(Date))

  if (!is.null(country)) {
    who_cases <- who_cases[, c("Date", country), with = FALSE]
    colnames(who_cases) <- c("date", "cases")

  }

  if (daily) {
    cols <- colnames(who_cases)
    cols <- cols[!colnames(who_cases) %in% c("Date", "date", "SituationReport")]
    safe_diff <- purrr::safely(diff)

    who_cases <- dplyr::mutate_at(who_cases,
                                  .vars = cols,
                                  ~ . - dplyr::lag(., default = 0))
  }

  return(who_cases)
}
