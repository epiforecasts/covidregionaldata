#' Get patient line list data
#'
#' @description
#'  `r lifecycle::badge("deprecated")`
#'
#'  Provides a public international patient line list from January 2020 to June
#'  2020.
#'
#'  This version of the line list has stopped updating. The new version of the
#'  line list is free but requires a login.
#'
#'  See: https://global.health/
#'
#' @param clean Logical, defaults to `TRUE`.
#'  Should the data returned be cleaned for use.
#' @param report_delay_only Logical, defaults to `FALSE`.
#'  Should only certain variables (id, country, onset date, days' delay),
#'  and observations (patients with a report delay) be returned
#' @return A line list of reported cases of COVID-19
#' @source \url{https://github.com/beoutbreakprepared/nCoV2019}
#'
#' @keywords internal
#'
#' @importFrom lifecycle deprecate_warn
#' @importFrom dplyr if_else select mutate filter
#' @importFrom lubridate dmy
#' @importFrom tibble as_tibble
#' @importFrom utils download.file untar
#' @examples
#' \dontrun{
#' # Get the complete linelist
#' get_linelist()
#'
#' # Return the report delay only
#' get_linelist(report_delay_only = TRUE)
#' }
#'
get_linelist <- function(clean = TRUE, report_delay_only = FALSE) {
  deprecate_warn(
    when = "0.9.0",
    what = "covidregionaldata::get_linelist()",
    details = c(
      "Linelist no longer accessible through this package.",
      "The line list is now hosted at: https://global.health/"
    )
  )

  tmpdir <- tempdir()
  linelist <- try(csv_reader(file.path(tmpdir, "latestdata.csv")))

  if (any(class(linelist) %in% "try-error")) {
    message("Downloading linelist")

    url <- "https://github.com/beoutbreakprepared/nCoV2019/raw/master/latest_data/latestdata.tar.gz" # nolint
    download.file(url, destfile = file.path(tmpdir, "tmp.tar.gz"))
    untar(file.path(tmpdir, "tmp.tar.gz"),
      files = "latestdata.csv", exdir = tmpdir
    )

    linelist <- try(csv_reader(file.path(tmpdir, "latestdata.csv")))

    if (any(class(linelist) %in% "try-error")) {
      if (nrow(linelist) == 1) {
        stop("Problem reading linelist")
      } else {
        stop("Problem getting linelist source")
      }
    }
  }

  if (clean | report_delay_only) {
    linelist <- linelist %>%
      mutate(
        date_confirm = suppressWarnings(lubridate::dmy(.data$date_confirmation)),
        date_onset = suppressWarnings(lubridate::dmy(.data$date_onset_symptoms)),
        date_admission_hospital =
          suppressWarnings(lubridate::dmy(.data$date_admission_hospital)),
        date_death_or_discharge =
          suppressWarnings(lubridate::dmy(.data$date_death_or_discharge)),
        death = ifelse(.data$outcome %in% c(
          "dead", "death", "died",
          "deceases", "Dead", "Death",
          "Died", "Deceased"
        ), TRUE, FALSE),
        delay_onset_report =
          as.integer(as.Date(.data$date_confirm) - as.Date(.data$date_onset)),
        delay_onset_admission =
          as.integer(as.Date(.data$date_admission_hospital) -
            as.Date(.data$date_onset)),
        delay_onset_death = ifelse(.data$death == TRUE,
          as.integer(as.Date(.data$date_death_or_discharge) -
            as.Date(.data$date_onset)),
          NA
        )
      ) %>%
      select(
        id = .data$ID, .data$country, .data$death,
        .data$date_onset, .data$date_confirm,
        .data$date_admission_hospital, .data$date_death_or_discharge,
        .data$delay_onset_report, .data$delay_onset_admission,
        .data$delay_onset_death
      )
  }

  if (report_delay_only) {
    linelist <- dplyr::filter(
      linelist,
      !is.na(.data$delay_onset_report)
    ) %>%
      dplyr::select(
        .data$id, .data$country,
        .data$date_onset, .data$delay_onset_report
      )
  }

  message("Note: This line list covers January to June 2020. We will update when a new data source becomes available.")

  return(linelist)
}
