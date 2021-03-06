#' Get Linelist Data
#'
#' @description This function downloads the latest linelist. As this linelist
#' is experiencing a high user demand it may not always be available.
#' @param clean Logical, defaults to `TRUE`. Should the data returned be
#'  cleaned for use.
#' @param report_delay_only Logical, defaults to `FALSE`. Should only certain
#'  variables (id, country, onset date, days' delay), and observations
#'  (patients with a report delay) be returned
#' @importFrom dplyr if_else select mutate filter
#' @importFrom lubridate dmy
#' @importFrom tibble as_tibble
#' @return A linelist of case data
#' @export
#' @examples
#' \dontrun{
#' # Get the complete linelist
#' get_linelist()
#'
#' # Return the report delay only
#' get_linelist(report_delay_only = TRUE)
#' }
get_linelist <- function(clean = TRUE, report_delay_only = FALSE) {
  tmpdir <- tempdir()
  linelist <- try(csv_reader(file.path(tmpdir, "latestdata.csv")))

  if (any(class(linelist) %in% "try-error")) {
    message("Downloading linelist")
    base_url <- "https://github.com/beoutbreakprepared/nCoV2019/raw/master/"
    url <- paste0(base_url, "latest_data/latestdata.tar.gz")
    download.file(url, destfile = file.path(tmpdir, "tmp.tar.gz"))
    untar(file.path(tmpdir, "tmp.tar.gz"), files = "latestdata.csv",
                    exdir = tmpdir)
    linelist <- try(csv_reader(file.path(tmpdir, "latestdata.csv")))

    if (any(class(linelist) %in% "try-error")) {
      if (nrow(linelist) == 1) {
        stop("Problem reading linelist")
      } else {
        stop("Problem getting linelist source")
      }
    }
  }

  if (clean) {
    linelist <- linelist %>%
      mutate(
        date_confirm = suppressWarnings(dmy(date_confirmation)),
        date_onset = suppressWarnings(dmy(date_onset_symptoms)),
        date_admission_hospital = suppressWarnings(
          dmy(date_admission_hospital)),
        date_death_or_discharge = suppressWarnings(
          dmy(date_death_or_discharge)),
        death = ifelse(outcome %in% c("dead", "death", "died",
                                      "deceases", "Dead", "Death",
                                      "Died", "Deceased"),
                                      TRUE, FALSE),
        delay_onset_report = as.integer(as.Date(date_confirm) -
                              as.Date(date_onset)),
        delay_onset_admission = as.integer(as.Date(date_admission_hospital) -
                                  as.Date(date_onset)),
        delay_onset_death = ifelse(death == TRUE,
          as.integer(as.Date(date_death_or_discharge) - as.Date(date_onset)),
          NA
        )
      ) %>%
      select(
        id = .data$ID, .data$country, .data$death,
        .data$date_onset, .data$date_confirm, .data$date_admission_hospital,
        .data$date_death_or_discharge, .data$delay_onset_report,
        .data$delay_onset_admission, .data$delay_onset_death)
  }

  if (report_delay_only) {
    linelist <- filter(
      linelist,
      !is.na(.data$delay_onset_report)
    ) %>%
      select(.data$id, .data$country, .data$date_onset,
             .data$delay_onset_report)
  }
  message("Note: This linelist covers January to June 2020. We will update when
          a new data source becomes available.")
  return(linelist)
}
