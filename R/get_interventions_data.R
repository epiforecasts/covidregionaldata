#' Get ACAPS Government Interventions dataset
#'
#' @description
#'  `r lifecycle::badge("deprecated")`
#'
#'  Downloads the ACAPS Government Interventions dataset.
#'  This function is deprecated: data are no longer updated as of December 2020.
#'
#'  Over 100 alternative datasets are available, covering government
#'  interventions worldwide. Several include subnational level policy.
#'  See: https://supertracker.spi.ox.ac.uk/policy-trackers/
#'
#' @return a dataframe of government interventions up to Dec 2020 from ACAPS
#' @source \url{https://www.acaps.org/covid-19-government-measures-dataset}
#' @author Paul Campbell @paulcampbell91
#'
#' @keywords internal
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate
#' @importFrom utils download.file
#' @importFrom lifecycle deprecate_warn
#'
get_interventions_data <- function() {
  deprecate_warn(
    when = "0.9.0",
    what = "covidregionaldata::get_interventions_data()",
    details = c(
      "The ACAPS data source stopped updating in December 2020.",
      "Over 100 alternative data sets are available at:",
      "https://supertracker.spi.ox.ac.uk/policy-trackers/"
    )
  )

  base_url <- "https://www.acaps.org/sites/acaps/files/resources/files/acaps_covid19_government_measures_dataset_0.xlsx" # nolint
  temp <- tempdir()
  filename <- "interventions.xlsx"
  download.file(base_url,
    destfile = file.path(temp, filename),
    mode = "wb", quiet = TRUE
  )
  # Read in data and correct excel dates
  data <- suppressWarnings(read_excel(file.path(temp, filename),
    sheet = "Dataset",
    col_types = "text"
  )) %>%
    mutate(
      ENTRY_DATE = as.Date((as.numeric(.data$ENTRY_DATE) - 2),
        origin = as.Date("1900-01-01")
      ),
      DATE_IMPLEMENTED = as.Date((as.numeric(.data$DATE_IMPLEMENTED) - 2),
        origin = as.Date("1900-01-01")
      )
    )
  names(data) <- tolower(names(data))

  return(data)
}
