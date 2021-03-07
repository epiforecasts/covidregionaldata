#' Import ACAPS Government Interventions dataset
#'
#' Data available here:
#' https://data.humdata.org/dataset/acaps-covid19-government-measures-dataset#
#'
#' @author Paul Campbell @paulcampbell91
#'
#' @return a dataframe of government interventions compiled by ACAPS
#'
#' @importFrom memoise cache_filesystem memoise
#' @importFrom xml2 read_html url_absolute
#' @importFrom rvest html_node html_attr
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate
#' @importFrom utils download.file
#'
#' @export
#' @examples
#' \dontrun{
#' get_interventions_data()
#' }
get_interventions_data <- function() {

if (!is.null(getOption("useMemoise"))) {
    if (getOption("useMemoise")) {
      # Set up cache
      ch <- cache_filesystem(".cache")
      mem_download <- memoise(download.file, cache = ch)
    }
  }
  if (is.null(mem_download)) {
    mem_download <- download.file
  }

  base_url <- "https://data.humdata.org"
  dl_url <- read_html(
    paste0(base_url, "/dataset/acaps-covid19-government-measures-dataset#")) %>%
    html_node(css = ".resource-item+ .resource-item .ga-download") %>%
    html_attr("href") %>%
    url_absolute(base_url)

  temp <- tempdir()
  filename <- "interventions.xlsx"
  mem_download(dl_url, destfile = file.path(temp, filename),
               mode = "wb", quiet = TRUE)

  # Read in data and correct excel dates
  data <- suppressWarnings(read_excel(file.path(temp, filename),
                           sheet = "Dataset", col_types = "text") %>%
    mutate(
      ENTRY_DATE = as.Date((as.numeric(.data$ENTRY_DATE) - 2),
                     origin = as.Date("1900-01-01")),
      DATE_IMPLEMENTED = as.Date((as.numeric(.data$DATE_IMPLEMENTED) - 2),
                            origin = as.Date("1900-01-01"))
    ))
  names(data) <- tolower(names(data))
  return(data)
}
