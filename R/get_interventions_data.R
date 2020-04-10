#' Import ACAPS Government Interventions dataset
#'
#' Data available here: https://data.humdata.org/dataset/acaps-covid19-government-measures-dataset#
#'
#' @author Paul Campbell @paulcampbell91
#'
#' @return a dataframe of government interventions compiled by ACAPS
#'
#' @importFrom memoise cache_filesystem memoise
#' @importFrom xml2 read_html url_absolute
#' @importFrom rvest html_node html_attr
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate_if
#' @importFrom lubridate is.POSIXct as_date
#' @importFrom utils download.file
#'
#' @export
#' @examples
#'
#' ## Code
#' get_interventions_data

get_interventions_data <- function() {

  ## Set up caching
  ch <- memoise::cache_filesystem(".cache")
  mem_download <- memoise::memoise(download.file, cache = ch)

  # the file name sometimes changes so scrape the current URL rather than hard code it
  base_url <- "https://data.humdata.org"
  dl_url <- xml2::read_html(paste0(base_url, "/dataset/acaps-covid19-government-measures-dataset#")) %>%
    rvest::html_node(css = ".ga-download") %>%
    rvest::html_attr("href") %>%
    xml2::url_absolute(base_url)

  temp <- tempdir()
  filename <- "interventions.xlsx"
  mem_download(dl_url, destfile = file.path(temp, filename), mode = 'wb', quiet = TRUE)

  data <- readxl::read_excel(file.path(temp, filename), sheet = "Database") %>%
    dplyr::mutate_if(lubridate::is.POSIXct, lubridate::as_date)
  names(data) <- tolower(names(data))
  
  return(data)
}
