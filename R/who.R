#' Download the most recent WHO case data
#'
#' @description Downloads the latest WHO case data.
#'
#' @return A tibble of all WHO data by date
#' @export
#' @importFrom jsonlite from JSON
#' @importFrom here here
#' @importFrom dplyr 
#' @examples
#'
#' get_who_cases

get_who_cases <- function() {

  if (!is.null(getOption("useMemoise"))) {
    if (getOption("useMemoise")) {
      # Set up cache
      ch <- memoise::cache_filesystem(".cache")
      read_fun <- memoise::memoise(jsonlite::fromJSON, cache = ch)
    }
  }
  
 json_url <- "https://dashboards-dev.sprinklr.com/data/9043/global-covid19-who-gis.json"

  who <- jsonlite::fromJSON(json_url, flatten = F)

  who <- as.data.frame(who$rows)
 
  colnames(who) <- c("date", "iso_code", "who_region", "cases_new", "cases_total", "deaths_new", "deaths_total")

  cols <- c(1, 4:7)
  
  who[,cols] <- sapply(who[,cols], as.character)
  who[,cols] <- sapply(who[,cols], as.numeric)
  
  who$date <- as.Date(as.POSIXct(who$date / 1000, origin="1970-01-01"))

  name_match <- read.csv(here::here("data-raw", "country_names.csv"), 
                            colClasses = "character")
  
  colnames(name_match)[1] = "iso_code"
  
  who <- who %>%
    dplyr::left_join(name_match %>% dplyr::select(iso_code, country), by = "iso_code") %>%
    dplyr::mutate(country = ifelse(iso_code == "NA" & is.na(country), "Namibia", country))
  
  return(who)
}
