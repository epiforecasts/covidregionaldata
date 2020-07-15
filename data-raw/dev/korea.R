#' South Korea regional cases
#'
#' @description Extract regional case counts for South Korea.
#' [Source](http://ncov.mohw.go.kr/en).
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @importFrom dplyr filter mutate
#' @importFrom tibble as_tibble
#' @examples
#'
#'\dontrun{
#' # Mapping
#' data <- get_korea_regional_cases()
#'
#' regions <- rnaturalearth::ne_states("South Korea", returnclass = "sf")
#'
#' regions_with_data <- regions %>%
#' mutate(name_de = str_replace_all(name_de, "Jeju-do", "Jeju")) %>%
#' left_join(data, by = c("name_de" = "region"))
#'
#'
#' ggplot(regions_with_data) +
#' geom_sf(aes(fill = cases))
#'}
#'
#' get_korea_regional_cases

get_korea_regional_cases <- function() {
  webpage <- xml2::read_html("http://ncov.mohw.go.kr/en/bdBoardList.do?brdId=16&brdGubun=162&dataGubun=&ncvContSeq=&contSeq=&board_id=")
   # Set up cache
  ch <- memoise::cache_filesystem(".cache")
  mem_read <- memoise::memoise(xml2::read_html, cache = ch)
  # Read region & cases
   region <- webpage %>%
    rvest::html_nodes("tbody tr+ tr th") %>%
    rvest::html_text()

  cases <- webpage %>%
    rvest::html_nodes("tr+ tr th+ .number") %>%
    rvest::html_text()

  cases <- cbind(region, cases)

  cases <- tibble::as_tibble(cases) %>%
    dplyr::filter(region != "Lazaretto") %>%
    dplyr::mutate(cases = as.numeric(as.character(cases)))

return(cases)
}



