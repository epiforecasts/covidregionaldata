#' Get countries data is avaliable for
#'
#' @description Show what countries have what level data avaliable.
#' The function uses the region_codes file to find countries.
#' @return A tibble of avaliable countries and the region level
#' data is avaliable for
#' @importFrom dplyr filter %>% %in% mutate
#' @importFrom tidyr pivot_wider
#' @export
#'
show_countries <- function() {
  exclude <- c("who", "ecdc")
  countries <- region_codes %>%
    filter(!.data$country %in% exclude) %>%
    select(c("country", "level"))
  countries <- countries %>%
    tidyr::pivot_wider(names_from = "level", values_from = "level") %>%
    mutate(
      level_1_region = ifelse(!(is.na(.data$level_1_region)), "Y", "N")
    ) %>%
    mutate(
      level_2_region = ifelse(!(is.na(.data$level_2_region)), "Y", "N")
    )
  return(countries)
}
