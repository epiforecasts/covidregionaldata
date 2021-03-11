#' Get countries data is avaliable for
#'
#' @description Show what countries have what level data avaliable.
#' The function uses the region_codes file to find countries.
#' @return A tibble of avaliable countries and the region level
#' data is avaliable for
#' @importFrom dplyr filter %>% mutate
#' @importFrom tidyr pivot_wider
#' @export
#'
show_countries <- function() {
  exclude <- c("who", "ecdc")
  countries <- covidregionaldata::region_codes %>%
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

show_R6_classes <- function(){
  envi <-ls("package:covidregionaldata")
  starts_with_capitals_idx <- grep("^[A-Z][a-z]", envi)
  starts_with_capitals <- envi[starts_with_capitals_idx]
  exclude <- c("Who", "Ecdc", "NameOfCountry")
  check <- lapply(starts_with_capitals, function(x) {class(get(x))=="R6ClassGenerator" & !(x %in% c(exclude))})
  valid_country_classes <- starts_with_capitals[unlist(check)]
  public_field_names <- lapply(valid_country_classes, function(x) {!(is.na(get(x)$public_fields["level_2_region"]))})
  level_2_avaliable <- valid_country_classes[unlist(public_field_names)]
  # Format the level 1 and level 2 into a dataframe like show countries.
  
  return(level_2_avaliable)
}