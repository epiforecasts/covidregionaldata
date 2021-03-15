#' Get the countries data is avaliable for
#'
#' @description Show what countries have what level data avaliable.
#' The function searches the environment for R6 class objects and 
#' extracts the country name and what level it has from the object.
#' @return A list of avaliable countries and the region level
#' data is avaliable for
#' @export
#'
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
  level_1 <- data.frame(valid_country_classes, "Y", stringsAsFactors = FALSE)
  colnames(level_1) <- c("county", "level_1")
  level_2 <- data.frame(level_2_avaliable, "Y", stringsAsFactors = FALSE)
  colnames(level_2) <- c("county", "level_2")
  countries_level_avaliable <- merge(level_1, level_2, by = "county", all=TRUE)
  countries_level_avaliable[is.na(countries_level_avaliable)] <- "N"
  return(countries_level_avaliable)
}