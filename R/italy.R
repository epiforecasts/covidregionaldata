#' Download Italian Regional Daily COVID-19 Count Data
#'
#' @description Extracts daily COVID-19 data for Italy, stratified by .
#' Data available at .
#' @inheritParams download_regional
#' @method download_regional crd_italy_1
#' @author Sam Abbott
#' @export
#' @examples
#' \dontrun{
#' italy <- new_covidregionaldata("italy")
#' italy <- download_regional(italy)
#' italy$raw
#' }
download_regional.crd_italy_1 <- function(region, verbose = TRUE, ...) {
  region$raw <- NULL
  return(region)
}

#' Italy Specific State Level Data Cleaning
#'
#' @export
#' @inheritParams clean_regional
#' @method clean_regional crd_italy_1
#' @author Sam Abbott
#' @examples
#' \dontrun{
#' italy <- new_covidregionaldata("italy")
#' italy <- download_regional(italy)
#' clean_regional(italy)$clean
#' }
clean_regional.crd_italy_1 <- function(region, verbose = TRUE, ...) {
  region$clean <- region$raw
  return(region)
}
