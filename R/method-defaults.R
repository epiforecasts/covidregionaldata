#' Default regional data downloading
#'
#' @description Default download method. Causes an error if called.
#' @export
#' @inheritParams download_regional
#' @method download_regional default
#' @author Sam Abbott
#' @examples
#' region <- new_covidregionaldata("uk", "1")
#' class(region) <- "list"
#' download_regional(region, verbose = FALSE)
download_regional.default <- function(region, verbose = TRUE, ...) {
  if (verbose) {
    warning(
        "A download function has not been implemented for this datasource"
        )
  }
  region$raw <- NA
  return(region)
}

#' Default region specific data cleaning
#'
#' @description Default regional cleaning method. Highlights that no custom
#' cleaning is happening.
#' @export
#' @inheritParams clean_regional
#' @method clean_regional default
#' @author Sam Abbott
#' region <- new_covidregionaldata("uk", "1")
#' class(region) <- "list"
#' region <- download_regional(region, verbose = FALSE)
#' clean_regional(region, verbose = FALSE)
clean_regional.default <- function(region, verbose = TRUE, ...) {
  if (verbose) {
    warning(
        "A cleaning function has not been implemented for this data source"
        )
  }
  region$clean <- region$raw
  return(region)
}
