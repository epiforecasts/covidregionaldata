#' Reset Cache and Update all Local Data
#'
#' @return Null
#' @export
#' @importFrom memoise cache_filesystem
#' @examples
#'
#'## Code
#'reset_cache
reset_cache <- function() {

  unlink("cache", recursive = TRUE)

  cache <- memoise::cache_filesystem(".cache")

  tmp <- NCoVUtils::get_international_linelist()

  tmp <- NCoVUtils::get_who_cases()

  return(invisible(NULL))
}

