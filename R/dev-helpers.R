# Development helpers

#' Add useMemoise to options
#' @description Adds useMemoise to options meaning memoise is used when reading data in
start_using_memoise <- function() {
  options("useMemoise" = TRUE)
}

#' Stop using useMemoise
#' @description Sets useMemoise in options to NULL, meaning memoise isn't used when reading data in
stop_using_memoise <- function() {
  if (!is.null(options("useMemoise"))) {
    options("useMemoise" = NULL)
  }
}



