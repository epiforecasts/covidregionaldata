#' Pipe operator
#'
#' @description See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' @importFrom rlang .data
rlang::`.data`

#' Custom CSV reading function
#'
#' @description Checks for use of memoise and then uses vroom::vroom.
#' @param file A URL or filepath to a CSV
#' @param ... extra parameters to be passed to vroom::vroom
#' @return A data table
#' @importFrom memoise memoise cache_filesystem
#' @importFrom vroom vroom
#' @importFrom tibble tibble
csv_reader <- function(file, ...) {
  read_csv_fun <- vroom

  if (!is.null(getOption("useMemoise"))) {
    if (getOption("useMemoise")) {
      # Set up cache
      ch <- cache_filesystem(".cache")
      read_csv_fun <- memoise(vroom, cache = ch)
    }
  }
  data <- read_csv_fun(file, ..., guess_max = 500)
  return(tibble(data))
}

#' Custom CSV reading function
#'
#' @description Checks for use of memoise and then uses readr::read_csv,
#' which appears more robust in loading some streams
#' @inheritParams csv_reader
#' @param ... extra parameters to be passed to readr::read_csv
#' @return A data frame
#' @importFrom memoise memoise cache_filesystem
#' @importFrom tibble tibble
#' @importFrom readr read_csv
csv_readr <- function(file, ...) {
  read_csv_fun <- read_csv

  if (!is.null(getOption("useMemoise"))) {
    if (getOption("useMemoise")) {
      # Set up cache
      ch <- cache_filesystem(".cache")
      read_csv_fun <- memoise(read_csv, cache = ch)
    }
  }

  data <- read_csv_fun(file, ...)
  return(tibble(data))
}

#' Add useMemoise to options
#'
#' @description Adds useMemoise to options meaning memoise is
#' used when reading data in.
#' @export
start_using_memoise <- function() {
  options("useMemoise" = TRUE)
}

#' Stop using useMemoise
#'
#' @description Sets useMemoise in options to NULL, meaning memoise isn't used
#' when reading data in
#' @export
stop_using_memoise <- function() {
  if (!is.null(options("useMemoise"))) {
    options("useMemoise" = NULL)
  }
}

#' Reset Cache and Update all Local Data
#'
#' @return Null
#' @export
#' @importFrom memoise cache_filesystem
reset_cache <- function() {
  unlink(".cache", recursive = TRUE)
  cache_filesystem(".cache")
  return(invisible(NULL))
}

#' Control data return
#'
#' @description Controls data return for `get_reigonal_data` and
#' `get_national_data`
#' @param obj A Class based on a `DataClass`
#' @param class Logical, defaults to FALSE. If TRUE returns the
#' `Country` class object rather than a tibble or a list of tibbles.
#' Overides `steps`.
return_data <- function(obj, class = FALSE) {
  if (class) {
    return(obj)
  } else {
    obj <- obj$return()
    return(obj)
  }
}
