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
#' @param guess_max Maximum number of records to use for guessing column types.
#' Defaults to a 1000.
#' @param ... extra parameters to be passed to vroom::vroom
#' @inheritParams message_verbose
#' @return A data table
#' @importFrom memoise memoise cache_filesystem
#' @importFrom vroom vroom
#' @importFrom tibble tibble
csv_reader <- function(file, verbose = FALSE, guess_max = 1000, ...) {
  read_csv_fun <- vroom

  if (!is.null(getOption("useMemoise"))) {
    if (getOption("useMemoise")) {
      ch <- cache_filesystem(getOption("cache_path"))
      read_csv_fun <- memoise(vroom, cache = ch)
    }
  }
  if (verbose) {
    message("Downloading data from ", file)
    data <- read_csv_fun(file, ..., guess_max = guess_max)
  } else {
    data <- suppressWarnings(
      suppressMessages(
        read_csv_fun(file, ..., guess_max = guess_max)
      )
    )
  }
  return(tibble(data))
}

#' Wrapper for message
#'
#' @description A wrapper for `message` that only prints output when
#' `verbose = TRUE`.
#' @param verbose Logical, defaults to `TRUE`. Should verbose processing
#' messages and warnings be returned.
#' @param ... Additional arguments passed to `message`.
message_verbose <- function(verbose = TRUE, ...) {
  if (verbose) {
    message(...)
  }
  return(invisible(NULL))
}
#' Add useMemoise to options
#' @param path Path to cache directory, defaults to a temporary directory.
#' @inheritParams message_verbose
#' @description Adds useMemoise to options meaning memoise is
#' used when reading data in.
#' @export
start_using_memoise <- function(path = tempdir(), verbose = TRUE) {
  message_verbose(verbose, "Using a cache at: ", path)
  options("useMemoise" = TRUE, cache_path = path)
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
  unlink(getOption("cache_path"), recursive = TRUE)
  cache_filesystem(getOption("cache_path"))
  return(invisible(NULL))
}

#' Control data return
#'
#' @description Controls data return for `get_reigonal_data` and
#' `get_national_data`
#' @param obj A Class based on a `DataClass`
#' @param class Logical, defaults to FALSE. If TRUE returns the
#' `Country` class object rather than a tibble or a list of tibbles.
#' Overrides `steps`.
return_data <- function(obj, class = FALSE) {
  if (class) {
    return(obj)
  } else {
    obj <- obj$return()
    return(obj)
  }
}
