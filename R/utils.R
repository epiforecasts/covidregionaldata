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
#' @importFrom withr with_envvar
#' @concept utility
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
    with_envvar(
      new = c("VROOM_SHOW_PROGRESS" = "false"),
      data <- suppressWarnings(
        suppressMessages(
          read_csv_fun(file, ..., guess_max = guess_max)
        )
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
#' @concept utility
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
#' @concept utility
#' @export
#' @concept utility
start_using_memoise <- function(path = tempdir(), verbose = TRUE) {
  message_verbose(verbose, "Using a cache at: ", path)
  options("useMemoise" = TRUE, cache_path = path)
}

#' Stop using useMemoise
#'
#' @description Sets useMemoise in options to NULL, meaning memoise isn't used
#' when reading data in
#' @concept utility
#' @export
#' @concept utility
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
#' @concept utility
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
#' `DataClass` object rather than a tibble or a list of tibbles.
#' Overrides `steps`.
#' @concept utility
return_data <- function(obj, class = FALSE) {
  if (class) {
    return(obj)
  } else {
    obj <- obj$return()
    return(obj)
  }
}

#' Control Grouping Variables used in process_internal
#'
#' @description Controls the grouping variables used in
#' `process_internal` based on the supported regions present in the
#' class.
#' @param level A character string indicating the current level.
#' @param all_levels A character vector indicating all the levels supported.
#' @param region_names A named list of region names named after the levels
#'  supported.
#' @param region_codes A named list of region codes named after the levels
#' supported.
#' @importFrom purrr map
#' @concept utility
region_dispatch <- function(level, all_levels, region_names, region_codes) {
  sel_levels <- all_levels[1:grep(level, all_levels)]

  region_vars <- map(sel_levels, function(l) {
    rn <- c()
    if (!is.null(region_names[[l]])) {
      rn <- c(region_names[[l]])
      names(rn) <- glue_level(l)
    }

    rc <- c()
    if (!is.null(region_codes[[l]])) {
      rc <- c(region_codes[[l]])
      names(rc) <- paste0(glue_level(l), "_code")
    }
    region_vars <- c(rn, rc)
    return(region_vars)
  })
  region_vars <- unlist(region_vars)

  region_vars <- region_vars[!is.null(region_vars)]
  region_vars <- region_vars[!is.na(region_vars)]
  return(region_vars)
}

#' Glue the spatial level into a variable name
#'
#' @inheritParams region_dispatch
#' @return A string in the form "level_1_region".
#' @concept utility
glue_level <- function(level) {
  paste0("level_", level, "_region")
}

#' Checks a given level is supported
#' @param supported_levels A character vector of supported levels
#' @inheritParams region_dispatch
#' @return NULL
#' @concept utility
check_level <- function(level, supported_levels) {
  if (!any(supported_levels %in% level)) {
    stop(
      level,
      " is not a supported level check supported_levels for options"
    )
  }
  return(invisible(NULL))
}

#' Download Excel Documents
#'
#' @param url Character string containing the full URL to the Excel document.
#' @param archive Character string naming the file name to assign in the
#' temporary directory.
#' @param transpose Logical, should the read in data be transposed
#' @param ... Additional parameters to pass to `read_excel()`.
#' @inheritParams message_verbose
#' @importFrom readxl read_excel
#' @return A `data.frame`.
#' @concept utility
download_excel <- function(url, archive, verbose = FALSE,
                           transpose = TRUE, ...) {
  # download
  archive <- file.path(tempdir(), archive)
  download.file(
    url = url,
    destfile = archive,
    mode = "wb", quiet = !(TRUE)
  )
  # read in
  dt <- suppressMessages(
    read_excel(archive, ...)
  )

  if (transpose) {
    dt <- t(dt)
  }
  dt <- as.data.frame(dt)
  return(dt)
}
