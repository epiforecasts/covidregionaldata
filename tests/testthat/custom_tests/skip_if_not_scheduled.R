skip_if_not_scheduled <- function() {
    # only runs if time between midnight and 1 am
  if (lubridate::hour(Sys.time()) == 0) {
    return(invisible(TRUE))
  }
  testthat::skip("Only run for scheduled checks")
}