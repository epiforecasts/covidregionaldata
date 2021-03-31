#' Get available datasets
#' 
#' @description 
#'  `r lifecycle::badge("deprecated")`
#'  
#'  This function is deprecated. Please use `get_available_datasets()` instead.
#'  
#' @keywords internal
#' @import lifecycle deprecate_stop
get_info_covidregionaldata <- function() {
  deprecate_warn(when = "0.9.0",
                 what = "covidregionaldata::get_info_covidregionaldata()", 
                 with = "covidregionaldata::get_available_datasets()")
  data <- get_available_datasets()
  return(data)
}
