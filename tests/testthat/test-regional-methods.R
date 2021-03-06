#' # initialise data downloading in the UK
#' new_covidregionaldata("uk", "1")
#' # initialise data downloading for mexico
#' new_covidregionaldata("mexico", "1")
#' # initialise data download from the ECDC source
#' new_covidregionaldata("ecdc")
#' 
#' #' region <- new_covidregionaldata("uk", "1")
#' class(region) <- "list"
#' download_regional(region, verbose = FALSE)
#' #' region <- new_covidregionaldata("uk", "1")
#' class(region) <- "list"
#' region <- download_regional(region, verbose = FALSE)
#' clean_regional(region, verbose = FALSE)
#' return_regional.default <- function(region, steps = FALSE) {
 # region$return <- NA
 # if (steps) {
 #   return(region)
 # } else {
 #   return(region$processed)
 # }
#}