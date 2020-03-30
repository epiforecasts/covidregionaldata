##' Get the latest compiled linelist
##'
##' @return a tibble with the linelist
##' @importFrom readr read_csv
##' @importFrom dplyr rename mutate mutate_at mutate_all
##' @importFrom stringr str_trim
##' @author Sebastian Funk <sebastian.funk@lshtm.ac.uk>
##'
##' @export
##'
##' @examples
##'
##' ## Example
##' \dontrun{
##' get_linelist()
##' }
##'
##' ## Code
##' get_linelist

get_linelist <- function() {

  ## CRAN check - dealing with global variables
  date_onset_symptoms_str <- NULL; date_admission_hospital_str <- NULL;
  ate_confirmation_str <- NULL; date_confirmation <- NULL;
  date_onset_symptoms <- NULL; date_onset_symptoms <- NULL;
  date_admission_hospital <- NULL; date_confirmation_str <- NULL;
  
  ## Set up cache
  ch <- memoise::cache_filesystem(".cache")

  mem_read <- memoise::memoise(readr::read_csv, cache = ch)
  
  data <- mem_read('https://raw.githubusercontent.com/beoutbreakprepared/nCoV2019/master/latest_data/latestdata.csv')
  
  data <- data %>% 
    dplyr::mutate_at(.vars = c("longitude", "latitude"), ~ ifelse(. %in% "#REF!", NA, .) %>% 
                       stringr::str_trim() %>%
                       as.numeric()) %>%
    dplyr::mutate_all(~ stringr::str_replace_all(., "N/A", NA_character_)) %>% 
    dplyr::rename(
      date_onset_symptoms_str = "date_onset_symptoms",
      date_admission_hospital_str = "date_admission_hospital",
      date_confirmation_str = "date_confirmation"
    ) %>%
    dplyr::mutate(
      date_onset_symptoms =
        as.Date(date_onset_symptoms_str, format = "%d.%m.%Y"),
      date_admission_hospital =
        as.Date(date_admission_hospital_str, format = "%d.%m.%Y"),
      date_confirmation =
        as.Date(date_confirmation_str, format = "%d.%m.%Y")
    ) %>%
    dplyr::mutate(
      delay_confirmation =
        date_confirmation - date_onset_symptoms,
      delay_admission =
        date_admission_hospital - date_onset_symptoms
    )
  
  return(data)
}


