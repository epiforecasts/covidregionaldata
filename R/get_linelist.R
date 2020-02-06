##' Get the latest compiled linelist
##'
##' @return a tibble with the linelist
##'   delays to randomly sample
##' @importFrom readr read_csv
##' @importFrom dplyr bind_rows rename mutate mutate_at
##' @importFrom purrr map
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



  gids <- c(outside_hubei = 0, hubei = 429276722)
  urls <- paste0("https://docs.google.com/spreadsheets/d/",
                 "1itaohdPiAeniCXNlntNztZ_oRvjh0HsGuJXUJWET008/pub",
                 "?single=true&output=csv&gid=", gids)
  linelists <- lapply(urls, readr::read_csv)

  ## Drop columns
  drops <- c("chroDisea_Yes(1)/No(0)")
  linelists[[1]] <- linelists[[1]][ , !(names(linelists[[1]]) %in% drops)]
  linelists[[2]] <-  linelists[[2]][ , !(names(linelists[[2]]) %in% drops)]

  ##Munge dataset specific data
  linelists <- linelists %>%
    purrr::map(
      function(data) {
        dplyr::mutate_at(data,
                         .vars = c("longitude", "latitude"),
                         ~ ifelse(. %in% "#REF!", NA, .) %>%
                           stringr::str_trim() %>%
                           as.numeric()) %>%
        dplyr::mutate_all(~ stringr::str_replace_all(., "N/A", NA_character_))
      })

  linelists <- dplyr::bind_rows(linelists) %>%
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

  return(linelists)
}
