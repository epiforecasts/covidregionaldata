#' Get Total Cases by Country
#'
#' @return Returns a data frame of variables along with their most recent value.
#' @export
#' @importFrom dplyr select contains slice mutate arrange desc
#' @importFrom tibble as_tibble
#' @examples
#'
#'
#' get_total_cases()
get_total_cases <- function() {

  V1 <- NULL; cases <- NULL;

  total_cases <- NCoVUtils::get_who_cases()

  total_cases <- total_cases %>%
    dplyr::select(!dplyr::contains("deaths"))

  total_cases <- total_cases[nrow(total_cases), ] %>%
    t() %>%
    tibble::as_tibble(rownames = "country") %>%
    dplyr::slice(-c(1:2))  %>%
    dplyr::mutate(cases = as.numeric(V1)) %>%
    dplyr::select(-V1) %>%
    dplyr::arrange(dplyr::desc(cases))

  return(total_cases)
}
