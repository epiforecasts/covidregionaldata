#' Get Total Cases by Country
#'
#' @param source Character String specifying data source for total cases "WHO", "ECDC", default: "WHO".
#' @return Returns a data frame of variables along with their most recent value.
#' @export
#' @importFrom dplyr select contains slice mutate arrange desc filter
#' @importFrom tibble as_tibble
#' @examples
#'
#' ## Total cases sourced from the WHO
#' get_total_cases(source = "WHO")
#'
#' ## Total cases sourced from ECDC
#' get_total_cases(source = "ECDC")
get_total_cases <- function(source = 'WHO') {

  who_total_cases <- function(total_cases){

    V1 <- NULL; cases <- NULL;

    total_cases <- total_cases %>%
      dplyr::select(!dplyr::contains("deaths"))

    total_cases <- total_cases[nrow(total_cases), ] %>%
      t() %>%
      tibble::as_tibble(rownames = "country") %>%
      dplyr::slice(-c(1:2))  %>%
      dplyr::mutate(cases = as.numeric(V1)) %>%
      dplyr::select(-V1) %>%
      dplyr::arrange(dplyr::desc(cases)) %>%
      dplyr::filter(!grepl("Region", country)) %>%
      dplyr::filter(!grepl("Global", country))


    return(total_cases)

  }

  ecdc_total_cases <- function(total_cases){
    total_cases <- total_cases %>%
      dplyr::group_by(country) %>%
      dplyr::summarise(cases = sum(cases)) %>%
      dplyr::arrange(-cases)

    return(total_cases)

  }

  if (source == 'WHO'){
    total_cases <- NCoVUtils::get_who_cases()

    total_cases <- who_total_cases(total_cases)

  }else if (source == 'ECDC'){
    total_cases <- NCoVUtils::get_ecdc_cases()

    total_cases <- ecdc_total_cases(total_cases)

  }else{
    stop('Unknown Data Source. Current data sources: "WHO", "ECDC"')
  }

  return(total_cases)

}
