#' French Regional Daily Case Counts
#'
#' @description Data from Santé Publique France https://github.com/opencovid19-fr/data
#'
#' @author Paul Campbell @paulcampbell91
#'
#' @return A dataframe of daily French regional case counts.
#' @importFrom httr GET content
#' @importFrom purrr map_dfr map_chr
#' @importFrom dplyr mutate arrange group_by ungroup
#' @importFrom memoise cache_filesystem memoise
#' @export
#' @examples
#'
#' ## Code
#' get_france_regional_cases

get_france_regional_cases <- function() {

  # github api url to list contents of folder (daily data is stored in separate yaml files)
  spf_url <- "https://api.github.com/repos/opencovid19-fr/data/contents/sante-publique-france"
  spf_ls <- httr::GET(spf_url) %>% httr::content() %>% purrr::map_chr("download_url")

  #set up cache
  ch <- memoise::cache_filesystem(".cache")

  mem_read_spf_regions <- memoise::memoise(read_spf_regions_yaml, cache = ch)

  df_spf <- spf_ls %>%
    purrr::map_dfr(mem_read_spf_regions) %>%
    dplyr::group_by(region) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(cases = c(cases[1], diff(cases))) %>% # convert from cumulative to daily
    dplyr::ungroup()

  return(df_spf)
}

#' Read SPF yaml file to dataframe
#'
#' @param path path to daily SPF covid-19 yaml file
#' @importFrom yaml read_yaml
#' @importFrom tibble tibble
#' @importFrom purrr map_chr map_int
read_spf_regions_yaml <- function(path) {
  ydat <- suppressWarnings(yaml::read_yaml(path))
  regions <- ydat$donneesRegionales

  tibble::tibble(
    date = as.Date(ydat$date),
    country = "France",
    source = "SPF",
    region = purrr::map_chr(regions, "nom"),
    region_code = purrr::map_chr(regions, "code"),
    cases = purrr::map_int(regions, "casConfirmes")
  )
}

