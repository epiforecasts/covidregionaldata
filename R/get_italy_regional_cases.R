
#' Italian Regional Case Counts
#' @description Fetches COVID data by region. Data is collated by the Italian Department of Civil Protection
#' and is available on github: https://github.com/pcm-dpc/COVID-19
#' @return A dataframe of Italian regional case counts.
#' @export
#' @importFrom readr read_csv
#' @importFrom lubridate ymd
#' @importFrom purrr map_dfr
#' @importFrom dplyr mutate select arrange group_by n lag ungroup left_join
#' @importFrom memoise cache_filesystem memoise
#' @examples
#'
#' ## Code
#' get_italy_regional_cases
get_italy_regional_cases <- function() {

  ## Path to data
  path <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni-__date__.csv"


  ## Set up cache
  ch <- memoise::cache_filesystem(".cache")


  mem_read <- memoise::memoise(readr::read_csv, cache = ch)


  ## Function to get daily files
  get_daily_files = function(date){

    message("Reading in data from the ", date)
    suppressMessages(suppressWarnings(
      mem_read(gsub("__date__", format(date, "%Y%m%d"), x=path))
    ))

  }

  ## Extract daily data
  start_date <- lubridate::ymd(20200224)
  start_date <- as.Date(format(start_date, "%Y-%m-%d"))
  end_date <-  as.Date(Sys.Date() - 1)

  dates <- seq(start_date, end_date, by = "day")

  cases <- purrr::map_dfr(dates,
                          get_daily_files)

  ## Clean variables
  cases <- cases %>%
    dplyr::mutate(date = as.Date(data),
                  region = as.character(denominazione_regione),
                  region_code = codice_regione,
                  total_cases = totale_casi) %>%
    dplyr::select(date, region, region_code, total_cases) %>%
    dplyr::arrange(date) %>%
    dplyr::group_by(region) %>%
    dplyr::mutate(
      index = 1:dplyr::n(),
      cases = total_cases - ifelse(index == 1, 0, dplyr::lag(total_cases))) %>%
    dplyr::ungroup() %>%
    dplyr::select(-index, -total_cases) %>%
    ## Adjust negative cases by setting to 0
    dplyr::mutate(cases = ifelse(cases < 0 , 0, cases))


  cases <- cases %>%
    dplyr::mutate(region = dplyr::recode(region, "P.A. Trento" = "Trentino-Alto Adige",
                                        "P.A. Bolzano" = "Trentino-Alto Adige") %>%
                    as.character()) %>%
    dplyr::group_by(region, date) %>%
    dplyr::summarise(cases = sum(cases)) %>%
    dplyr::ungroup()


  regions <-  data.frame(region = c("Abruzzo", "Basilicata", "Calabria",
                            "Campania", "Emilia Romagna", "Friuli Venezia Giulia",
                            "Lazio", "Lombardia", "Marche", "Molise", "P.A. Bolzano",
                            "P.A. Trento", "Piemonte", "Puglia", "Sardegna", "Sicilia", "Toscana",
                            "Umbria", "Valle d'Aosta", "Veneto"),
                         region_code = c(15, 10, 11, 1, 21, 20, 6, 8, 17, 14,
                                        18, 13, 9, 2, 4, 3, 19, 16, 12, 5),
                         stringsAsFactors = FALSE)

  cases <- cases %>%
    dplyr::left_join(regions, by = "region")

  return(cases)
}

