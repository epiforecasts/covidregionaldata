#' Download Mexico data across levels
#'
#' @description Data download function for Mexico data. To get the latest data
#' use a PHP script from the website.
#' @param level A character string indicating the target administrative level
#' to download. Supports "estados" and "municipio".
#' @inheritParams download_regional
#' @author Sam Abbott
#' @importFrom httr POST content
#' @importFrom xml2 xml_find_first xml_text
#' @importFrom dplyr select full_join
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer
mexico_api <- function(level = "estados", verbose = TRUE) {
  . <- NULL
  level <- match.arg(level, choices = c("municipio", "estados"))

  if (level %in% "municipio") {
    path <- "Downloads/filesDD.php?csvmun"
  } else {
    path <- "Downloads/filesDD.php?csvaxd"
  }
  domain <- "https://datos.covid-19.conacyt.mx/"
  script_url <- file.path(domain, path)

  confirmed_url <- script_url %>%
    POST(body = "Confirmados", encode = "form", verbose = TRUE) %>%
    content() %>%
    xml_find_first("//script") %>%
    xml_text() %>%
    strsplit("\\n\\t*") %>%
    unlist() %>%
    {
      grep("^a\\.href", ., value = TRUE)
    } %>%
    {
      gsub('^a\\.href\\s+=\\s+"(.*)";', "\\1", .)
    }

  deceased_url <- gsub("Confirmados", "Defunciones", confirmed_url,
    fixed = TRUE
  )

  read_data <- function(target, new_name) {
    if (verbose) {
      message("Downloading ", new_name)
    }
    csv_reader(file.path(domain, target)) %>%
      select(-.data$poblacion) %>%
      pivot_longer(-c("cve_ent", "nombre"),
        names_to = "date", values_to = new_name
      )
  }

  confirmed <- read_data(confirmed_url, "cases_new")
  deceased <- read_data(deceased_url, "deaths_new")
  dat <- full_join(confirmed, deceased,
    by = c("cve_ent", "nombre", "date")
  )
  return(dat)
}

#' Download Mexican Regional Daily COVID-19 Count Data - Estado
#'
#' @description Extracts daily COVID-19 data for Mexico, stratified by Estados.
#' Data available at <https://datos.covid-19.conacyt.mx/#DownZCSV>.
#' @export
#' @inheritParams download_regional
#' @method download_regional crd_mexico_1
#' @author Sam Abbott
#' @examples
#' \dontrun{
#' mexico <- new_covidregionaldata("mexico")
#' mexico <- download_regional(mexico)
#' mexico$raw
#' }
download_regional.crd_mexico_1 <- function(region, verbose = TRUE, ...) {
  dat <- mexico_api(level = "estados", verbose = verbose)
  region$raw <- dat
  return(region)
}

#' Download Mexican Regional Daily COVID-19 Count Data - Municipio
#'
#' @description Extracts daily COVID-19 data for Mexico, stratified by
#' Municipio.
#' Data available at <https://datos.covid-19.conacyt.mx/#DownZCSV>.
#' @export
#' @inheritParams download_regional
#' @method download_regional crd_mexico_2
#' @author Sam Abbott
#' @examples
#' \dontrun{
#' mexico <- new_covidregionaldata("mexico", level = "2")
#' mexico <- download_regional(mexico)
#' mexico$raw
#' }
download_regional.crd_mexico_2 <- function(region, verbose = TRUE, ...) {
  dat <- mexico_api(level = "municipio", verbose = verbose)
  region$raw <- dat
  return(region)
}

#' Mexico Specific State Level Data Cleaning
#'
#' @export
#' @inheritParams clean_regional
#' @method clean_regional crd_mexico_1
#' @author Sam Abbott
#' @importFrom dplyr mutate full_join filter rename select
#' @importFrom stringr str_to_title
#' @importFrom lubridate dmy
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' mexico <- new_covidregionaldata("mexico")
#' mexico <- download_regional(mexico)
#' clean_regional(mexico)$clean
#' }
clean_regional.crd_mexico_1 <- function(region, verbose = TRUE, ...) {
  region$clean <- region$raw %>%
    mutate(
      region_level_1 = str_to_title(.data$nombre),
      region_level_1 = ifelse(.data$region_level_1 == "Distrito Federal",
        "Ciudad de Mexico",
        .data$region_level_1
      ),
      date = dmy(.data$date)
    ) %>%
    full_join(region$codes_lookup, by = "region_level_1") %>%
    filter(.data$region_level_1 != "Nacional") %>%
    rename(level_1_region_code = .data$iso_code) %>%
    select(-c(.data$nombre, .data$cve_ent))
  return(region)
}

#' Mexico Specific Municipality Level Data Cleaning
#'
#' @export
#' @inheritParams clean_regional
#' @method clean_regional crd_mexico_2
#' @author Sam Abbott
#' @importFrom dplyr mutate full_join filter rename select
#' @importFrom stringr str_to_title
#' @importFrom lubridate dmy
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' mexico <- new_covidregionaldata("mexico", level = "2")
#' mexico <- download_regional(mexico)
#' clean_regional(mexico)$clean
#' }
clean_regional.crd_mexico_2 <- function(region, verbose = TRUE, ...) {
  region$clean <- region$raw %>%
    mutate(
      region_level_2 = .data$nombre,
      inegi_state = substr(.data$cve_ent, 1, 2),
      date = dmy(.data$date)
    ) %>%
    select(-.data$nombre) %>%
    full_join(region$codes_lookup, by = "inegi_state") %>%
    mutate(
      level_1_region_code = .data$iso_code,
      level_2_region_code = .data$cve_ent
    ) %>%
    select(
      -.data$inegi_state, -.data$cve_ent,
      -.data$inegi_state, -.data$iso_code
    )
  return(region)
}
