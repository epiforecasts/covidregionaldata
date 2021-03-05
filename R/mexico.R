#' Download Mexico data across levels
#'
#' @description Data download function for Mexico data
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
  # Fetch the latest data using a PHP script from the website
  level <- match.arg(level, choices = c("municipio", "estados"))
  if (level %in% "municipio") {
    path <- "Downloads/filesDD.php?csvmun"
  }else {
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

 deceased_url <- gsub("Confirmados", "Defunciones", confirmed_url, fixed = TRUE)

  read_data <- function(target, new_name) {
    if (verbose) {
      message("Downloading ", new_name)
    }
    csv_reader(file.path(domain, target)) %>%
    select(- .data$poblacion) %>%
    pivot_longer(-c("cve_ent", "nombre"),
                 names_to = "date", values_to = new_name)
  }

  confirmed <- read_data(confirmed_url, "cases_new")
  deceased <- read_data(deceased_url, "deaths_new")
  df <- full_join(confirmed, deceased,
                   by = c("cve_ent", "nombre", "date"))
  return(df)
}

#' Download Mexican Regional Daily COVID-19 Count Data - Estado
#'
#' @description Extracts daily COVID-19 data for Mexico, stratified by Estados.
#' Data available at <https://datos.covid-19.conacyt.mx/#DownZCSV>.
#' @export
#' @inheritParams download_regional
#' @method download_regional crd_mexico_1
#' @author Sam Abbott
download_regional.crd_mexico_1 <- function(region, verbose = TRUE, ...) {
  df <- mexico_api(level = "estados", verbose = verbose)
  region$raw <- df
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
download_regional.crd_mexico_2 <- function(region, verbose = TRUE, ...) {
  df <- mexico_api(level = "municipio", verbose = verbose)
  region$raw <- df
  return(region)
}

#' Mexican Regional Daily COVID-19 Count Data - Estado
#'
#' @description Extracts daily COVID-19 data for Mexico, stratified by Estados.
#' Data available at <https://datos.covid-19.conacyt.mx/#DownZCSV>.
#' It is loaded and then sanitised.
#' @return A data frame of COVID cases by Estado in Mexico, ready to be used by [get_regional_data()].
#' @importFrom dplyr %>% select full_join mutate filter rename
#' @importFrom httr POST content
#' @importFrom xml2 xml_find_first xml_text
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_to_title
#' @importFrom lubridate dmy
#'
get_mexico_regional_cases_only_level_1 <- function() {

  # Get codes which are used to match

  # There is no permanent URL leading to the latest data. All URLs contain
  # the date of the latest update. We might be able to append Sys.Date() to
  # the base URL but it's more robust to fetch the URL using the PHP script
  # from the website
  domain <- "https://datos.covid-19.conacyt.mx/"
  script_url <- file.path(domain, "Downloads/filesDD.php?csvaxd")

  confirmed_url <- script_url %>%
    httr::POST(body = "Confirmados", encode = "form", verbose = TRUE) %>%
    httr::content() %>%
    xml2::xml_find_first("//script") %>%
    xml2::xml_text() %>%
    strsplit("\\n\\t*") %>%
    unlist() %>%
    {
      grep("^a\\.href", ., value = TRUE)
    } %>%
    {
      gsub('^a\\.href\\s+=\\s+"(.*)";', "\\1", .)
    }

  deceased_url <- gsub("Confirmados", "Defunciones", confirmed_url, fixed = TRUE)

  confirmed <- csv_reader(file.path(domain, confirmed_url)) %>%
    dplyr::select(-poblacion) %>%
    tidyr::pivot_longer(-c("cve_ent", "nombre"), names_to = "date", values_to = "cases_new")

  deceased <- csv_reader(file.path(domain, deceased_url)) %>%
    dplyr::select(-poblacion) %>%
    tidyr::pivot_longer(-c("cve_ent", "nombre"), names_to = "date", values_to = "deaths_new")

  state_codes <- get_mexico_inegi_codes()

  data <- dplyr::full_join(confirmed, deceased, by = c("cve_ent", "nombre", "date")) %>%
    dplyr::mutate(
      region_level_1 = stringr::str_to_title(nombre),
      region_level_1 = ifelse(region_level_1 == "Distrito Federal",
        "Ciudad de Mexico",
        region_level_1
      ),
      date = lubridate::dmy(date)
    ) %>%
    dplyr::full_join(state_codes, by = "region_level_1") %>%
    dplyr::filter(region_level_1 != "Nacional") %>%
    dplyr::rename(level_1_region_code = iso_code) %>%
    dplyr::select(-c(nombre, cve_ent, inegi_state))

  return(data)
}

#' Mexican Regional Daily COVID-19 Count Data - Municipio
#'
#' @description Extracts daily COVID-19 data for Germany, stratified by Municipio
#' Data available at <https://datos.covid-19.conacyt.mx/#DownZCSV>
#' It is loaded and then sanitised.
#' @return A data.frame of COVID cases by Municipio in Mexico, ready to be used by [get_regional_data()].
#' @importFrom dplyr %>% select full_join mutate if_else filter
#' @importFrom httr POST content
#' @importFrom xml2 xml_find_first xml_text
#' @importFrom tidyr pivot_longer
#' @importFrom tibble tribble
#' @importFrom lubridate as_date dmy
get_mexico_regional_cases_with_level_2 <- function() {
  domain <- "https://datos.covid-19.conacyt.mx/"
  script_url <- file.path(domain, "Downloads/filesDD.php?csvmun")

  confirmed_url <- script_url %>%
    httr::POST(body = "Confirmados", encode = "form", verbose = TRUE) %>%
    httr::content() %>%
    xml2::xml_find_first("//script") %>%
    xml2::xml_text() %>%
    strsplit("\\n\\t*") %>%
    unlist() %>%
    {
      grep("^a\\.href", ., value = TRUE)
    } %>%
    {
      gsub('^a\\.href\\s+=\\s+"(.*)";', "\\1", .)
    }

  deceased_url <- gsub("Confirmados", "Defunciones", confirmed_url, fixed = TRUE)

  confirmed <- csv_reader(file.path(domain, confirmed_url)) %>%
    dplyr::select(-poblacion) %>%
    tidyr::pivot_longer(-c("cve_ent", "nombre"), names_to = "date", values_to = "cases_new")

  deceased <- csv_reader(file.path(domain, deceased_url)) %>%
    dplyr::select(-poblacion) %>%
    tidyr::pivot_longer(-c("cve_ent", "nombre"), names_to = "date", values_to = "deaths_new")

  state_codes <- get_mexico_inegi_codes()

  data <- dplyr::full_join(confirmed, deceased, by = c("cve_ent", "nombre", "date")) %>%
    dplyr::mutate(
      region_level_2 = nombre,
      inegi_state = substr(cve_ent, 1, 2),
      date = lubridate::dmy(date)
    ) %>%
    dplyr::select(-nombre) %>%
    dplyr::full_join(state_codes, by = "inegi_state") %>%
    dplyr::mutate(
      level_1_region_code = iso_code,
      level_2_region_code = cve_ent
    ) %>%
    dplyr::select(-inegi_state, -cve_ent, -inegi_state, -iso_code)

  return(data)
}

#' Mexico level 1 codes
#' Needed to join across level 1 ISO codes, full region 1 names, and INEGI state codes used in level 2 codes.
#' @importFrom tibble tribble

get_mexico_inegi_codes <- function() {
  state_codes <- tibble::tribble(
    ~inegi_state, ~region_level_1, ~iso_code,
    "01", "Aguascalientes", "MX-AGU",
    "02", "Baja California", "MX-BCN",
    "03", "Baja California Sur", "MX-BCS",
    "04", "Campeche", "MX-CAM",
    "07", "Chiapas", "MX-CHP",
    "08", "Chihuahua", "MX-CHH",
    "09", "Ciudad de Mexico", "MX-CMX",
    "05", "Coahuila", "MX-COA",
    "06", "Colima", "MX-COL",
    "10", "Durango", "MX-DUR",
    "11", "Guanajuato", "MX-GUA",
    "12", "Guerrero", "MX-GRO",
    "13", "Hidalgo", "MX-HID",
    "14", "Jalisco", "MX-JAL",
    "15", "Mexico", "MX-MEX",
    "16", "Michoacan", "MX-MIC",
    "17", "Morelos", "MX-MOR",
    "18", "Nayarit", "MX-NAY",
    "19", "Nuevo Leon", "MX-NLE",
    "20", "Oaxaca", "MX-OAX",
    "21", "Puebla", "MX-PUE",
    "22", "Queretaro", "MX-QUE",
    "23", "Quintana Roo", "MX-ROO",
    "24", "San Luis Potosi", "MX-SLP",
    "25", "Sinaloa", "MX-SIN",
    "26", "Sonora", "MX-SON",
    "27", "Tabasco", "MX-TAB",
    "28", "Tamaulipas", "MX-TAM",
    "29", "Tlaxcala", "MX-TLA",
    "30", "Veracruz", "MX-VER",
    "31", "Yucatan", "MX-YUC",
    "32", "Zacatecas", "MX-ZAC"
  )
  return(state_codes)
}
