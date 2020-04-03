#' Spain regional cases daily
#'
#' @description Extract regional spanish case counts.
#' [Source](https://covid19.isciii.es/)
#' @param dataset Character String specifying dataset: "cases_provincial", "hospitalisation_provincial", "icu_provincial", "mortality_provincial", "recovered_provincial", "all". Default: "cases_provincial".
#' @return A dataframe of specified Covid data. If dataset = "all", a dataframe with all variables (cases, hospitalisation, ICU, mortality, recovered)
#' @importFrom dplyr select mutate filter left_join first
#' @importFrom memoise cache_filesystem memoise
#' @importFrom readr read_csv
#' @importFrom lubridate dmy
#' @export
#' @examples
#'
#' \dontrun{
#'
#' # Regional map
#'
#' library(sf)
#' library(rnaturalearth)
#' library(ggplot2)
#'
#' data <- get_spain_regional_cases()
#'
#' regions <- rnaturalearth::ne_states("Spain", returnclass = "sf")
#' regions_with_data <- regions %>%
#'   mutate(region_shortcode = stringr::str_remove_all(region_cod, "^ES\."), # Match codes to dataset
#'          region_shortcode = stringr::str_replace(region_shortcode, "^PM$", "IB"), # Baleares
#'          region_shortcode = stringr::str_replace(region_shortcode, "^MU$", "MC"), # Murcia
#'          region_shortcode = stringr::str_replace(region_shortcode, "^NA$", "NC"), # Navarra
#'          region_shortcode = stringr::str_replace(region_shortcode, "^LO$", "RI"), # La Rioja
#'          region_shortcode = ifelse(region == "Melilla", "ME", region_shortcode) # Melilla
#'   ) %>%
#'   group_by(region_shortcode) %>%
#'   summarise(subregions = n(), do_union = TRUE) %>%
#'   left_join(data, by = c("region_shortcode" = "region"))
#'
#' # Map: Spain mainland + Baleares
#' es_mainland <- ggplot2::ggplot(regions_with_data) +
#'   geom_sf(ggplot2::aes(fill = cases)) +
#'   coord_sf(crs = st_crs(4326), xlim = c(-11, 4), ylim = c(35, 44))+
#'   theme_bw()
#'
#' # Map inset: Canarias
#' es_canarias <- dplyr::filter(regions_with_data, region == "Canarias")
#' es_canarias <- ggplot2::ggplot(es_canarias) +
#'   geom_sf(aes(fill = cases)) +  coord_sf(datum = NA) +
#'   xlab(es_canarias$region) +
#'   theme_bw() +  theme(legend.position = "none")
#'
#' # Map: mainland with insets
#' es_mainland +
#'   annotation_custom(
#'     grob = ggplotGrob(es_canarias),  xmin = -11,  xmax = -7,  ymin = 33.5,  ymax = 37.5)
#'
#'
#' }
#'
#' ##Code
#' get_spain_regional_cases

get_spain_regional_cases <- function(dataset = "cases_provincial"){

  if(!dataset %in% c("cases_provincial", "hospitalisation_provincial", "icu_provincial", "mortality_provincial", "recovered_provincial", "all")) {
    stop('Unknown input. Please specify dataset: "cases_provincial", "hospitalisation_provincial", "icu_provincial", "mortality_provincial", "recovered_provincial", "all". Default: "cases_provincial".')
  }

  spain_default_all <- function() {
    # Set up
    province_name <- tibble::tibble(code = c("AN", "AR", "AS", "CB", "CE", "CL", "CM", "CN", "CT", "EX", "GA", "IB", "MC", "MD", "ME", "NC", "PV", "RI", "VC"),
                                  name = c("Andaluc\u00eda","Arag\u00f3n","Principado de Asturias","Cantabria", "Ceuta", "Castilla y Le\u00f3n", "Castilla-La Mancha", "Santa Cruz de Tenerife", "Catalu\u00f1a", "Extremadura", "Galicia", "Islas Baleares", "Regi\u00f3n de Murcia", "Comunidad de Madrid", "Melilla", "Comunidad Foral de Navarra", "Pa\u00eds Vasco", "La Rioja", "Comunidad Valenciana"))

    location <- "https://covid19.isciii.es/resources/serie_historica_acumulados.csv"

    # Cache
    ch <- memoise::cache_filesystem(".cache")
    mem_read <- memoise::memoise(readr::read_csv, cache = ch)

    # Read data
    all_data <- suppressMessages(mem_read(location)) %>%
      dplyr::rename(province = 1, date = 2, cases_cum = 3, hospital_cum = 4, icu_cum = 5, deaths_cum = 6, recover_cum = 7) %>%
      dplyr::left_join(province_name, by = c("province" = "code")) %>%
      dplyr::filter(is.na(date) == FALSE) %>%
      dplyr::mutate(date = lubridate::dmy(date)) %>%
      dplyr::group_by(province) %>%
      dplyr::arrange(date, .by_group = T) %>%
      dplyr::mutate(cases_daily = cases_cum - lag(cases_cum, default = dplyr::first(cases_cum)),
                    hospital_daily = hospital_cum - lag(hospital_cum, default = first(hospital_cum)),
                    icu_daily = icu_cum - lag(icu_cum, default = first(icu_cum)),
                    deaths_daily = deaths_cum - lag(deaths_cum, default = first(deaths_cum)),
                    recover_daily = recover_cum - lag(recover_cum, default = first(recover_cum))
                    )
    all_data[is.na(all_data)] = 0
    all_data[all_data < 0] = 0

    return(all_data)
  }

  if (dataset == "cases_provincial"){
    return(spain_default_all() %>%
             select(province, name, date, cases_cum, cases_daily))

  }else if (dataset == "hospitalisation_provincial"){
    return(cases_provincial <- spain_default_all() %>%
             select(province, name, date, cases_cum, cases_daily))
    
  }else if (dataset == "icu_provincial"){
    return(icu_provincial <- spain_default_all() %>%
             select(province, name, date, cases_cum, cases_daily))
    
  }else if (dataset == "mortality_provincial"){
    return(mortality_provincial <- spain_default_all() %>%
             select(province, name, date, cases_cum, cases_daily))
    
  }else if (dataset == "recovered_provincial"){
    return(recovered_provincial <- spain_default_all() %>%
             select(province, name, date, cases_cum, cases_daily))
    
  }else if (dataset == "all"){
    return(all <- spain_default_all())
  }
}
