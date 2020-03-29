#' Spain regional cases daily
#'
#' @description Extract regional spanish case counts.
#' [Source](https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-China/situacionActual.html)
#' @return A dataframe of daily Spanish regional case counts.
#' @importFrom tabulizer extract_tables
#' @importFrom dplyr select mutate
#' @importFrom memoise cache_filesystem memoise
#' @importFrom stringr str_remove_all
#' @importFrom tibble as_tibble
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
#'
#' regions_with_data <- regions %>%
#'   group_by(region) %>%
#'   summarise(subregions = n(), do_union = TRUE) %>%
#'   mutate(region = str_replace_all(region, "í", "i"),
#'          region = str_replace_all(region, "ó", "o"),
#'          region = str_replace_all(region, "ñ", "n"),
#'          region = str_replace_all(region, "Foral de Navarra", "Navarra"),
#'          region = str_replace_all(region, "Valenciana", "C. Valenciana"),
#'          region = str_replace_all(region, "Canary Is\\.", "Canarias"),
#'          region = str_replace_all(region, "Islas Baleares", "Baleares"),
#'          region = str_replace_all(region, "-", " ")
#'          ) %>%
#'   left_join(data, by = c("region" = "region"))
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
#' ##Code
#' get_spain_regional_cases
# get_spain_regional_cases <- function() {
#
#   # Path to table. Daily report 52 = 22 March 2020 so calculating daily updated link based on that
#   link_1 <- "https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-China/documentos/Actualizacion_"
#   link_2 <- 52 + (as.numeric(Sys.Date()) - 18343)
#   link_3 <- "_COVID-19.pdf"
#   location <- paste0(link_1, link_2, link_3)
#
#   #set up cache
#   ch <- memoise::cache_filesystem(".cache")
#   mem_read <- memoise::memoise(tabulizer::extract_tables, cache = ch)
#
#   # Extract & clean table (e.g. remove cumulative cases & incidence)
#   out <- mem_read(location, pages = 1)
#   cases <- do.call(rbind, out[length(out)])
#   cases <- tibble::as_tibble(cases, .name_repair = "universal") %>%
#     dplyr::select("region" = 1, "cases" = ncol(cases)) %>%
#     dplyr::filter(region != "ESPAÃ‘A" & region != "CCAA" & region != "") %>%
#     dplyr::mutate(region = iconv(region, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
#                   cases = as.character(cases),
#                   cases = stringr::str_remove_all(cases, "[[:punct:]]"),
#                   cases = as.numeric(cases))
#   return(cases)
#
# }


