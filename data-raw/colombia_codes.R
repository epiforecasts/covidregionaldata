# Set Colombia region codes
#
# Level 1 codes: ISO-3166-2
# Source: https://en.wikipedia.org/wiki/ISO_3166-2:CO
#
library(rvest)
library(stringi)
library(dplyr)
library(tibble)

# Level 1 -----------------------------------------------------------------
# Get ISO codes
co_iso <- "https://en.wikipedia.org/wiki/ISO_3166-2:CO"

level_1_region_code <- read_html(co_iso) %>%
  html_nodes(".monospaced , .jquery-tablesorter td") %>%
  html_text()
level_1_region_code <- level_1_region_code[2:34]

level_1_region <- read_html(co_iso) %>%
  html_nodes("tr:nth-child(1) .monospaced , td:nth-child(2)") %>%
  html_text()
level_1_region <- level_1_region[1:33]

colombia_departments <- data.frame(
  level_1_region_code,
  level_1_region,
  stringsAsFactors = FALSE
) %>%
  mutate(
    level_1_region = stri_trans_general(level_1_region, "latin-ascii"),
    level_1_region = stri_trim_both(level_1_region),
    level_1_region = stringr::str_to_title(level_1_region)
  )

# Some ISO names need replacements to match data
replacements <- list(
  "Distrito Capital De Bogota" = "Bogota"
)
colombia_departments <- colombia_departments %>%
  mutate(
    level_1_region = ifelse(level_1_region %in% names(replacements),
      replacements[level_1_region],
      level_1_region
    ),
    level_1_region = as.character(level_1_region)
  )

# Download list of municipalities and codes
# 

colombia_municipalities_sheet <- download_excel(
  "https://www.dane.gov.co/files/censo2005/provincias/subregiones.xls",
  "colombia_municipalities.xls",
  verbose = TRUE,
  transpose = FALSE,
  sheet = "Hoja1"
) 
  colombia_municipalities <- colombia_municipalities_sheet %>%
  select(level_2_region = NOM_MPIO,
         level_2_region_code = DPTOC_MPIO,
         level_1_region = NOM_DEPTO) %>%
  mutate(
    level_1_region = stri_trans_general(level_1_region, "latin-ascii"),
    level_1_region = stri_trim_both(level_1_region),
    level_1_region = stringr::str_to_title(level_1_region),
    level_1_region =
      str_replace_all(.data$level_1_region,
                      c(" D.c." = "",
                        "Archipielago De San Andres"
                        = "San Andres, Providencia Y Santa Catalina",
                        "Norte Santander" = "Norte De Santander"
                      )
      ),
    level_2_region = stri_trans_general(level_2_region, "latin-ascii"),
    level_2_region = stri_trim_both(level_2_region),
    level_2_region =
      str_replace_all(.data$level_2_region,
                      c(" D.C." = ""
                      )
      ),
    level_2_region = stringr::str_to_title(level_2_region),
  ) 
  

# anti_join(colombia_municipalities, colombia_departments, by=c("level_1_region"))
colombia_codes <- left_join(colombia_municipalities,
                            colombia_departments,
                            by=c("level_1_region"))

# update package region_codes
usethis::use_data(colombia_codes, overwrite = TRUE)
