# Set Mexico region codes
# 
# Level 1 codes: ISO-3166-2
# Source: https://en.wikipedia.org/wiki/ISO_3166-2:MX
# 
# Level 2 codes: INEGI Mexican official statistics geocoding
# Source: Raw data
# Note: Level 1 INEGI codes are the first 2 characters of Level 2 INEGI codes
library(rvest)
library(stringi)

# Level 1 -----------------------------------------------------------------
# Get ISO codes
mx_iso <- "https://en.wikipedia.org/wiki/ISO_3166-2:MX"

level_1_region_code <- read_html(mx_iso) %>%
  html_nodes(".monospaced , .jquery-tablesorter td") %>%
  html_text()
level_1_region_code <- level_1_region_code[2:33]

region_level_1 <- read_html(mx_iso) %>%
  html_nodes("tr:nth-child(1) .monospaced , td:nth-child(2)") %>%
  html_text()
region_level_1 <- region_level_1[1:32]

level_1 <- data.frame(level_1_region_code, region_level_1) %>%
  mutate(region_level_1 = stri_trans_general(region_level_1, "latin-ascii"),
         region_level_1 = stri_trim_both(region_level_1))

# Some ISO names need replacements to match data
replacements <- list(
  "Coahuila de Zaragoza" = "Coahuila",
  "Michoacan de Ocampo" = "Michoacan",
  "Veracruz de Ignacio de la Llave" = "Veracruz")
level_1 <- level_1 %>%
  mutate(region_level_1 = ifelse(region_level_1 %in% names(replacements),
                                 replacements[region_level_1],
                                 region_level_1),
         region_level_1 = as.character(region_level_1)) 
  

# Use source data to get level 1 regions INEGI codes
# - used for matching with level 2
data_l1 <- check_country_available(country = "Mexico", level = "1")
data_l1 <- data_l1$download() %>%
  select(inegi_level1 = cve_ent, region_level_1 = nombre) %>%
  distinct()
inegi_l1 <- data_l1 %>%
  mutate(region_level_1 = stri_trans_totitle(region_level_1),
         region_level_1 = ifelse(region_level_1 == "Distrito Federal",
                                 "Ciudad de Mexico", region_level_1)) %>%
  filter(region_level_1 != "Nacional")

# Match INEGI and ISO codes 
level_1_full <- left_join(level_1, inegi_l1, by = "region_level_1")

# Level 2 -----------------------------------------------------------------
# Use source data to get level 2 INEGI codes
data_l2 <- check_country_available(country = "Mexico", level = "2")
data_l2 <- data_l2$download()
level_2 <- data_l2 %>%
  select(level_2_region_code = cve_ent,
         region_level_2 = nombre) %>%
  distinct()

# Get the level 1 INEGI code to match to level 1 regions
# - to get level 1 ISO codes matched with level 2 regions
level_2_full <- level_2 %>%
  mutate(inegi_level1 = stri_sub(level_2_region_code, 1, 2)) %>%
  left_join(level_1_full, by = "inegi_level1")

# Long join and save --------------------------------------------------
mexico_codes <- bind_rows(level_1_full, level_2_full) %>%
  select(-inegi_level1)

vroom::vroom_write(mexico_codes, "data-raw/mexico_codes.csv")
 
# update package region_codes
usethis::use_data(mexico_codes, overwrite = TRUE)
