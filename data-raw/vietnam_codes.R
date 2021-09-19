# Set vietnam region codes
#
# Level 1 codes: ISO-3166-2
# Source: https://en.wikipedia.org/wiki/ISO_3166-2:VN
#
library(rvest)
library(stringi)
library(stringr)
library(dplyr)
library(tibble)

# Level 1 -----------------------------------------------------------------
# Get ISO codes
vn_iso <- "https://en.wikipedia.org/wiki/ISO_3166-2:VN"

level_1_region_df <- read_html(vn_iso) %>%
  html_element(css="table.wikitable:nth-child(11)") %>%
  html_table()

vietnam_codes <- data.frame(
  level_1_region_code = level_1_region_df$Code,
  level_1_region = level_1_region_df$`Subdivision name (vi)`,
  stringsAsFactors = FALSE
) %>%
  mutate(
    level_1_region = stri_trans_general(level_1_region, "latin-ascii"),
    level_1_region = stri_trim_both(level_1_region),
    level_1_region = str_replace_all(level_1_region, '\\(.*\\)|-| ', ''),
    level_1_region = str_to_title(level_1_region)
  )

# update package region_codes
usethis::use_data(vietnam_codes, overwrite = TRUE)
