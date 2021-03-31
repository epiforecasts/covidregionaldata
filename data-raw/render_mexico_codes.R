# mexico region codes
# For ISO-3166-2 codes, see: https://www.iso.org/obp/ui/#iso:code:3166:MX
# ISO codes were matched by name to INEGI codes in source data
# Note that Level 1 codes are the first 2 characters of Level 2 codes
# Level 2 codes remain as provided in source data
mexico_codes <- vroom::vroom("data-raw/mexico_codes.csv")
mexico_codes <- tibble::tibble(
  level = "level_1_region",
  name = "iso_3166_2",
  mexico_codes
)

# update package region_codes
usethis::use_data(mexico_codes, overwrite = TRUE)
