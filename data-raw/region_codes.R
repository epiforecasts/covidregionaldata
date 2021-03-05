
# mexico region codes
mexico_codes <- readr::read_csv("data-raw/mexico_codes.csv")
region_codes <- tibble::tibble(
  country = "mexico",
  level = c("level_1_region", "level_2_region"),
  name = c("iso_3166_2", "inegi_code"),
  codes = list(
    dplyr::select(mexico_codes, region_level_1, iso_code) %>%
      unique(),
    mexico_codes
  )
)

# add additional regions in the same format and bind together

# update package region_codes
usethis::use_data(region_codes, overwrite = TRUE)
