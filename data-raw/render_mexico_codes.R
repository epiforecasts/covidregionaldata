# mexico region codes
mexico_codes <- vroom::vroom("data-raw/mexico_codes.csv")
mexico_codes <- tibble::tibble(
  level = c("level_1_region", "level_2_region"),
  name = c("iso_3166_2", "inegi_code"),
  codes = list(
    dplyr::select(mexico_codes, region_level_1, iso_code) %>%
      unique(),
    mexico_codes
  )
)

# update package region_codes
usethis::use_data(mexico_codes, overwrite = TRUE)
