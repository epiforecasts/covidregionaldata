# mexico region codes
mexico_codes <- vroom::vroom("data-raw/mexico_codes.csv")
mexico_codes <- tibble::tibble(
  level = "level_1_region",
  name = "iso_3166_2",
  mexico_codes
)

# update package region_codes
usethis::use_data(mexico_codes, overwrite = TRUE)
