# italy codes
italy_codes <- vroom::vroom("data-raw/italy_codes.csv")
italy_codes <- tibble::tibble(
  level = c("level_1_region"),
  name = c("iso_3166_2"),
  italy_codes
)

# update package region_codes
usethis::use_data(italy_codes, overwrite = TRUE)
