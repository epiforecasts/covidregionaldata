# global region codes
global_codes <- tibble::tibble(
  country = c("who", "ecdc"),
  level = c("level_1_region"),
  name = c("iso_code"),
  codes = NULL
)

# mexico region codes
mexico_codes <- readr::read_csv("data-raw/mexico_codes.csv")
mexico_codes <- tibble::tibble(
  country = "mexico",
  level = c("level_1_region", "level_2_region"),
  name = c("iso_3166_2", "inegi_code"),
  codes = list(
    dplyr::select(mexico_codes, region_level_1, iso_code) %>%
      unique(),
    mexico_codes
  )
)

# italy codes
italy_codes <- readr::read_csv("data-raw/italy_codes.csv")
italy_codes <- tibble::tibble(
  country = "italy",
  level = c("level_1_region"),
  name = c("iso_3166_2"),
  codes = list(
    italy_codes
  )
)

# add additional regions in the same format and bind together
region_codes <- purrr::reduce(list(global_codes, mexico_codes,
                                   italy_codes),
                                   dplyr::bind_rows)

# update package region_codes
usethis::use_data(region_codes, overwrite = TRUE)
