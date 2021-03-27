# global region codes
global_codes <- tibble::tibble(
  country = c("who", "ecdc"),
  level = c("level_1_region"),
  name = c("iso_code"),
  codes = NULL
)

# mexico region codes
mexico_codes <- vroom::vroom("data-raw/mexico_codes.csv")
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
italy_codes <- vroom::vroom("data-raw/italy_codes.csv")
italy_codes <- tibble::tibble(
  country = "italy",
  level = c("level_1_region"),
  name = c("iso_3166_2"),
  codes = list(
    italy_codes
  )
)

# germany codes
level_1_germany <- tibble::tibble(
  level_1_region_code = c(
    "DE-BB", "DE-BE", "DE-BW", "DE-BY", "DE-HB", "DE-HE", "DE-HH", "DE-MV",
    "DE-NI", "DE-NW", "DE-RP", "DE-SH", "DE-SL", "DE-SN", "DE-ST", "DE-TH"
  ),
  region_level_1 = c(
    "Brandenburg", "Berlin", "Baden-W\u00FCrttemberg", "Bayern", "Bremen",
    "Hessen", "Hamburg", "Mecklenburg-Vorpommern", "Niedersachsen",
    "Nordrhein-Westfalen",
    "Rheinland-Pfalz", "Schleswig-Holstein", "Saarland", "Sachsen",
    "Sachsen-Anhalt", "Th\u00FCringen"
  )
)
germany_codes <- tibble::tibble(
  country = "germany",
  level = c("level_1_region", "level_2_region"),
  name = c("iso_3166_2", "code"),
  codes = list(
    level_1_germany,
    dplyr::mutate(
      level_1_germany,
      level_2_region_code = NA,
    )
  )
)

# india codes

india_state_names <- tibble::tibble(
  code = c(
    "AN", "AP", "AR", "AS", "BR", "CH", "CT", "DN", "DD",
    "DL", "GA", "GJ", "HR", "HP", "JK", "JH", "KA", "KL",
    "LA", "LD", "MP", "MH", "MN", "ML", "MZ", "NL", "OR",
    "PY", "PB", "RJ", "SK", "TN", "TG", "TR", "UN", "UP", "UT", "WB"
  ),
  region_level_1 = c(
    "Andaman and Nicobar", "Andhra Pradesh", "Arunachal Pradesh", "Assam", "Bihar",
    "Chandigarh", "Chhattisgarh", "Dadra and Nagar Haveli", "Daman and Diu",
    "NCT of Delhi", "Goa", "Gujarat", "Haryana", "Himachal Pradesh", "Jammu and Kashmir",
    "Jharkhand", "Karnataka", "Kerala", "Ladakh", "Lakshadweep", "Madhya Pradesh",
    "Maharashtra", "Manipur", "Meghalaya", "Mizoram", "Nagaland", "Odisha",
    "Puducherry", "Punjab", "Rajasthan", "Sikkim", "Tamil Nadu", "Telangana",
    "Tripura", "Unknown", "Uttar Pradesh", "Uttarakhand", "West Bengal"
  )
)

india_codes <- tibble::tibble(
  country = "india",
  level = c("level_1_region"),
  name = c("iso_3166_2"),
  codes = list(
    india_state_names
  )
)

# add additional regions in the same format and bind together
region_codes <- purrr::reduce(
  list(
    global_codes, mexico_codes,
    india_codes,
    italy_codes, germany_codes
  ),
  dplyr::bind_rows
)

# update package region_codes
usethis::use_data(region_codes, overwrite = TRUE)
