library(vroom)

# nolint start
jhu_codes_link <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv"
# nolint end
JHU_codes <- vroom(jhu_codes_link)
JHU_codes <- JHU_codes %>%
  select(
    level_1_region = Combined_Key,
    level_1_region_code = iso3
  )
# update package region_codes
usethis::use_data(JHU_codes, overwrite = TRUE)
