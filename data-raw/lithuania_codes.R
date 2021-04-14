library(vroom)

lithuania_codes <- vroom("data-raw/lithuania_codes.csv")

# update package region_codes
usethis::use_data(lithuania_codes, overwrite = TRUE)
