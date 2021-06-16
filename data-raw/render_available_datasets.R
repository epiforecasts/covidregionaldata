# render available datasets table

all_country_data <- get_regional_data(render = TRUE)
usethis::use_data(all_country_data, overwrite = TRUE)
