# render available datasets table

all_country_data <- get_available_datasets(render = TRUE)
usethis::use_data(all_country_data, overwrite = TRUE)
# 