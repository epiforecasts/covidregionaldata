# render available datasets table

envi <- ls(getNamespace("covidregionaldata"), all.names = TRUE)
# regional data
starts_with_capitals_idx <- grep("^[A-Z]", envi)
starts_with_capitals <- envi[starts_with_capitals_idx]
exclude <- c("DataClass", "CountryDataClass")
valid_country_objects <- lapply(
  starts_with_capitals,
  function(x) {
    country_obj <- get(x)
    if (class(country_obj) == "R6ClassGenerator" & !(x %in% c(exclude))) {
      dat <- get(x)$new()
      dat <- dat$summary()
      return(dat)
    }
  }
)
all_country_data <- valid_country_objects %>%
  bind_rows()
usethis::use_data(all_country_data, overwrite = TRUE)
