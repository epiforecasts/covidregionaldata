# save level 1 mexico data for testing
mexico <- get_regional_data("mexico", steps = TRUE, level = "1")
saveRDS(mexico, "tests/testthat/custom_data/mexico_level_1.rds")

# save level 2 mexico data for testing 
# (downsample for storage and speed)
mexico <- get_regional_data("mexico", steps = TRUE, level = "2")
mexico$raw <- dplyr::slice(mexico$raw, 1:1000)
mexico <- clean_regional(mexico)
mexico <- process_regional(mexico)
saveRDS(mexico, "tests/testthat/custom_data/mexico_level_2.rds")
