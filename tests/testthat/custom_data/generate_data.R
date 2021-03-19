# save level 1 mexico data for testing
mexico <- get_regional_data("mexico", steps = TRUE, level = "1")
saveRDS(mexico, "tests/testthat/custom_data/mexico_level_1_snap.rds")

# save level 2 mexico data for testing
# (downsample for storage and speed)
mexico <- get_regional_data("mexico", steps = TRUE, level = "2")
mexico$raw <- dplyr::slice(mexico$raw, 1:1000)
mexico <- clean_regional(mexico)
mexico <- process_regional(mexico)
saveRDS(mexico, "tests/testthat/custom_data/mexico_level_2_snap.rds")

# save a snapshot of the ECDC data
ecdc <- get_national_data(source = "ecdc", steps = TRUE)
ecdc$raw <- dplyr::slice(ecdc$raw, 1:1000)
ecdc <- clean_regional(ecdc)
ecdc <- process_regional(ecdc)
ecdc <- return_regional(ecdc, steps = TRUE)
saveRDS(ecdc, "tests/testthat/custom_data/ecdc.rds")

# save a snapshot of the WHO data
who <- get_national_data(source = "who", steps = TRUE)
who$raw <- dplyr::slice(who$raw, 1:1000)
who <- clean_regional(who)
who <- process_regional(who)
who <- return_regional(who, steps = TRUE)
saveRDS(who, "tests/testthat/custom_data/who.rds")
