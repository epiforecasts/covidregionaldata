# helper functions
source("tests/testthat/functions/dummy_class.R")
# save level 1 mexico data for testing
mexico <- covidregionaldata::Mexico$new(verbose = TRUE)
mexico <- dummy_class(mexico)
saveRDS(mexico, "tests/testthat/custom_data/mexico_level_1_snap.rds")

# save level 2 mexico data for testing
# (downsample for storage and speed)
mexico <- covidregionaldata::Mexico$new(level = "2", verbose = TRUE)
mexico <- dummy_class(mexico)
saveRDS(mexico, "tests/testthat/custom_data/mexico_level_2_snap.rds")

# save a snapshot of the ECDC data
ecdc <- ECDC$new(verbose = TRUE)
ecdc <- dummy_class(ecdc)
saveRDS(ecdc, "tests/testthat/custom_data/ecdc.rds")
vroom::vroom_write(ecdc$data$clean, "tests/testthat/custom_data/ecdc.csv")
# save a snapshot of the WHO data
who <- WHO$new(verbose = TRUE)
who <- dummy_class(who)
saveRDS(who, "tests/testthat/custom_data/who.rds")
