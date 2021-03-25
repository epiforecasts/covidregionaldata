# helper functions
source("tests/testthat/functions/replacePublicR6Method.R")
# save level 1 mexico data for testing
mexico <- covidregionaldata::Mexico$new(verbose = FALSE)
mexico$download()
mexico$data$raw <- dplyr::slice(mexico$data$raw, 1:1000)
mexico$clean()
mexico$process()
replacePublicR6Method(mexico, "download", function() {
  return(invisible(NULL))
})
saveRDS(mexico, "tests/testthat/custom_data/mexico_level_1_snap.rds")

# save level 2 mexico data for testing
# (downsample for storage and speed)
mexico <- covidregionaldata::Mexico$new(level = "2", verbose = FALSE)
mexico$download()
mexico$data$raw <- dplyr::slice(mexico$data$raw, 1:1000)
mexico$clean()
mexico$process()
replacePublicR6Method(mexico, "download", function() {
  return(invisible(NULL))
})
saveRDS(mexico, "tests/testthat/custom_data/mexico_level_2_snap.rds")

# save a snapshot of the ECDC data
ecdc <- ECDC$new(verbose = FALSE, steps = FALSE)
ecdc$download()
ecdc$data$raw <- dplyr::slice(ecdc$data$raw, 1:1000)
ecdc$clean()
ecdc$process()
replacePublicR6Method(ecdc, "download", function() {
  return(invisible(NULL))
})
saveRDS(ecdc, "tests/testthat/custom_data/ecdc.rds")

# save a snapshot of the WHO data
who <- WHO$new(verbose = FALSE)
who$download()
who$data$raw <- dplyr::slice(who$data$raw, 1:1000)
who$clean()
who$process()
replacePublicR6Method(who, "download", function() {
  return(invisible(NULL))
})
saveRDS(who, "tests/testthat/custom_data/who.rds")
