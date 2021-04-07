all_levels <- list("1", "2", "nhs")
region_names <- list("1" = "region", "2" = "province", "nhs" = "county")
region_codes <- list("1" = "iso", "2" = "code")

test_that("region_dispatch works as expected on level 1", {
  dis <- region_dispatch("1", all_levels, region_names, region_codes)
  expected <- c("region", "iso")
  names(expected) <- c("level_1_region", "level_1_region_code")
  expect_equal(dis, expected)
})

test_that("region_dispatch works as expected on level 2", {
  dis <- region_dispatch("2", all_levels, region_names, region_codes)
  expected <- c("region", "iso", "province", "code")
  names(expected) <- c(
    "level_1_region", "level_1_region_code",
    "level_2_region", "level_2_region_code"
  )
  expect_equal(dis, expected)
})

test_that("region_dispatch works as expected on a custom partial level", {
  dis <- region_dispatch("nhs", all_levels, region_names, region_codes)
  expected <- c("region", "iso", "province", "code", "county")
  names(expected) <- c(
    "level_1_region", "level_1_region_code",
    "level_2_region", "level_2_region_code",
    "level_nhs_region"
  )
  expect_equal(dis, expected)
})
