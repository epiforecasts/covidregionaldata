test_that("get_mexico_regional_cases returns correct column names and types", {
  skip_on_cran()
  
  adm_level_1 <- get_mexico_regional_cases_only_level_1()
  
  expected_colnames_adm_level_1 <- c("date", "level_1_region_code", "region_level_1", "level_1_region_code", "cases_new", "deaths_new")
  
  expect_setequal(colnames(adm_level_1), expected_colnames_adm_level_1)
  
  expect_s3_class(adm_level_1, "data.frame")
  expect_s3_class(adm_level_1$date, "Date")
  expect_type(adm_level_1$region_level_1, "character")
  expect_type(adm_level_1$level_1_region_code, "character")
  expect_type(adm_level_1$cases_new, "double")
  expect_type(adm_level_1$deaths_new, "double")
  
  expect_length(unique(adm_level_1$region_level_1), 32)

  adm_level_2 <- get_mexico_regional_cases_with_level_2()
  
  expected_colnames_adm_level_2 <- c("level_2_region_code", "region_level_2", "level_1_region_code", "region_level_1", "date", "cases_new", "deaths_new")
  
  expect_setequal(colnames(adm_level_2), expected_colnames_adm_level_2)

  expect_s3_class(adm_level_2, "data.frame")
  expect_s3_class(adm_level_2$date, "Date")
  expect_type(adm_level_2$region_level_1, "character")
  expect_type(adm_level_2$region_level_2, "character")
  expect_type(adm_level_2$level_1_region_code, "character")
  expect_type(adm_level_2$level_2_region_code, "character")
  expect_type(adm_level_2$cases_new, "double")
  expect_type(adm_level_2$deaths_new, "double")
  
})
