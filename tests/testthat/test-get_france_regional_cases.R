test_that("get_france_regional_cases data source is unchanged and up to date", {
  skip_on_cran()
  
  cases_reg <- vroom::vroom("https://www.data.gouv.fr/fr/datasets/r/001aca18-df6a-45c8-89e6-f82d689e6c01")
  expect_named(cases_reg, c("reg", "jour", "P_f", "P_h", "P", "T_f", "T_h", "T", "cl_age90", "pop"))

  cases_dep <- vroom::vroom("https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675")
  expect_named(cases_dep, c("dep", "jour", "P", "T", "cl_age90", "pop"))
  
  hosp_dep <- vroom::vroom("https://www.data.gouv.fr/fr/datasets/r/6fadff46-9efd-4c53-942a-54aca783c30c")
  expect_named(hosp_dep, c("dep", "jour", "incid_hosp", "incid_rea", "incid_dc", "incid_rad"))

})

test_that("get_france_regional_cases returns the correct column names", {
  skip_on_cran()
  
  expected_colnames_adm_level_1 <- c("insee_code", "level_1_region_code", "region_level_1", "date", "cases_new", "tested_new")
  
  adm_level_1 <- get_france_regional_cases_only_level_1()
  
  expect_setequal(colnames(adm_level_1), expected_colnames_adm_level_1)


  expected_colnames_adm_level_2 <- c("level_2_region_code", "region_level_2", "level_1_region_code", "region_level_1", "date", "cases_new", "tested_new", "hosp_new", "deaths_new")
  
  adm_level_2 <- get_france_regional_cases_with_level_2()

  expect_setequal(colnames(adm_level_2), expected_colnames_adm_level_2)
})

test_that("get_france_regional_cases returns correct column types", {
  skip_on_cran()
  
  data <- get_france_regional_cases_only_level_1()

  expect_is(data, "data.frame")
  expect_is(data$region_level_1, "character")
  expect_is(data$date, "Date")
  expect_is(data$cases_new, "numeric")
  expect_is(data$tested_new, "numeric")

  data <- get_france_regional_cases_with_level_2()

  expect_is(data, "data.frame")
  expect_is(data$region_level_2, "character")
  expect_is(data$date, "Date")
  expect_is(data$cases_new, "numeric")
  expect_is(data$tested_new, "numeric")
  expect_is(data$hosp_new, "numeric")
  expect_is(data$deaths_new, "numeric")
  
})
