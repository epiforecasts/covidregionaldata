test_that("get_lithuania_regional_cases data source is unchanged and up to date", {
  skip_on_cran()

    data <- vroom::vroom("https://opendata.arcgis.com/datasets/d49a63c934be4f65a93b6273785a8449_0.csv")

  expected_colnames = c("object_id", "date", "municipality_code",
                        "municipality_name", "population",
                        "ab_pos_day", "ab_neg_day", "ab_tot_day", "ab_prc_day",
                        "ag_pos_day", "ag_neg_day", "ag_tot_day", "ag_prc_day",
                        "pcr_pos_day", "pcr_neg_day", "pcr_tot_day", "pcr_prc_day",
                        "dgn_pos_day", "dgn_neg_day", "dgn_tot_day", "dgn_prc_day",
                        "dgn_tot_day_gmp",
                        "daily_deaths_def1", "daily_deaths_def2", "daily_deaths_def3",
                        "daily_deaths_all", "incidence", "cumulative_totals",
                        "active_de_jure", "active_sttstcl", "dead_cases",
                        "recovered_de_jure", "recovered_sttstcl")
  expect_true(all(expected_colnames %in% colnames(data)))
  expect_true(max(as.Date(data$date), na.rm = TRUE) > Sys.Date() - 7)
})

test_that("get_lithuania_regional_cases returns the correct column names", {
  skip_on_cran()
  
  expected_colnames_adm_level_1 <- c( "date", "region_level_1", "cases_new", "cases_total", "tested_new",
                                      "deaths_new")
  returned_colnames_adm_level_1 <- colnames(get_lithuania_regional_cases_only_level_1())
  ##
  # We are currently passing back many additional data which aren't part
  # of the covidregionaldata framework, so we only check that the ones
  # which covidregionaldata does expect are there.
  #expect_true(all(returned_colnames_adm_level_1 %in% expected_colnames_adm_level_1))
  expect_true(all(expected_colnames_adm_level_1 %in% returned_colnames_adm_level_1))


  expected_colnames_adm_level_2 <- c("date", "region_level_1", "region_level_2","cases_new", "cases_total", "tested_new",
                                     "deaths_new")
  returned_colnames_adm_level_2 <- colnames(get_lithuania_regional_cases_with_level_2())
  ##
  # We are currently passing back many additional data which aren't part
  # of the covidregionaldata framework, so we only check that the ones
  # which covidregionaldata does expect are there.
  #expect_true(all(returned_colnames_adm_level_2 %in% expected_colnames_adm_level_2))
  expect_true(all(expected_colnames_adm_level_2 %in% returned_colnames_adm_level_2))
})

test_that("get_lithuania_regional_cases returns correct column types", {
  skip_on_cran()
  
  data <- get_lithuania_regional_cases_only_level_1()

  expect_is(data, "data.frame")
  expect_is(data$region_level_1, "character")
  expect_is(data$date, "Date")
  expect_is(data$cases_new, "numeric")
  expect_is(data$cases_total, "numeric")
  expect_is(data$deaths_new, "numeric")
  expect_is(data$tested_new, "numeric")


  data <- get_lithuania_regional_cases_with_level_2()

  expect_is(data, "data.frame")
  expect_is(data$region_level_1, "character")
  expect_is(data$region_level_2, "character")
  expect_is(data$date, "Date")
  expect_is(data$cases_new, "numeric")
  expect_is(data$cases_total, "numeric")
  expect_is(data$deaths_new, "numeric")
  expect_is(data$tested_new, "numeric")
})

test_that("get_lithuania_regional_cases returns correct numbers of regions", {
  skip_on_cran()
  
  adm_1_data <- get_lithuania_regional_cases_only_level_1()
  adm_2_data <- get_lithuania_regional_cases_with_level_2()

  expect_equal(length(unique(na.omit(adm_1_data$region_level_1))), 11)
  expect_equal(length(unique(na.omit(adm_2_data$region_level_2))), 61)
})


