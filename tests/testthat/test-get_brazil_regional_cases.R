test_that("get_brazil_regional_cases data source is unchanged and up to date", {
  skip_on_cran()
  
  data <- vroom::vroom("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv.gz")
  expected_colnames <- c("date", "country", "state", "city", "ibgeID", "newDeaths", "deaths",
                         "newCases", "totalCases", "deaths_per_100k_inhabitants", "totalCases_per_100k_inhabitants",
                         "deaths_by_totalCases", "_source")
  expect_true(all(expected_colnames %in% colnames(data)))
  expect_true(max(as.Date(data$date), na.rm = TRUE) > Sys.Date() - 7)
})

test_that("get_brazil_regional_cases returns the correct column names", {
  skip_on_cran()
  
  expected_colnames_adm_level_1 <- c("region_level_1", "date", "cases_new", "cases_total", "deaths_total", "deaths_new")
  returned_colnames_adm_level_1 <- colnames(get_brazil_regional_cases_only_level_1())
  expect_true(all(returned_colnames_adm_level_1 %in% expected_colnames_adm_level_1))
  expect_true(all(expected_colnames_adm_level_1 %in% returned_colnames_adm_level_1))


  expected_colnames_adm_level_2 <- c("region_level_1", "region_level_2", "date", "cases_new", "cases_total", "deaths_total", "deaths_new")
  returned_colnames_adm_level_2 <- colnames(get_brazil_regional_cases_with_level_2())
  expect_true(all(returned_colnames_adm_level_2 %in% expected_colnames_adm_level_2))
  expect_true(all(expected_colnames_adm_level_2 %in% returned_colnames_adm_level_2))
})

test_that("get_brazil_regional_cases returns correct column types", {
  skip_on_cran()
  
  data <- get_brazil_regional_cases_only_level_1()

  expect_is(data, "data.frame")
  expect_is(data$region_level_1, "character")
  expect_is(data$date, "Date")
  expect_is(data$cases_new, "numeric")
  expect_is(data$cases_total, "numeric")
  expect_is(data$deaths_new, "numeric")
  expect_is(data$deaths_total, "numeric")


  data <- get_brazil_regional_cases_with_level_2()

  expect_is(data, "data.frame")
  expect_is(data$region_level_1, "character")
  expect_is(data$region_level_2, "character")
  expect_is(data$date, "Date")
  expect_is(data$cases_new, "numeric")
  expect_is(data$cases_total, "numeric")
  expect_is(data$deaths_new, "numeric")
  expect_is(data$deaths_total, "numeric")
})

test_that("get_brazil_regional_cases returns correct numbers of regions", {
  adm_1_data <- get_brazil_regional_cases_only_level_1()

  expect_equal(length(unique(na.omit(adm_1_data$region_level_1))), 27)
})
