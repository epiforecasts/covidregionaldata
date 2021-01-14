test_that("get_india_regional_cases data source is unchanged and up to date", {
  skip_on_cran()
  
  data <- vroom::vroom("https://api.covid19india.org/csv/latest/state_wise_daily.csv")
  expected_colnames <- c("Date", "Status", "TT", "AN", 	"AP", 	"AR", 	"AS", 	"BR", 	"CH", 	"CT", 	"DN",
                         "DD", 	"DL", 	"GA", 	"GJ", 	"HR", 	"HP", 	"JK", 	"JH",
                         "KA", 	"KL", 	"LA", 	"LD", 	"MP", 	"MH", 	"MN", 	"ML",
                         "MZ", 	"NL", 	"OR", 	"PY", 	"PB", 	"RJ", 	"SK", 	"TN",
                         "TG", 	"TR", 	"UP", 	"UT", 	"WB")
  expect_true(all(expected_colnames %in% colnames(data)))
  expect_true(max(lubridate::dmy(data$Date), na.rm = TRUE) > Sys.Date() - 7)
})

test_that("get_india_regional_cases returns the correct column names", {
  skip_on_cran()
  
  expected_colnames <- c("region_level_1", "date", "cases_new",  "deaths_new", "recovered_new")

  returned_colnames <- colnames(get_india_regional_cases())

  expect_true(all(returned_colnames %in% expected_colnames))
  expect_true(all(expected_colnames %in% returned_colnames))
})

test_that("get_india_regional_cases returns correct column types", {
  skip_on_cran()
  
  data <- get_india_regional_cases()
  expect_is(data, "data.frame")
  expect_is(data$region_level_1, "character")
  expect_is(data$date, "Date")
  expect_is(data$cases_new, "numeric")
  expect_is(data$deaths_new, "numeric")
  expect_is(data$recovered_new, "numeric")
})
