source('./custom_tests/expect_colname.R')

test_that("get_germany_regional_cases cases works as expected", {
  
  base <- get_germany_regional_cases()
  expect_is(base, "data.frame")
  expect_true(class(base$date) == 'Date')
  expect_true(sum(as.numeric(base$cases) < 0) == 0)
  
})

test_that("get_germany_regional_cases data source is unchanged", {
  
  base <- readr::read_csv("https://raw.githubusercontent.com/jgehrcke/covid-19-germany-gae/master/data.csv")
  
  expected_colnames = c("time_iso8601", "source", "DE-BW_cases", "DE-BW_deaths", "DE-BY_cases", 
                        "DE-BY_deaths", "DE-BE_cases", "DE-BE_deaths", "DE-BB_cases", "DE-BB_deaths", 
                        "DE-HB_cases", "DE-HB_deaths", "DE-HH_cases", "DE-HH_deaths", "DE-HE_cases", 
                        "DE-HE_deaths", "DE-MV_cases", "DE-MV_deaths", "DE-NI_cases", "DE-NI_deaths", 
                        "DE-NW_cases", "DE-NW_deaths", "DE-RP_cases", "DE-RP_deaths", "DE-SL_cases", 
                        "DE-SL_deaths", "DE-SN_cases", "DE-SN_deaths", "DE-SH_cases", "DE-SH_deaths", 
                        "DE-ST_cases", "DE-ST_deaths", "DE-TH_cases", "DE-TH_deaths", "sum_cases", "sum_deaths")
  
  sapply(expected_colnames, expect_colname, colnames = colnames(base))
  
})




