test_that("get_canada_regional_cases cases works as expected", {
  
  base <- get_canada_regional_cases(out = 'timeseries')
  expect_is(base, "data.frame")
  expect_is(base$date, "Date")
  expect_true(sum(as.numeric(base$cases_probable) < 0) == 0)
  expect_true(sum(as.numeric(base$deaths) < 0) == 0)
  expect_true(sum(as.numeric(base$cases_confirmed) < 0) == 0)
  
  base <- get_canada_regional_cases(out = 'total')
  expect_is(base, "data.frame")
  expect_true(sum(as.numeric(base$cases_probable) < 0) == 0)
  expect_true(sum(as.numeric(base$deaths) < 0) == 0)
  expect_true(sum(as.numeric(base$cases_confirmed) < 0) == 0)
  
})

test_that("get_canada_regional_cases data source is unchanged", {
  
  base <- readr::read_csv("https://health-infobase.canada.ca/src/data/covidLive/covid19.csv")
  
  expected_colnames = c("pruid", "prname", "prnameFR", "date", "numconf", "numprob", 
                        "numdeaths", "numtotal", "numtested", "numrecover", "percentrecover", 
                        "ratetested", "numtoday", "percentoday")
  
  sapply(expected_colnames, expect_colname, colnames = colnames(base))
  
})
