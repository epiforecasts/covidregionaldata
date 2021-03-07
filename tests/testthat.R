library(testthat)
library(covidregionaldata)
source("testthat/custom_tests/mock_data.R")
source("testthat/custom_tests/regional_dataset_tests.R")

test_check("covidregionaldata")
