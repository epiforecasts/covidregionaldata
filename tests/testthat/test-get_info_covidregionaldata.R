test_that("get_info_covidregionaldata works as expected", {
  expect_warning(get_info_covidregionaldata())
  expect_s3_class(
    suppressWarnings(get_info_covidregionaldata()), "data.frame"
  )
})
