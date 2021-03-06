test_get_regional_data <- function(level) {
  test_that(paste0("get_regional_data returns level ", level, " data"), {
    true <- readRDS(paste0("custom_data/mexico_level_", level, ".rds"))
    raw <- true
    raw$clean <- NULL
    raw$processed <- NULL
    raw$return <- NULL
    mockery::stub(get_regional_data, "download_regional", raw)
    d <- get_regional_data("mexico", level = level)
    expect_s3_class(d, "data.frame")
    expect_true(sum(as.numeric(d$cases_new) < 0, na.rm = TRUE) == 0)
    expect_equal(true, get_regional_data("mexico", level = level, steps = TRUE))
  })
}

test_get_regional_data("1")
test_get_regional_data("2")
