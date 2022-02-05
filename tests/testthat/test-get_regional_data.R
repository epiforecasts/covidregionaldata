source("functions/test_initialise.R")
test_get_regional_data <- function(level) {
  test_that(paste0("get_regional_data returns level ", level, " data"), {
    mexico <- readRDS(
      paste0("custom_data/mexico_level_", level, "_snap.rds")
    )
    true <- mexico$return()
    true_R6 <- mexico$clone()
    true_R6$steps <- TRUE
    true_steps <- true_R6$return()
    mockery::stub(
      get_regional_data, "initialise_dataclass",
      test_initialise(class = mexico)
    )
    d <- get_regional_data("mexico", level = level, verbose = FALSE)
    expect_s3_class(d, "data.frame")
    expect_true(sum(as.numeric(d$cases_new) < 0, na.rm = TRUE) == 0)
    expect_equal(
      true_steps,
      get_regional_data("mexico",
        level = level,
        steps = TRUE, verbose = FALSE
      )
    )
    expect_equal(
      true_R6,
      get_regional_data("mexico",
        level = level,
        steps = TRUE, class = TRUE, verbose = FALSE
      )
    )
    d <- get_regional_data("mexico",
      level = level, verbose = FALSE,
      localise = FALSE
    )
    expect_true(any(colnames(d) %in% paste0("level_", level, "_region")))
    d <- get_regional_data("mexico",
      level = level, verbose = FALSE,
      totals = TRUE
    )
    expect_true(any(grepl("total", colnames(d))))
    expect_true(!any(colnames(d) == "date"))
    expect_error(
      get_regional_data("mexico",
        regions = "made up land",
        verbose = FALSE
      )
    )
  })
}

test_get_regional_data("1")
test_get_regional_data("2")
