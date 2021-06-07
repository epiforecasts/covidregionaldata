test_get_regional_data_region_checks <- function(source, max_level) {
  data_level_1 <- get_regional_data(source, level = 1, localise = FALSE)
  if (max_level >= 2) {
    data_level_2 <- get_regional_data(source, level = 2, localise = FALSE)
  }

  expect_lte(
    nrow(
      unique(
        data_level_1 %>%
          dplyr::select(level_1_region, level_1_region_code) %>%
          dplyr::filter(is.na(level_1_region_code))
        )), 1)
  if (max_level >= 2) {
    expect_equal(
      length(unique(data_level_1$level_1_region)),
      length(unique(data_level_2$level_1_region)))
    expect_lte(
      length(unique(data_level_2$level_1_region)),
      length(unique(data_level_2$level_2_region)))
    expect_lte(
      nrow(
        unique(
          data_level_2 %>%
            dplyr::select(level_1_region, level_1_region_code) %>%
            dplyr::filter(is.na(level_1_region_code)))), 1)
    if (!is.null(data_level_2$level_2_region_code)) {
      expect_lte(
        nrow(
          unique(
            data_level_2 %>%
              dplyr::select(level_2_region, level_2_region_code) %>%
              dplyr::filter(is.na(level_2_region_code)))), 1)
    }
  }
}
