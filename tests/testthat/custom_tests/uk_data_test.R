# create data for testing uk data function
uk_level_1_default <- get_regional_data("uk",
  level = 1,
  class = T,
  verbose = F
)

uk_level_2_default <- get_regional_data("uk",
  level = 2,
  class = T,
  verbose = F
)

uk_level_1_totals <- get_regional_data("uk",
  level = 1,
  class = T,
  verbose = F,
  totals = T
)
uk_level_2_totals <- get_regional_data("uk",
  level = 2,
  class = T,
  verbose = F,
  totals = T
)
uk_level_1_nhsregions <- get_regional_data("uk",
  level = 1,
  class = T,
  verbose = F,
  nhsregions = T
)

uk_level_2_resolution_ltla <- get_regional_data("uk",
  level = 2,
  class = T,
  verbose = F,
  resolution = "ltla"
)

uk_level_1_release_date <- get_regional_data("uk",
  level = 1,
  class = T,
  verbose = F,
  release_date = "2021-01-01"
)

uk_level_2_release_date <- get_regional_data("uk",
  level = 2,
  class = T,
  verbose = F,
  release_date = "2021-01-01"
)
