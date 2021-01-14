test_that("get_germany_regional_cases data source is unchanged and up to date", {
  skip_on_cran()
  
  data <- vroom::vroom("https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv")

  expected_colnames = c("ObjectId", "IdBundesland", "Bundesland", "Landkreis", "Altersgruppe", "Geschlecht",
                        "AnzahlFall", "AnzahlTodesfall", "Meldedatum", "IdLandkreis", "Datenstand", "NeuerFall",
                        "NeuerTodesfall", "Refdatum", "NeuGenesen", "AnzahlGenesen", "IstErkrankungsbeginn", "Altersgruppe2")
  expect_true(all(expected_colnames %in% colnames(data)))
  expect_true(max(as.Date(data$Meldedatum), na.rm = TRUE) > Sys.Date() - 7)
})

test_that("get_germany_regional_cases returns the correct column names", {
  skip_on_cran()
  
  expected_colnames_adm_level_1 <- c("region_level_1", "date", "cases_new", "deaths_new")
  returned_colnames_adm_level_1 <- colnames(get_germany_regional_cases_only_level_1())
  expect_true(all(returned_colnames_adm_level_1 %in% expected_colnames_adm_level_1))
  expect_true(all(expected_colnames_adm_level_1 %in% returned_colnames_adm_level_1))


  expected_colnames_adm_level_2 <- c("region_level_1", "region_level_2", "date", "cases_new", "deaths_new")
  returned_colnames_adm_level_2 <- colnames(get_germany_regional_cases_with_level_2())
  expect_true(all(returned_colnames_adm_level_2 %in% expected_colnames_adm_level_2))
  expect_true(all(expected_colnames_adm_level_2 %in% returned_colnames_adm_level_2))
})

test_that("get_germany_regional_cases returns correct column types", {
  skip_on_cran()
  
  data <- get_germany_regional_cases_only_level_1()

  expect_is(data, "data.frame")
  expect_is(data$region_level_1, "character")
  expect_is(data$date, "Date")
  expect_is(data$cases_new, "numeric")
  expect_is(data$deaths_new, "numeric")


  data <- get_germany_regional_cases_with_level_2()

  expect_is(data, "data.frame")
  expect_is(data$region_level_1, "character")
  expect_is(data$region_level_2, "character")
  expect_is(data$date, "Date")
  expect_is(data$cases_new, "numeric")
  expect_is(data$deaths_new, "numeric")
})

test_that("get_germany_regional_cases returns correct numbers of regions", {
  skip_on_cran()
  
  adm_1_data <- get_germany_regional_cases_only_level_1()
  adm_2_data <- get_germany_regional_cases_with_level_2()

  expect_equal(length(unique(na.omit(adm_1_data$region_level_1))), 16)
  expect_gt(length(unique(na.omit(adm_2_data$region_level_2))), 400)
})


