test_that("get_germany_regional_cases data source is unchanged", {

  data <- readr::read_csv("https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv")

  expected_colnames = c("FID", "IdBundesland", "Bundesland", "Landkreis", "Altersgruppe", "Geschlecht",
                        "AnzahlFall", "AnzahlTodesfall", "Meldedatum", "IdLandkreis", "Datenstand", "NeuerFall",
                        "NeuerTodesfall", "Refdatum", "NeuGenesen", "AnzahlGenesen", "IstErkrankungsbeginn", "Altersgruppe2")
  expect_true(all(expected_colnames %in% colnames(data)))
})

test_that("get_germany_regional_cases returns the correct column names", {
  expected_colnames <- c("region", "date", "cases_new", "deaths_new")

  returned_colnames <- colnames(get_germany_regional_cases())

  expect_true(all(returned_colnames %in% expected_colnames))
  expect_true(all(expected_colnames %in% returned_colnames))
})

test_that("get_germany_regional_cases returns correct column types", {
  data <- get_germany_regional_cases()
  expect_is(data, "data.frame")
  expect_is(data$region, "character")
  expect_is(data$date, "Date")
  expect_is(data$cases_new, "numeric")
  expect_is(data$deaths_new, "numeric")
})



