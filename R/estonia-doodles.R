library(readr)
library(dplyr)

# https://www-terviseamet-ee.translate.goog/et/koroonaviirus/avaandmed?_x_tr_sl=et&_x_tr_tl=en&_x_tr_hl=lt&_x_tr_pto=ajax,se,elem

# Based on the test results, location-based statistics are issued by test dates and test results at the level of local governments and settlements. All local governments (rural municipalities, rural municipality cities) and, at the settlement level, the districts of Tallinn, the city of Kuressaare and the “intra-municipal city” of the city of Pärnu are covered.
# 
# The number of cases is given in ranges of 10, ranges can be e.g. "0-9", "10-19" ... "200-209", etc. The lower and upper limits of the ranges are presented as separate figures. The ranges represent a cumulative number in the form of aggregated results for that statistic date, and do not indicate increments.
# 
# Both positive and negative event ranges are published at the location level. A case is considered to be a first positive or first negative test per person, i. repeat tests are not considered. Thus, there can be a maximum of 2 cases per person - one positive, one negative.
# 
# In terms of locations, EHAK codes are also provided to facilitate the creation of map applications. More information on EHAK codes on the Land Board website: https://geoportaal.maaamet.ee/est/Andmed-ja-kaardid/Haldus-ja-asustusjaotus-p119.html
# 
# In addition, the number, area and population density of the population of the respective settlement unit or local government have been compiled into the data on the basis of the report of Statistics Estonia RV0291U.

opendata_covid19_test_location <- read_csv("~/Downloads/opendata_covid19_test_location.csv", 
                                                  col_types = cols(LastStatisticsDate = col_date(format = "%Y-%m-%d"), 
                                             StatisticsDate = col_date(format = "%Y-%m-%d"), 
                                             CountryEHAK = col_integer(), CountyEHAK = col_integer(), 
                                             Commune = col_integer(), CommuneEHAK = col_integer(), 
                                             Village = col_character(), VillageEHAK = col_integer()))
View(opendata_covid19_test_location)                                
last_day_data <- opendata_covid19_test_location %>%
  filter(StatisticsDate == LastStatisticsDate) %>%
  mutate(midpointCount = TotalCasesFrom + 5) %>%
  group_by(LocationEHAK) %>%
  arrange(ResultValue, .by_group = TRUE) %>%
  mutate(TotalTests = midpointCount + lag(midpointCount)) %>%
  mutate(TestPositivity = if_else(ResultValue == "P", midpointCount / TotalTests, NA_real_)) %>%
  filter(ResultValue == "P") %>%
  ungroup()
