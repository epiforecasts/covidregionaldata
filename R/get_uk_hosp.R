

library(dplyr)
library(rio)

#they actually have a nice api system in place, so might be a nicer way to do this!
#tried playing a bit with sprintf(), but doesn't work well with url format

#base query url
base_url <- "https://api.coronavirus-staging.data.gov.uk/v1/data?filters=areaType="

#available geographies (regions and countries):
regions <- c(
  #regions:
  "nhsregion;areaName=London", "nhsregion;areaName=East%2520of%2520England",
  "nhsregion;areaName=Midlands", "nhsregion;areaName=North%2520West",
  "nhsregion;areaName=North%2520East%2520and%2520Yorkshire", 
  "nhsregion;areaName=South%2520East", "nhsregion;areaName=South%2520West",
  #countries:
  "nation;areaName=England", "nation;areaName=Wales",
  "nation;areaName=Scotland", "nation;areaName=Northern%2520Ireland")

#available data types:
url_hosp_inc <- "&structure=%7B%22date%22:%22date%22,%22geography%22:%22areaName%22,%22value%22:%22newAdmissions%22%7D&format=csv"
url_hosp_prev <- "&structure=%7B%22date%22:%22date%22,%22geography%22:%22areaName%22,%22value%22:%22hospitalCases%22%7D&format=csv"
url_icu_prev <- "&structure=%7B%22date%22:%22date%22,%22geography%22:%22areaName%22,%22value%22:%22covidOccupiedMVBeds%22%7D&format=csv"


#create empty dataframe for storing
data <- data.frame()


#loop through all regions to extract data
for(i in regions){
  
  #message("Working on ", i, "...")
  
  #create full urls
  url_reg_hospi_inc <- paste0(base_url, i, url_hosp_inc)
  url_reg_hosp_prev <- paste0(base_url, i, url_hosp_prev)
  url_reg_icu_prev <- paste0(base_url, i, url_icu_prev)
  
  #extract datasets
  data_reg_hospi_inc <- rio::import(url_reg_hospi_inc, format = "csv") %>%
    mutate(value_type = "hospital_inc",
           value_desc = "New and newly confirmed patients in hospital")
  
  data_reg_hosp_prev <- rio::import(url_reg_hosp_prev, format = "csv") %>%
    mutate(value_type = "hospital_prev",
           value_desc = "Total beds occupied")
  
  data_reg_icu_prev <- rio::import(url_reg_icu_prev, format = "csv") %>%
    mutate(value_type = "icu_prev",
           value_desc = "ICU beds occupied")
  
  #combine & save datasets
  data <- rbind(data, data_reg_hospi_inc,
                data_reg_hosp_prev, data_reg_icu_prev)
  
}

#some final tweaks
data <- data %>%
  select(date, geography, value_type, value, value_desc) %>%
  mutate(date = as.Date(date))

