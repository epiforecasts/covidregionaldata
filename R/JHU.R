#' Download and clean data from JHU
#' @description Downloads daily covid data from JHU 
#' (https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data)
#' and formats the data
#' @return A data frame of COVID cases for all countris listed by JHU, by country and region if avaliable
#' @importFrom dplyr %>% select group_by rename mutate ungroup arrange lag first
#' @importFrom tidyr %>% gather replace_na
#' 
get_JHU_data <- function(){
  # Path to data & targets
  path <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
  vals <-  c(
    "confirmed",
    "deaths",
    "recovered"
  )
  
  # make empty list to store individual dataframes in
  data_list <-  replicate(3, data.frame())
  
  # load individual dataframes
  for (i in 1:length(vals)) {
    # format full data path
    v <-  vals[[i]]
    wpath <-  paste0(path, "time_series_covid19_", v, "_global.csv")
    
    # read data
    data <-  csv_reader(file=wpath)
    
    # convert to long format
    colname <-  paste0("daily_", v)
    data_long <- tidyr::gather(data, key="Date", value=!!colname, 5:ncol(data))
    
    # create row id for joining
    data_long$id <-  paste(data_long$`Province/State`, data_long$`Country/Region`, data_long$Date, sep="_")
    
    # append to list
    data_list[[i]] <-  data_long
  }
  
  # merge dataframes together
  data = Reduce(function(...) merge(..., on="id", all=T), data_list)
  
  # clean data
  data <- data %>% 
    dplyr::select(Date, `Province/State`, `Country/Region`, daily_confirmed, daily_deaths, daily_recovered) %>% 
    dplyr::mutate(Date=lubridate::mdy(Date),
                  daily_confirmed=as.numeric(daily_confirmed),
                  daily_deaths=as.numeric(daily_deaths),
                  daily_recovered=as.numeric(daily_recovered)) %>% 
    tidyr::replace_na(list(`Province/State`="Unknown",
                           `Country/Region`="Unknown")) %>% 
    dplyr::rename(date=Date,
                  region=`Province/State`,
                  country=`Country/Region`,
                  cases_total=daily_confirmed,
                  deaths_total=daily_deaths,
                  recovered_total=daily_recovered
    )
  
  # calculate new cases/deaths/recovered (change from previous day)
  data <- data %>% 
    # sort by date ascending
    dplyr::arrange(date) %>%
    # group by the country and providence
    dplyr::group_by_("country", "region") %>% 
    # subract previous row from target row
    dplyr::mutate(cases_new=cases_total-dplyr::lag(cases_total, default = dplyr::first(cases_total)),
                  deaths_new=deaths_total-dplyr::lag(deaths_total, default = dplyr::first(deaths_total)),
                  recovered_new=recovered_total-dplyr::lag(recovered_total, default = dplyr::first(recovered_total))) %>%
    # arange date descending
    dplyr::arrange(dplyr::desc(date)) %>% 
    # ungroup
    dplyr::ungroup() %>% 
    # reorder columns 
    dplyr::select(date, country, region, cases_new, cases_total, deaths_new, deaths_total, recovered_new, recovered_total)
  
  # return data
  return(data)
}
