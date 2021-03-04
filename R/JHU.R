#' Filter JHU data for target country
#' @description Downloads daily covid data from JHU (via get_JHU_data)
#' and subsets it for a given target country
#' @param target_country A string with the country of interest
#' @return A data frame of COVID cases for the target country
#' @importFrom dplyr %>% filter select
#' @importFrom rlang .data
#' 
check_alternate_data_source <- function(target_country){
  warning(paste0("'", target_country, "' has no direct data source: using JHU for data"))
  # make first letter uppercase so it can find resource
  target_country_ <- paste0(toupper(substr(target_country, 1, 1)),
                          substr(target_country, 2, nchar(target_country)))
  
  # get the full data set
  data <- get_JHU_data()
  
  # check country is in the data
  if (!(target_country_ %in% unique(data$country))){
    stop(paste0("Country '", target_country, "' has no data."))
  }
  
  # filter data for requested country
  data <- data %>% dplyr::filter(.data$country==target_country_)
  
  # remove country column
  data <- data %>% dplyr::select(-c(.data$country))
  
  return(data)
}

#' Download and clean data from JHU
#' @description Downloads daily covid data from JHU 
#' (https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data)
#' and formats the data
#' @return A data frame of COVID cases for all countris listed by JHU, by country and region if avaliable
#' @importFrom dplyr %>% select group_by rename mutate ungroup arrange lag first
#' @importFrom tidyr %>% gather replace_na
#' @importFrom rlang .data
#' 
get_JHU_data <- function(){
  # bind variables to local function
  
  
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
    dplyr::select(.data$Date, .data$`Province/State`, .data$`Country/Region`, .data$daily_confirmed, .data$daily_deaths, .data$daily_recovered) %>% 
    dplyr::mutate(Date=lubridate::mdy(.data$Date),
                  daily_confirmed=as.numeric(.data$daily_confirmed),
                  daily_deaths=as.numeric(.data$daily_deaths),
                  daily_recovered=as.numeric(.data$daily_recovered)) %>% 
    dplyr::rename(date=.data$Date,
                  region_level_1=.data$`Province/State`,
                  country=.data$`Country/Region`,
                  cases_total=.data$daily_confirmed,
                  deaths_total=.data$daily_deaths,
                  recovered_total=.data$daily_recovered) %>% 
    tidyr::replace_na(list(region_level_1="Unknown",
                           country="Unknown"))
  
  # calculate new cases/deaths/recovered (change from previous day)
  data <- data %>% 
    # sort by date ascending
    dplyr::arrange(.data$date) %>%
    # group by the country and providence
    dplyr::group_by(.data$country, .data$region_level_1) %>% 
    # subract previous row from target row
    dplyr::mutate(cases_new=.data$cases_total-dplyr::lag(.data$cases_total, default = dplyr::first(.data$cases_total)),
                  deaths_new=.data$deaths_total-dplyr::lag(.data$deaths_total, default = dplyr::first(.data$deaths_total)),
                  recovered_new=.data$recovered_total-dplyr::lag(.data$recovered_total, default = dplyr::first(.data$recovered_total))) %>%
    # arange date descending
    dplyr::arrange(dplyr::desc(.data$date)) %>% 
    # ungroup
    dplyr::ungroup() %>% 
    # reorder columns 
    dplyr::select(.data$date,
                  .data$country,
                  .data$region_level_1,
                  .data$cases_new,
                  .data$cases_total,
                  .data$deaths_new,
                  .data$deaths_total,
                  .data$recovered_new,
                  .data$recovered_total)
  
  # insert place holder level_1_region_code
  data$level_1_region_code <- "Unknown"
  
  # return data
  return(data)
}
