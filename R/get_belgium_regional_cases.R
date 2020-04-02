#' Fetch daily COVID cases by region for Belgium.
#' @description Fetches daily COVID data from Sciensano, the Belgian Institute for Health.
#' Data is available at https://epistat.wiv-isp.be/covid/
#' @param dataset Character String specifying dataset "cases_municipal", "cases_provincial", "hospitalization_provincial", "mortality_provincial", "testing_national". Default: "cases_provincial". 
#' @return A data.frame of specified Covid data. 
#' @importFrom readr read_csv locale
#' @importFrom dplyr rename_all 
#' @examples
#' 
#' \dontrun{
#' 
#'  get_belgium_regional_cases(dataset = "testing_national")
#'
#' }

get_belgium_regional_cases <- function(dataset = "cases_provincial"){
  
  if(!dataset %in% c("cases_municipal", "cases_provincial", "hospitalization_provincial", "mortality_provincial", "testing_national")){
    stop('Unknown input. Please specify dataset: "cases_municipal", "cases_provincial", "hospitalization_provincial", "mortality_provincial", "testing_national". Default: "cases_provincial".')
  }
  
  c_provincial <- "https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv"
  c_municipal <- "https://epistat.sciensano.be/Data/COVID19BE_CASES_MUNI.csv"
  h_provincial <- "https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv"
  m_provincial <- "https://epistat.sciensano.be/Data/COVID19BE_MORT.csv"
  t_national <- "https://epistat.sciensano.be/Data/COVID19BE_tests.csv"
  
  ch <- memoise::cache_filesystem(".cache")
  
  mem_read <- memoise::memoise(readr::read_csv, cache = ch)
  
  clean_belgium_data_default <- function(url){
    
    data <- mem_read(url, locale = readr::locale(encoding = "latin1"))
    
    return(data %>% dplyr::rename_all(tolower))
    
  }
  
  clean_t_national_data <- function(url){
    
    data = clean_belgium_data_default(url)
    
    data$date = as.Date(data$date, format = '%d/%m/%Y')
    
    return(data)
    
  }
  
  #c_municipal data has somewhat messy raw columns
  clean_c_municipal_data <- function(url){
    
    data <- mem_read(url, locale = readr::locale(encoding = "latin1"))
    
    colnames(data) <- unlist(lapply(colnames(data), gsub, pattern = "TX_", replacement = ""))
    
    return(data %>% dplyr::rename_all(tolower))
    
  }
  
  if (dataset == "cases_provincial"){
    
    return(clean_belgium_data_default(c_provincial))
    
  }else if (dataset == "cases_municipal"){
    
    return(clean_c_municipal_data(c_municipal))
    
  }else if (dataset == "hospitalization_provincial"){
    
    return(clean_belgium_data_default(h_provincial))
    
  }else if (dataset == "mortality_provincial"){
    
    return(clean_belgium_data_default(m_provincial))
    
  }else if (dataset == "testing_national"){
   
    return(clean_t_national_data(t_national))
     
  }
  
}





