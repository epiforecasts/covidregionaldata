#' Indian Regional Daily COVID-19 Count Data - State
#'
#' @description Extracts daily COVID-19 data for India, stratified by State
#' Data available at  \url{https://api.covid19india.org/csv/latest/state_wise_daily.csv}. 
#' It is loaded and then sanitised.
#' @return A dataframe of daily India data to be further processed by [get_regional_data()].
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate select filter rename left_join %>% full_join mutate
#' @importFrom lubridate dmy
#'
get_india_regional_cases <- function() {

  # Set up state names ---------------------------------------------------------------
  state_names <- tibble::tibble(
    code = c("AN", 	"AP", 	"AR", 	"AS", 	"BR", 	"CH", 	"CT", 	"DN", 	"DD",
             "DL", 	"GA", 	"GJ", 	"HR", 	"HP", 	"JK", 	"JH", 	"KA", 	"KL",
             "LA", 	"LD", 	"MP", 	"MH", 	"MN", 	"ML", 	"MZ", 	"NL", 	"OR",
             "PY", 	"PB", 	"RJ", 	"SK", 	"TN", 	"TG", 	"TR",   "UN",	  "UP", 	"UT", 	"WB"),
    region_level_1 = c("Andaman and Nicobar", 	"Andhra Pradesh", 	"Arunachal Pradesh", 	"Assam", 	"Bihar",
               "Chandigarh", 	"Chhattisgarh", 	"Dadra and Nagar Haveli", 	"Daman and Diu",
               "NCT of Delhi", 	"Goa", 	"Gujarat", 	"Haryana", 	"Himachal Pradesh", 	"Jammu and Kashmir",
               "Jharkhand", 	"Karnataka", 	"Kerala", 	"Ladakh", 	"Lakshadweep", 	"Madhya Pradesh",
               "Maharashtra", 	"Manipur", 	"Meghalaya", 	"Mizoram", 	"Nagaland", 	"Odisha",
               "Puducherry", 	"Punjab", 	"Rajasthan", 	"Sikkim", 	"Tamil Nadu", 	"Telangana",
               "Tripura",  "Unknown",  "Uttar Pradesh", 	"Uttarakhand", 	"West Bengal"))

  #  Read in data --------------------------------------------------------------------
  url <- "https://api.covid19india.org/csv/latest/state_wise_daily.csv"
  data <- csv_reader(file = url)

  india_cases <- data %>%
    dplyr::filter(Status == "Confirmed") %>%
    dplyr::select(-Status, -TT) %>%
    tidyr::pivot_longer(-Date, names_to = "state", values_to = "cases_new")

  india_deaths <- data %>%
    dplyr::filter(Status == "Deceased") %>%
    dplyr::select(-Status, -TT) %>%
    tidyr::pivot_longer(-Date, names_to = "state", values_to = "deaths_new")

  india_recoveries <- data %>%
    dplyr::filter(Status == "Recovered") %>%
    dplyr::select(-Status, -TT) %>%
    tidyr::pivot_longer(-Date, names_to = "state", values_to = "recovered_new")

  # Join datasets ---------------------------------------------------------------------
  cases_and_death_data <- dplyr::full_join(india_cases, india_deaths, by = c("Date" = "Date", "state" = "state"))
  data <- dplyr::full_join(cases_and_death_data, india_recoveries, by = c("Date" = "Date", "state" = "state"))

  data <- data %>%
    dplyr::mutate(Date = lubridate::dmy(Date)) %>%
    dplyr::rename(date = Date) %>%
    dplyr::left_join(state_names, by = c("state" = "code")) %>%
    dplyr::select(-state)

  return(data)
}
