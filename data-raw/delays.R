# Analysis of delay from public linelist

# library(data.table)
# 
# linelist <- data.table::fread("C:/Users/kaths/Downloads/latestdata.csv")
# 
# clean_list <- linelist[, .(country, country_new, 
#                            date_onset_symptoms, date_confirmation, 
#                            date_admission_hospital,
#                            date_death_or_discharge, 
#                            outcome)]
# dated_list <- linelist[, .(country, country_new,
#                            date_onset = lubridate::dmy(date_onset_symptoms),
#                            date_report = lubridate::dmy(date_confirmation))]
# 
# complete_list <- dated_list[!is.na(date_onset)]
# 
# have_onset <- data.table::copy(complete_list)[, .N, by = country][, `:=`(p = N/sum(N) * 100)]

