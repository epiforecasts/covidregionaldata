#
# Sample code to download Germany line list data from RKI by paging
# through data offered by their RESTful api
#
# This currently does not parse dates correctly.
#

library("httr")
library("jsonlite")
library("dplyr")
library("lubridate")

# https://services7.arcgis.com/mOBPykOjAyBO2ZKk/arcgis/rest/services/RKI_COVID19/FeatureServer/0/query?where=1%3D1&outFields=Bundesland,Landkreis,AnzahlFall,AnzahlTodesfall,Meldedatum&outSR=4326&f=json

rki_call <- "https://services7.arcgis.com/mOBPykOjAyBO2ZKk/arcgis/rest/services/RKI_COVID19/FeatureServer/0/query?where=1%3D1&outFields=Bundesland,Landkreis,AnzahlFall,AnzahlTodesfall,Meldedatum&outSR=4326&f=json"

get_de_data <- GET(rki_call)

get_de_content <- content(get_de_data, "text")

get_de_json <- fromJSON(get_de_content, flatten = TRUE)

get_de_df <- as.data.frame(get_de_json$features)

result_offset <- dim(get_de_df)
result_offset <- result_offset[1]

get_de_df <- get_de_df %>%
  transmute(
    Bundesland = attributes.Bundesland,
    Landkreis = attributes.Landkreis,
    Anzahlfahl = attributes.AnzahlFall,
    AnzahlTodesfall = attributes.AnzahlTodesfall,
    Meldedatum = as_date(attributes.Meldedatum)
  )
page <- 1
done_download <- FALSE
supp_de_data <- get_de_data 

rm("get_de_data","get_de_content", "get_de_json")

while(!done_download && supp_de_data$status_code == 200) {
  offset <- result_offset * page
  page <- page + 1
  supp_rki_call <- paste(rki_call,"&resultOffset=", sprintf("%d",offset), sep="" )
  supp_de_data <- GET(supp_rki_call)
  message(paste("downloading page", page, "offset is", offset, "supp_de_data$status_code is", supp_de_data$status_code))
  supp_de_content <- content(supp_de_data, "text")
  supp_de_json <- fromJSON(supp_de_content, flatten = TRUE)
  supp_de_df <- as.data.frame(supp_de_json$features) %>%
    transmute(
      Bundesland = attributes.Bundesland,
      Landkreis = attributes.Landkreis,
      Anzahlfahl = attributes.AnzahlFall,
      AnzahlTodesfall = attributes.AnzahlTodesfall,
      Meldedatum = as_date(attributes.Meldedatum)
    )
  row_count <- dim(supp_de_json$features)
  row_count <- row_count[1]
  if (row_count < result_offset) {
    message (paste("row_count is", row_count, " - download possibly complete"))
    done_download <- TRUE
  }
  if ( supp_de_data$status_code == 200 ) {
    get_de_df <- rbind(get_de_df, supp_de_df)
  } else {
    message("done downloading")
    break
  }
  
}
