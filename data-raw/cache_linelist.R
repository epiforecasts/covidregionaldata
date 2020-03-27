
linelist <- NCoVUtils::get_international_linelist(clean = FALSE)


readr::write_csv(linelist, "data-raw/linelist.csv")

