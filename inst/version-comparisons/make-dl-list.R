#
# custom code to load and compare output from two different
# versions of covidregionaldata using switchr
#
# WARNINGS:
#  * This code takes a long time (on the order of a couple of hours)
#    and generates large output files (1.2gb total).
#  * This code uses switchr which may not interact well with other
#    elements of your work environment. You may want to run this
#    outside of RStudio

# OPTIONS
# testSource:
# should a single dataset be tested vs all datasets
# set this when implementing a new dataset.

source_of_interest <- NULL
if (!is.null(getOption("source_of_interest"))) {
  source_of_interest <- getOption("source_of_interest")
}

# initial_setup:
# should the switchr framework be built from scratch or
# can it be assumed to be in place

initial_setup <- FALSE
if (!is.null(getOption("initial_setup"))) {
  initial_setup <- getOption("initial_setup")
}

# save_data_files:
# should the data files be saved

save_data_files <- TRUE
if (!is.null(getOption("save_data_files"))) {
  save_data_files <- getOption("save_data_files")
}

# save_comparison:
# should the comparison data set be saved

save_comparison <- TRUE
if (!is.null(getOption("save_comparison"))) {
  save_comparison <- getOption("save_comparison")
}

library(switchr)

if(initial_setup) {
  source("initialise-switchr.R")
}

## Working from new version of covidregionaldata
#

switchTo("newcovidregionaldata")

library(covidregionaldata)
library(dplyr)

start_using_memoise()

# Build a list of data sources available, based on the newer version
sources <- get_available_datasets() %>%
  # temporary block - just regional data
  filter(get_data_function %in%
           c("get_regional_data")) %>% # , "get_national_data"
  dplyr::select(source = class, level_1_region, level_2_region) %>%
  tidyr::pivot_longer(
    cols = -source,
    names_to = "level",
    values_to = "regions"
  ) %>%
  filter(source != "SouthAfrica") %>%
  dplyr::mutate(
    level = stringr::str_split(level, "_"),
    level = purrr::map_chr(level, ~ .[2])
  ) %>%
  tidyr::drop_na(regions)


# filter out target datasets
if (!is.null(source_of_interest)) {
  sources <- sources %>%
    dplyr::filter(source %in% source_of_interest)
}

dl_list <- sources %>%
  mutate(label = paste0(source, "_", level)) %>%
  select(label, source, level, regions) %>%
  group_by(label) %>%
  dplyr::group_split()

names(dl_list) <- pull(sources %>%
                         #filter(source != "SouthAfrica") %>%
                         mutate(label = paste0(source, "_", level)) %>%
                         select(label))

saveRDS(dl_list, "dl_list.rds")

switchBack()
