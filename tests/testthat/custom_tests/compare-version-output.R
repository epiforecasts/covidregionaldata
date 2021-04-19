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
if (!is.null(getOption("testSource"))) {
  source_of_interest <- getOption("testSource")
}

# initialSetup
# should the switchr framework be built from scratch or
# can it be assumed to be in place

initialSetup <- FALSE
if (!is.null(getOption("initialSetup"))) {
  initialSetup <- getOption("initialSetup")
}

library(switchr)
#switchrBaseDir(file.path(tempdir(), ".switchr"))

if(initialSetup) {
  removeLib("oldcovidregionaldata")
  removeLib("newcovidregionaldata")
  
  # switchr cannot correctly pull releases from github, so 
  # we have stashed a version of the 0.8.3 release in a branch
  # on a separate account
  #
  crd_new <- GithubManifest("epiforecasts/covidregionaldata@master")
  crd_old <- GithubManifest("richardmn/covidregionaldata@old-0_8_3")
  
  switchTo("oldcovidregionaldata", seed = crd_old)
  ip_list_old <- installed.packages()
  switchBack()
  switchTo("newcovidregionaldata", seed = crd_new)
  ip_list_new <- installed.packages()
  switchBack()
  waldo::compare(ip_list_old, ip_list_new)
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
  # addin 
  filter(source != "SouthAfrica") %>%
  mutate(label = paste0(source, "_", level)) %>%
  select(label, source, level, regions) %>%
  # end addin
  #dplyr::rowwise() %>%
  group_by(label) %>%
  #tidyr::nest() %>%
  dplyr::group_split()

names(dl_list) <- pull(sources %>%
                         filter(source != "SouthAfrica") %>%
                         mutate(label = paste0(source, "_", level)) %>%
                         select(label))

# Now call get_regional_data on each item of the list and store it in a
# new list
dl_list %>% purrr::map(
    ~ get_regional_data(
      country = .$source[[1]],
      level = .$level[[1]]
    )
  ) -> new_version_output

saveRDS(new_version_output, "newversionoutput.rds")

switchBack()

## Now switch to the old version
#

switchTo("oldcovidregionaldata")

library(covidregionaldata)
library(dplyr)

start_using_memoise()

# Wrapper to the old version of get_regional_data so that it can
# be applied to the same format of list as the new version
get_regional_data_wrapper <- function(country, level = 1) {
  if (level == 1) {
    get_regional_data(country)
  } else {
    get_regional_data(country, include_level_2_regions = TRUE)
  }
}

# Now call get_regional_data_wrapper on each item of the list and store it
# in a new list
dl_list %>%
  purrr::map(
    ~ get_regional_data_wrapper(
      country = .$source[[1]],
      level = .$level[[1]]
    )
  ) -> old_version_output

saveRDS(old_version_output, "oldversionoutput.rds")

switchBack()

# Use waldo to compare the two lists
#waldo::compare(old_version_output,new_version_output)
purrr::map2(old_version_output, new_version_output, waldo::compare)
