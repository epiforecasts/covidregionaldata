# load testing function and tools.
# set up custom tests using:
# custom_tests/regional-dataset-specific.R
#source("custom_tests/test-regional-dataset.R")

# should a single dataset be tested vs all datasets
# set this when implementing a new dataset.
# Can also be set using environment variables
source_of_interest <- NULL
if (!is.null(getOption("testSource"))) {
  source_of_interest <- getOption("testSource")
}

library(switchr)
#switchrBaseDir(file.path(tempdir(), ".switchr"))

# reps = options("repos")[[1]]
# options(width=40)
# if(reps["CRAN"] == "@CRAN@") {
#   reps["CRAN"] = "https://cloud.r-project.org"
#   options(repos = reps)
# }
# 
initialSetup <- FALSE

if(initialSetup) {
  removeLib("oldcovidregionaldata")
  removeLib("newcovidregionaldata")
  crd_new <- GithubManifest("epiforecasts/covidregionaldata@0.9.0")
  crd_old <- GithubManifest("epiforecasts/covidregionaldata@0.8.3")
  #removeLib("oldcovidregionaldata")
  switchTo("oldcovidregionaldata", seed = crd_old)
  ip_list_old <- installed.packages()
  switchBack()
  switchTo("newcovidregionaldata", seed = crd_new)
  ip_list_new <- installed.packages()
  switchBack()
  waldo::compare(ip_list_old, ip_list_new)
} 



switchTo("newcovidregionaldata")

library(covidregionaldata)
library(dplyr)
sources <- get_available_datasets() %>%
  filter(get_data_function %in%
           c("get_regional_data", "get_national_data")) %>%
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

# apply tests to each data source in turn
sources %>%
  dplyr::rowwise() %>%
  dplyr::group_split() %>%
  purrr::map(
    ~ get_regional_data(
      country = .$source[[1]],
      level = .$level[[1]]
    )
  ) -> new_version_output

saveRDS(new_version_output, "newversionoutput.rds")

switchBack()
## now switch to the old version
#

switchTo("oldcovidregionaldata")

library(covidregionaldata)
library(dplyr)

get_regional_data_wrapper <- function(country, level = 1) {
  if (level == 1) {
    get_regional_data(country)
  } else {
    get_regional_data(country, include_level_2_regions = TRUE)
  }
}

# apply tests to each data source in turn
sources %>%
  dplyr::rowwise() %>%
  dplyr::group_split() %>%
  purrr::map(
    ~ get_regional_data_wrapper(
      country = .$source[[1]],
      level = .$level[[1]]
    )
  ) -> new_version_output

saveRDS(new_version_output, "oldversionoutput.rds")

switchBack()

# man = PkgManifest(name = "covidregionaldata",
#                   url = "https://github.com/epiforecasts/covidregionaldata",
#                   type = "git")
# man
#install_packages()