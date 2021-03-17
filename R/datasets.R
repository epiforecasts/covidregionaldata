#' Get meta information about available datasets.
#'
#' @description Lists available datasets including: function names,
#' data returned, subregion level / raw data sources, alongside
#' package information. It can also be used to help guide adding
#' data sources to the package.
#' @return A tibble of metadata about covidregionaldata.
"available_datasets"

#' Region Codes for Each Dataset.
#'
#' @description Details of the region codes used by each dataset.
#' @return A nested tibble of region codes and related information.
"region_codes"
