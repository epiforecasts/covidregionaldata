#'  Get regional-level data
#'
#' @description Provides an interface to source specific classes which
#' support regional level data, and where these do not exist, seeks regional
#' level data from JHU or google. For simple use cases this allows downloading
#' clean, standardised, regional-level COVID-19 data sets. Internally this uses
#' the `DataClass()` parent class which allows documented downloading, cleaning,
#' and processing. Optionally all steps of data processing can be returned
#' along with the functions used for processing but by default just the
#' finalised processed data is returned. See the examples for some potential
#' use cases and the links to lower level functions for more details and
#' options.
#'
#' @param country A character string specifying the country to get data from.
#' Not case dependent. Name should be the English name. For a list of
#' options use `get_available_datasets()`.
#' @inheritParams return_data
#' @inheritParams initialise_dataclass
#' @return A tibble with data related to cases, deaths, hospitalisations,
#' recoveries and testing stratified by regions within the given country.
#' @family interface
#' @seealso [Italy()], [UK()]
#' @export
#' @examples
#' \dontrun{
#' # set up a data cache
#' start_using_memoise()
#'
#' # download data for Italy
#' try_regional_data("italy")
#'
#' # return totals for Italy with no localisation
#' try_regional_data("italy", localise = FALSE, totals = TRUE)
#'
#' # download data for the UK but return the class
#' uk <- try_regional_data("United Kingdom", class = TRUE)
#' uk
#'
#' # return UK data from the class object]
#' uk$return()
#' }
get_regional_data <- function(country, level = "1", totals = FALSE,
                              localise = TRUE, steps = FALSE,
                              class = FALSE, verbose = TRUE, regions,
                              use_fallbacks = TRUE,
                            ...) {

  # construct short hand options
  title_country <- str_to_title(country)
  nospace <- str_replace_all(title_country, " ", "")
  targets <- c(
    title_country, toupper(title_country), nospace, toupper(nospace)
  )

  # check we have data for desired class
  datasets <- covidregionaldata::get_available_datasets("regional")
  target_class <- bind_rows(
    filter(datasets, map_lgl(.data$class, ~ any(str_detect(., targets)))),
    filter(datasets, map_lgl(.data$origin, ~ any(str_detect(., targets))))
  ) %>%
    distinct()

  if (nrow(target_class) != 0) {
    # check data availability and initiate country class if available
    region_class <- initialise_dataclass(
      class = country, level = level, regions = regions,
      totals = totals, localise = localise,
      verbose = verbose, steps = steps, get = TRUE,
      type = "regional", ...
    )

    return(
      return_data(region_class, class = class)
    )
  } else if (use_fallbacks) {
    google_sources <- csv_reader(Google$public_fields$common_data_urls$index)

    google_inputs <-
      filter(google_sources, map_lgl(.data$country_name, ~ any(str_detect(., targets))))
    if (nrow(google_inputs) != 0) {
      message_verbose(verbose = verbose,
                      "No country-specific data class found. Getting data from Google for ",
                      country)
      google_class <- initialise_dataclass(
        class = "Google", level = sprintf("%d", as.integer(level)+1),
        totals = totals, localise = TRUE,
        verbose = verbose, steps = steps,
        regions = country, get = TRUE,
        type = "national", ...
      )
      return_data(google_class, class = class)
    } else {
      if (use_fallbacks) {
        fallback_text <- ", including from Google."
      } else {
        fallback_text <- ", searching only in supported datasets."
      }
      stop(
        "No data available for ", country, fallback_text, "\n",
        "See get_available_datasets(type = c(\"regional\")) for supported datasets."
      )
    }
  }
}
