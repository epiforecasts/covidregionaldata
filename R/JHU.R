#' R6 Class containing specific attributes and methods for John Hopkins
#' University data
#'
#' @description Country specific information for downloading, cleaning
#'  and processing covid-19 region data for John Hopkins University.
#'
#' @details Inherits from `DataClass`
#' @examples
#' \dontrun{
#' region <- JHU$new(verbose = TRUE, steps = TRUE)
#' region$download()
#' region$clean()
#' region$process()
#' region$return()
#' }
JHU <- R6::R6Class("JHU", # rename to country name
  inherit = DataClass,
  public = list(

    # Core Attributes (amend each paramater for country specific infomation)
    #' @field level_1_region the level 1 region name.
    level_1_region = "country", # add more levels as needed
    #' @field data_url List of named links to raw data. The first, and
    #' sometimes only entry, should be named main
    # nolint start
    data_url = list(main = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"),
    # nolint end
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("confirmed", "deaths", "recovered"),

    #' @description Download JHU data
    #' @importFrom purrr map
    #'
    download = function() {
      paths <- paste0(
        self$data_url$main,
        "time_series_covid19_",
        self$source_data_cols,
        "_global.csv"
      )
      data_list <- map(paths, csv_reader, verbose = self$verbose)
      names(data_list) <- paste0("daily_", self$source_data_cols)
      self$data$raw <- data_list
    },

    #' @description JHU specific state level data cleaning
    #' @importFrom dplyr last_col bind_rows mutate rename select
    #' @importFrom tidyr pivot_longer pivot_wider replace_na
    clean = function() {
      message_verbose(self$verbose, "Cleaning data")
      self$data$clean <- lapply(self$data$raw,
        pivot_longer,
        cols = 5:last_col(),
        names_to = "Date",
        values_to = "value"
      )
      self$data$clean <- self$data$clean %>%
        bind_rows(.id = "variable") %>%
        pivot_wider(names_from = variable, values_from = value) %>%
        select(
          .data$Date,
          .data$`Province/State`,
          .data$`Country/Region`,
          .data$daily_confirmed,
          .data$daily_deaths,
          .data$daily_recovered
        ) %>%
        mutate(
          Date = lubridate::mdy(.data$Date),
          daily_confirmed = as.numeric(.data$daily_confirmed),
          daily_deaths = as.numeric(.data$daily_deaths),
          daily_recovered = as.numeric(.data$daily_recovered)
        ) %>%
        rename(
          date = .data$Date,
          level_1_region = .data$`Province/State`,
          country = .data$`Country/Region`,
          cases_total = .data$daily_confirmed,
          deaths_total = .data$daily_deaths,
          recovered_total = .data$daily_recovered
        ) %>%
        replace_na(
          list(
            level_1_region = "Unknown",
            country = "Unknown"
          )
        )
    }
  )
)
