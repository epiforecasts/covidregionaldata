# covidregionaldata 0.9.0

In this release `covidregionaldata` has been substantially retooled to be more robust, and to handle data in a more transparent way. Adding new data sets and functionality has also been made more streamlined. As this update is a substantial package refactor some breaking changes may be been inadvertently introduced. If requiring the old behaviour please install `covidregionaldata@0.8.3` from GitHub.

Thanks to @joseph-palmer, @RichardMN, and @kathsherratt for major contributions to this release. Thanks to @RichardMN for volunteering his time.

## New features

* Track data processing from raw to clean using the `step = TRUE` argument in `get_regional_data()`.
* Filter datasets for regions and countries of interest. 
* Access the underlying methods for data sets and all steps in the data processing pipeline.

## Documentation

* All vignettes have been updated for readability.
* A quickstart has been added to the package README.

## Technical improvements

* `get_regional_data()` and `get_national_data()` now use R6 method dispatch. This is an internal change and so should have minimal user impact for users of the `get_` functions. However, all datasets are now available to be used as R6 methods (see `get_available_datasets`) which may allow for more modular use cases. These classes can also be initialised using `initialise_dataclass()` which is used internally by both `get_regional_data()` and `get_national_data()`.
* Unit testing has been separated from data downloading which is now tested individually by data set. This allows for contributors to more easily assess the impact of their additions and also allows us to publish data status updates for each data sets (see the README: https://github.com/epiforecasts/covidregionaldata#readme).

## Deprecated functions

* `get_available_datasets()` replaces `get_info_covidregionaldata()` to view available data. `get_info_covidregionaldata()` is deprecated.
* `get_interventions_data()` is deprecated. These data no longer update as of December 2020. Check for alternatives at https://supertracker.spi.ox.ac.uk/policy-trackers/
* `get_linelist` is deprecated. Linelist stopped updating June 2020. Up to date linelist data are now behind a login: access at https://global.health/. We are working on a solution for accessing with `covidregionaldata`.

## Data changes since 0.8.3

* Colombia now has capitalized region names.
* Germany level 2 region codes have been removed (previously was all NAs).
* India uses NA for unknown region codes, a change from IN-UN previously.
* Italy column region is now regioni.
* Mexico codes 'inegi_code' has been changed to 'inegi'.
* UK Level 1 'ons_region_code' is now 'region_code'.
* UK level 2 "ltla_code" is now "local_authority_code".
* `get_available_datasets()` now return an origin column rather than a country column and a type column rather than a get_data_function to better reflect the types of data supported.


# covidregionaldata 0.8.3

## New data sets

* Level 1 admin data for Cuba
* Level 1 admin data for South Africa

## Data set changes

* UK data - added option to get either lower tier or upper tier local authorities (level 2 regions).
* Updated Northern Ireland case data to be by specimen date by default rather than by date of report as was previously the case. This means that in the UK all data except for data streams from Wales and Scotland are by date of specimen.
* Switched to the WHO source as our default for national level data.

## New features

* Relevant up to date package information can be fetched using `get_info_covidregionaldata()`.
* Switched to using `vroom` for faster `csv` downloads.

## Other changes

* Replaced silently broken functions for converting cumulative data to daily and vice versa.
* Removed integration with Covid19R formatting.
* UK data - removed hospital admissions by age, and occupied mechanical ventilation beds. Currently, these don't fit into the existing data structure and are not available at lower level regions.
* Removed code that required ECDC variables to work.
* Update the ECDC source to pull data from the new weekly snapshots. Updated the variables. In a later update the downloading the now archived daily data will be made possible.

# covidregionaldata 0.8.2

* Updates the API backend used to extract UK data to V2. Adds a release date variable which can be used to return data releases from specified dates rather than the latest snapshot.
* Various fixes to maintain compatibility with data set sources.
* Adds a quickstart vignette with examples of exploratory data analysis.

# covidregionaldata 0.7.0

## Breaking changes

* `get_linelist`: argument `clean` changed to `clean_dates` to reflect slight change in use case.

## Changes

* Added new option to return UK data by NHS region. This will also return "first admissions" hospital data (excludes readmissions). Specify 'nhsregions = TRUE'. Default is FALSE, returning ONS regions as before.
* Fixed inconsistent reference dates for variables in UK data. cases_new and cases_total now by "Specimen date" (date of test), while deaths_new and deaths_total are by "Date of death", for all regions and nations.

* Additional delays added to `get_linelist` when `clean_dates = TRUE`.

# covidregionaldata 0.6.0

* Added whitespace trimming to all regional data functions.
* Fixed region codes for Colombia.
* Fixed region name cleaning for afghanistan.
* Updated UK data source and expanded available variables based on the newly implemented API.
* Enabled regional localisation to be optional.
* Minor quality of life changes.

# covidregionaldata 0.5.0

* Release candidate.

# covidregionaldata 0.4.0

* Added functions to extract regional cases for Spain, Japan, South Korea, the United Kingdom, and the United States.
* Improved data extraction from the ECDC.
* Added minimal unit tests.

# covidregionaldata 0.3.0

* Added a function to extract case counts by region for Italy.
* Function to extract ECDC cases.
* Added a function to extract case counts by region in Germany.
* Fixed cache reset.
* Added a `covidregionaldata` cache of the public linelist as a fall back option if the source is not available.

# covidregionaldata 0.2.0

* Added `memoise` functionality to automatically cache in the current directory all remote resources.
* Added a non-Hubei linelist function that can extract country and city linelists.
* Added a function to extract the latest WHO case counts.
* Added a function to summarise the current total number of cases.

# covidregionaldata 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Added `get_linelist` function
* Added functions to reparameterise Weibull and Gamma distributions with mean and standard deviations.
