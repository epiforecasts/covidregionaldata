# Utility functions for the Covid-19 outbreak

[![badge](https://img.shields.io/badge/Launch-package-lightblue.svg)](https://mybinder.org/v2/gh/epiforecasts/NCoVUtils/master?urlpath=rstudio)
[![Build Status](https://travis-ci.com/epiforecasts/NCoVUtils.svg?branch=master)](https://travis-ci.com/epiforecasts/NCoVUtils)
  [![Codecov test coverage](https://codecov.io/gh/epiforecasts/NCoVUtils/branch/master/graph/badge.svg)](https://codecov.io/gh/epiforecasts/NCoVUtils?branch=master)
[![develVersion](https://img.shields.io/badge/devel%20version-0.3.0-green.svg?style=flat)](https://github.com/epiforecasts/NCoVUtils)
[![Documentation](https://img.shields.io/badge/Package-documentation-lightgrey.svg?style=flat)](https://epiforecasts.io/NCoVUtils)
[![DOI](https://zenodo.org/badge/238177228.svg)](https://zenodo.org/badge/latestdoi/238177228)


*Note: This package makes extensive use of `memoise` and writes a `.cache` to the directory in which its functions are run. This speeds up data retrieval and avoids hitting rate limits but does not follow CRAN best practice.* **Use with care.** *The cache can be reset with `reset_cache()` when updated data is required from the online source.*

## Installation

Install the package and all dependencies with:

```r
remotes::install_github("epiforecasts/NCoVUtils", dependencies = TRUE)
```

## Usage

### Sub-national data
There are three main functions to extract sub-national level data by country. The underlying data for each function is the same - each function just formats the data differently. The data is stratified by region and (for two of the three formats) by date. The data table includes the ISO-3166-2 code for each region. Each function takes the country name (in English) as a string (see list below for available countries). The country string is not case-sensitive.

The three ways to view the data (and the related functions) are:
1. Long format - this is the standard used by the Covid19R package. To get data in this format use:
```r
NCoVUtils::get_long_format_regional_covid_data("Belgium")
```

This returns a dataset with the following structure

**date**|**location**|**location\_type**|**location\_code**|**location\_code\_type**|**data\_type**|**value**
:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:
2020-05-24|Wallonia|region|BE-WAL|iso-3166-2|tests\_total|NA
2020-05-25|Brussels|region|BE-BRU|iso-3166-2|cases\_new|27
2020-05-25|Brussels|region|BE-BRU|iso-3166-2|cases\_total|5828

2. Wide format (aka time series format). To get data in this format use:
```r
NCoVUtils::get_wide_format_regional_covid_data("Belgium")
```

This returns a dataset with the following structure

**date**|**region**|**iso\_code**|**cases\_new**|**cases\_total**|**deaths\_new**|**deaths\_total**|**recoveries\_new**|**recoveries\_total**|**hospitalisations\_new**|**hospitalisations\_total**|**tests\_new**|**tests\_total**
:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:
2020-05-24|Wallonia|BE-WAL|24|18194|16|3257|NA|NA|8|5126|NA|NA
2020-05-25|Brussels|BE-BRU|27|5828|2|1436|NA|NA|6|2533|NA|NA
2020-05-25|Flanders|BE-VLG|184|32376|14|4673|NA|NA|29|9428|NA|NA

3. Totals only (cumulative data) up to the latest date available in the data (usually today's date or yesterday). To get data in this format use:
```r
NCoVUtils::get_totals_only_regional_covid_data("Belgium")
```

This returns a dataset with the following structure
 
**region**|**iso\_code**|**cases\_total**|**deaths\_total**|**recoveries\_total**|**hospitalisations\_total**|**tests\_total**
:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:
Flanders|BE-VLG|32817|4731|0|9550|0
Wallonia|BE-WAL|18489|3292|0|5187|0
Brussels|BE-BRU|5914|1463|0|2560|0

The data fields for counts returned will _always_ be the same for standardisation reasons, though if the corresponding data was missing from the original source then that data field will be all NA values (or 0 if accessing totals data). Note that some rows may have NA in `*_new` data fields if the data was missing from the source also. 

Dates will always be in YYYY-MM-DD format and the ISO codes are ISO-3166-2 codes. 

Data fields with `*_new` names are new counts for the day/region in question. Data fields with `*_total` names are cumulative counts for that region up to and including the date. 

#### Region Levels
All countries have data for regions at the admin-1 level, the largest regions available (e.g. state in the USA). *Some* countries have data for regions at the admin-2 level (e.g. county in the USA). Requesting data stratified by Level 2 regions instead of Level 1 is done by using the `include_level_2_regions` logical argument in the functions above. Note that `NCoVUtils::get_long_format_regional_covid_data()` does not support this currently. 

The datasets will also have the corresponding level 1 region included along with its ISO-3166-2 code. 

For an example of requesting Level 2 regions:
```r
NCoVUtils::get_wide_format_regional_covid_data("Belgium", include_level_2_regions = TRUE)
```

This returns a dataset with the following structure
**date**|**province**|**region**|**iso\_code**|**cases\_new**|**cases\_total**|**deaths\_new**|**deaths\_total**|**recoveries\_new**|**recoveries\_total**|**hospitalisations\_new**|**hospitalisations\_total**|**tests\_new**|**tests\_total**
:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:
2020-05-24|Antwerpen|Flanders|BE-VLG|16|7905|NA|NA|NA|NA|5|2510|NA|NA
2020-05-24|BrabantWallon|Wallonia|BE-WAL|4|1421|NA|NA|NA|NA|0|224|NA|NA
2020-05-24|Brussels|Brussels|BE-BRU|7|5804|NA|NA|NA|NA|4|2527|NA|NA


Currently we include functions for sub-national data in the following countries (* indicates data for level 2 regions as well):

Europe

  +	Belgium (*)

  + Germany

  +	Italy


Americas

  + Brazil (*)

  +	Canada


Asia

  + Afghanistan

  + India



### Worldwide data

There are two sources of worldwide, country-level data on cases and deaths.

1. Extract total global cases and deaths by country, and specify source, using:
  + ```NCoVUtils::get_total_cases(source = c("WHO", "ECDC"))```
2. Extract daily international case and death counts compiled by the WHO using:
  + ```NCoVUtils::get_who_cases(country = NULL, daily = TRUE))```
3. Extract daily international case and death counts compiled by ECDC using:
  + ```NCoVUtils::get_ecdc_cases()```

A further function for worldwide data extracts non-pharmaceutical interventions by country:

* ```NCoVUtils::get_interventions_data()```

And anonymised international patient linelist data can be imported and cleaned with:

* ```NCoVUtils::get_linelist()```


## Development
Developers who wish to contribute should read the System Maintenance Guide (SMG.md).