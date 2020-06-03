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

There is one main function to extract sub-national level data by country. These are typically at the admin-1 level, the largest regions available. The data table also includes the ISO-3166-2 code for the region. 

To access the sub-national data, use
```NCoVUtils::get_regional_covid_data(<country_name>)```
where country name is a string with the English name of the country (see list below). This is not case-sensitive.

This returns a dataset with the following structure

**date**|**region**|**iso\_code**|**cases\_new**|**cases\_total**|**deaths\_new**|**deaths\_total**|**recoveries\_new**|**recoveries\_total**|**hospitalisations\_new**|**hospitalisations\_total**|**tests\_new**|**tests\_total**
:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:
2020-05-24|Wallonia|BE-WAL|24|18194|16|3257|NA|NA|8|5126|NA|NA
2020-05-25|Brussels|BE-BRU|27|5828|2|1436|NA|NA|6|2533|NA|NA
2020-05-25|Flanders|BE-VLG|184|32376|14|4673|NA|NA|29|9428|NA|NA

The columns returned will _always_ be the same for standardisation reasons, though if the corresponding data was missing from the original source then the column will be all NA values. Note that some rows may have NA in `*_new` columns if the data was missing from the source also. 

Dates will always be in YYYY-MM-DD format; the region is typically at admin-1 level (largest subregions) and the ISO codes are ISO-3166-2 codes. 

Columns with `*_new` names are new counts for the day/region in question. Columns with `*_total` names are cumulative counts for that region up to and including the date. 

Currently we include functions for sub-national data in the following countries:

Europe

  +	Belgium

  + Germany

  +	Italy


Americas

  + Brazil

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