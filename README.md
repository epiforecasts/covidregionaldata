# Data extraction tools for the Covid-19 outbreak

[![badge](https://img.shields.io/badge/Launch-package-lightblue.svg)](https://mybinder.org/v2/gh/epiforecasts/covidregionaldata/master?urlpath=rstudio)
![R-CMD-check](https://github.com/epiforecasts/covidregionaldata/workflows/R-CMD-check/badge.svg)
[![Codecov test coverage](https://codecov.io/gh/epiforecasts/covidregionaldata/branch/master/graph/badge.svg)](https://codecov.io/gh/epiforecasts/covidregionaldata?branch=master)
[![develVersion](https://img.shields.io/badge/devel%20version-0.3.0-green.svg?style=flat)](https://github.com/epiforecasts/covidregionaldata)
[![Documentation](https://img.shields.io/badge/Package-documentation-lightgrey.svg?style=flat)](https://epiforecasts.io/covidregionaldata)
[![DOI](https://zenodo.org/badge/238177228.svg)](https://zenodo.org/badge/latestdoi/238177228)

## Installation

Install the stable version of the package using
[`{drat}`](https://epiforecasts.io/drat/):

``` r
install.packages("drat")
drat:::add("epiforecasts")
install.packages("covidregionaldata")
```

Install the development version of the package with:

``` r
remotes::install_github("epiforecasts/covidregionaldata")
```

## Usage

### Sub-national data
The function which returns sub-national level data by country is `covidregionaldata::get_regional_data()`.

This function takes 3 arguments:
* `country` - the English name of the country of interest. Not case sensitive
* `totals` (optional, default is FALSE) - a Boolean (TRUE/FALSE), denoting whether the data returned should be a table of total counts (one row per region) _or_ time series data (one row per region/date combination).
* `include_level_2_regions` (optional, default is FALSE) - a Boolean (TRUE/FALSE), denoting whether the data returned should be stratified by admin level 1 region (usually the largest subregion available) or admin level 2 region (usually the second largest). 

For example:
``` r
covidregionaldata::get_regional_data("Belgium")
```
This returns a dataset with the following structure

**date**|**region**|**iso\_code**|**cases\_new**|**cases\_total**|**deaths\_new**|**deaths\_total**|**recovered\_new**|**recovered\_total**|**hosp\_new**|**hosp\_total**|**tested\_new**|**tested\_total**
:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:
2020-05-24|Wallonia|BE-WAL|24|18196|16|3251|NA|NA|8|5126|NA|NA
2020-05-25|Brussels|BE-BRU|26|5838|2|1421|NA|NA|6|2533|NA|NA
2020-05-25|Flanders|BE-VLG|183|32381|14|4681|NA|NA|29|9334|NA|NA

#### Totals
For totals data, use the `totals` argument.

``` r
covidregionaldata::get_regional_data("Belgium", totals = TRUE)
```
This returns a dataset with the following structure
 
**region**|**iso\_code**|**cases\_total**|**deaths\_total**|**recovered\_total**|**hosp\_total**|**tested\_total**
:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:
Flanders|BE-VLG|34195|4878|0|9694|0
Wallonia|BE-WAL|19093|3362|0|5321|0
Brussels|BE-BRU|6229|1482|0|2657|0

#### Level 2 regions
All countries have data for regions at the admin-1 level, usually the largest regions available (e.g. state in the USA). *Some* countries have data for regions at the admin-2 level (e.g. county in the USA). Requesting data stratified by Level 2 regions instead of Level 1 is done by using the `include_level_2_regions` logical argument as discussed above. The datasets will also have the corresponding level 1 region included along with its corresponding code. 

For an example of requesting Level 2 regions:
```r
covidregionaldata::get_regional_data("Belgium", include_level_2_regions = TRUE)
```
This returns a dataset with the following structure

**date**|**province**|**level\_2\_region\_code**|**region**|**iso\_code**|**cases\_new**|**cases\_total**|**deaths\_new**|**deaths\_total**|**recovered\_new**|**recovered\_total**|**hosp\_new**|**hosp\_total**|**tested\_new**|**tested\_total**
:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:
2020-05-24|Brussels|BE-BRU|Brussels|BE-BRU|7|5812|NA|NA|NA|NA|4|2527|NA|NA
2020-05-24|Antwerpen|BE-VAN|Flanders|BE-VLG|16|7905|NA|NA|NA|NA|5|2510|NA|NA
2020-05-24|Limburg|BE-VLI|Flanders|BE-VLG|14|6126|NA|NA|NA|NA|2|1848|NA|NA


### Data Glossary
The possible data columns that will be returned by `get_regional_data()` are listed below. 
Note that Date is not included if `totals` is FALSE, and level 2 region/level 2 region code are not included if `include_level_2_regions` is FALSE.

The columns returned for each country will _always_ be the same for standardisation reasons, though if the corresponding data was missing from the original source then that data field will be all NA values (or 0 if accessing totals data). Some rows may also be all NA in `*_new` data cells if the data for that date was missing from the source also. 

* `date`: the date that the counts were reported (YYYY-MM-DD).
* `level 1 region`: The level 1 region. This column will be named differently for different countries (e.g. state, province).
* `level 1 region code`: A standard code for the level 1 region. The column will be named differently for different countries (e.g. iso_3166_2, ons).
* `level 2 region`: The level 2 region. This column will be named differently for different countries (e.g. city, county).
* `level 2 region code`: A standard code for the level 2 region. The column will be named differently for different countries (e.g. iso_3166_2, fips).
* `cases_new`: new reported cases for that day
* `cases_total`: total reported cases up to and including that day
* `deaths_new`: new reported deaths for that day
* `deaths_total`: total reported deaths up to and including that day
* `recovered_new`: new reported recoveries for that day
* `recovered_total`: total reported recoveries up to and including that day
* `hosp_new`: new reported hospitalisations for that day
* `hosp_total`: total reported hospitalisations up to and including that day (note this is cumulative total of new reported, _not_ total currently in hospital)
* `tested_new`: tests for that day
* `tested_total`: total tests completed up to and including that day

### Coverage
Currently we include functions for sub-national data in the following countries (* indicates data for level 2 regions as well):

Europe

  +	Belgium (*)

  + Germany (*)

  +	Italy

  + Russia

  + UK (*)


Americas

  + Brazil (*)

  +	Canada

  + Colombia

  + USA (*)


Asia

  + Afghanistan

  + India


## Worldwide data
Worldwide data is also included in the package to aid analysis. There are three sources of worldwide, country-level data on cases and deaths.

1. Extract total global cases and deaths by country, and specify source, using:
  + ```covidregionaldata::get_total_cases(source = c("WHO", "ECDC"))```
2. Extract daily international case and death counts compiled by the WHO using:
  + ```covidregionaldata::get_who_cases(country = NULL, daily = TRUE))```
3. Extract daily international case and death counts compiled by ECDC using:
  + ```covidregionaldata::get_ecdc_cases()```

A further function for worldwide data extracts non-pharmaceutical interventions by country:

* ```covidregionaldata::get_interventions_data()```

And anonymised international patient linelist data can be imported and cleaned with:

* ```covidregionaldata::get_linelist()```


## Development
Developers who wish to contribute should read the System Maintenance Guide (SMG.md).
