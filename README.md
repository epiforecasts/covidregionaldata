# Subnational data for the Covid-19 outbreak

[![badge](https://img.shields.io/badge/Launch-package-lightblue.svg)](https://mybinder.org/v2/gh/epiforecasts/covidregionaldata/master?urlpath=rstudio)
[![R-CMD-check](https://github.com/epiforecasts/covidregionaldata/workflows/R-CMD-check/badge.svg)](https://github.com/epiforecasts/covidregionaldata/actions)
[![Codecov test coverage](https://codecov.io/gh/epiforecasts/covidregionaldata/branch/master/graph/badge.svg)](https://codecov.io/gh/epiforecasts/covidregionaldata?branch=master)
[![develVersion](https://img.shields.io/badge/devel%20version-0.8.2-green.svg?style=flat)](https://github.com/epiforecasts/covidregionaldata/)
[![DOI](https://zenodo.org/badge/271601189.svg)](https://zenodo.org/badge/latestdoi/271601189)
[![metacran downloads](http://cranlogs.r-pkg.org/badges/grand-total/covidregionaldata?color=ff69b4)](https://cran.r-project.org/package=covidregionaldata)


An interface to subnational and national level Covid-19 data. For all countries supported, this includes a daily time-series of cases. Wherever available we also provide  data on deaths, hospitalisations, and tests. National level data is also supported using a range of data sources as well as linelist data and links to intervention data sets.

## Installation

Install from CRAN:

``` r
install.packages("covidregionaldata")
```

Install the stable development version of the package with:

```r
install.packages("drat")
drat:::add("epiforecasts")
install.packages("covidregionaldata")
```

Install the unstable development version of the package with:

``` r
remotes::install_github("epiforecasts/covidregionaldata")
```

## Quick start

[![Documentation](https://img.shields.io/badge/Package-documentation-lightgrey.svg?style=flat)](https://epiforecasts.io/covidregionaldata/)

First load the package using:

```r
library(covidregionaldata)
```

To get worldwide time-series data by country (sourced from the WHO), use `get_national_data()`:
``` r
get_national_data()
```

To get time-series data for subnational regions of a specific country, for example by local authority in the UK, use `get_regional_data()`:

```r
get_regional_data(country = "UK", include_level_2_regions = TRUE)
```
Subnational regions are only available in some countries. See the package documentation for further details.
## Development

We welcome contributions and new contributors! We particularly appreciate help adding new data sources for countries at sub-national level, or work on priority problems in the [issues](https://github.com/epiforecasts/covidregionaldata/issues). Please check and add to the issues, and/or add a [pull request](https://github.com/epiforecasts/covidregionaldata/pulls). For more detail, please read the [System Maintenance Guide](https://github.com/epiforecasts/covidregionaldata/blob/master/inst/smg/SMG.md).
