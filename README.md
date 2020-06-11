# Data extraction tools for the Covid-19 outbreak

[![badge](https://img.shields.io/badge/Launch-package-lightblue.svg)](https://mybinder.org/v2/gh/epiforecasts/covidregionaldata/master?urlpath=rstudio)
![R-CMD-check](https://github.com/epiforecasts/covidregionaldata/workflows/R-CMD-check/badge.svg)
[![Codecov test coverage](https://codecov.io/gh/epiforecasts/covidregionaldata/branch/master/graph/badge.svg)](https://codecov.io/gh/epiforecasts/covidregionaldata?branch=master)
[![develVersion](https://img.shields.io/badge/devel%20version-0.3.0-green.svg?style=flat)](https://github.com/epiforecasts/covidregionaldata)
[![Documentation](https://img.shields.io/badge/Package-documentation-lightgrey.svg?style=flat)](https://epiforecasts.io/covidregionaldata)
[![DOI](https://zenodo.org/badge/238177228.svg)](https://zenodo.org/badge/latestdoi/238177228)


*Note: This package makes extensive use of `memoise` and writes a `.cache` to the directory in which its functions are run. This speeds up data retrieval and avoids hitting rate limits but does not follow CRAN best practice.* **Use with care.** *The cache can be reset with `reset_cache()` when updated data is required from the online source.*

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

### Worldwide data

There are two sources of worldwide, country-level data on cases and deaths.

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

### Sub-national data

We have several functions to extract sub-national level data by country. These are typically at the admin-1 level, the largest regions available. We are also working on joining the data to standard georeferencing codes to allow easy mapping.

Currently we include functions for sub-national data in the following countries:

Europe

  +	Belgium

  +	France

  + Germany

  +	Italy

  +	Spain

  + United Kingdom

Americas

  +	Canada

  +	United States

Eastern Mediterranean

  + Afghanistan

Western Pacific

  + Korea

  + Japan

South-East Asia

  + None currently available

Africa

  + None currently available


We are working to improve and expand the package: please see the [Issues](https://github.com/epiforecasts/covidregionaldata/issues) and feel free to comment. We are keen to standardise geocoding (issues [#81](https://github.com/epiforecasts/covidregionaldata/issues/81) and [#84](https://github.com/epiforecasts/covidregionaldata/issues/84)) and include data on priority countries ([#72](https://github.com/epiforecasts/covidregionaldata/issues/72)). As our capacity is limited, we would very much appreciate any help on these and welcome new pull requests.

## Development

### Set up

Set your working directory to the home directory of this project (or use the provided Rstudio project). Install the analysis and all dependencies with:

```r
remotes::install_github("epiforecasts/covidregionaldata", dependencies = TRUE)
```

### Render documentation

Render the documentation with the following:

```bash
Rscript inst/scripts/render_output.R
```

### Docker


This package is developed in a docker container based on the tidyverse docker image.

To build the docker image run (from the `covidregionaldata` directory):

```bash
docker build . -t covidregionaldata
```

To run the docker image run:

```bash
docker run -d -p 8787:8787 --name covidregionaldata -e USER=covidregionaldata -e PASSWORD=covidregionaldata covidregionaldata
```

The rstudio client can be found on port :8787 at your local machines ip. The default username:password is covidregionaldata:covidregionaldata, set the user with -e USER=username, and the password with - e PASSWORD=newpasswordhere. The default is to save the analysis files into the user directory.

To mount a folder (from your current working directory - here assumed to be `tmp`) in the docker container to your local system use the following in the above docker run command (as given mounts the whole `covidregionaldata` directory to `tmp`).

```{bash, eval = FALSE}
--mount type=bind,source=$(pwd)/tmp,target=/home/covidregionaldata
```

To access the command line run the following:

```{bash, eval = FALSE}
docker exec -ti covidregionaldata bash
```

Alternatively the package environment can be accessed via [binder](https://mybinder.org/v2/gh/epiforecasts/covidregionaldata/master?urlpath=rstudio).
