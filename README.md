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

### Worldwide data

There are three sources of worldwide, country-level data on cases. One also includes deaths.

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

### Sub-national data

We have several functions to extract sub-national level data by country. These are typically at the admin-1 level, the largest regions available. We are also working on joining the data to standard georeferencing codes to allow easy mapping.

Currently we include functions for sub-national data in the following countries:

Europe

  +	Belgium

  +	France

  + Germany

  +	Italy

  +	Spain

  + United Kingdom - use `NCoVUtils::get_uk_nhs_region_cases()`

Americas

  +	Canada

  +	United States - use `NCoVUtils::get_us_regional_cases()`

Eastern Mediterranean

  + Afghanistan

Western Pacific

  + Korea

  + Japan

South-East Asia

  + None currently available

Africa

  + None currently available


We are working to include more countries.


## Development

### Set up

Set your working directory to the home directory of this project (or use the provided Rstudio project). Install the analysis and all dependencies with:

```r
remotes::install_github("epiforecasts/NCoVUtils", dependencies = TRUE)
```

### Render documentation

Render the documentation with the following:

```bash
Rscript inst/scripts/render_output.R
```

### Docker


This package is developed in a docker container based on the tidyverse docker image.

To build the docker image run (from the `NCoVUtils` directory):

```bash
docker build . -t ncovutils
```

To run the docker image run:

```bash
docker run -d -p 8787:8787 --name ncovutils -e USER=ncovutils -e PASSWORD=ncovutils ncovutils
```

The rstudio client can be found on port :8787 at your local machines ip. The default username:password is ncovutils:ncovutils, set the user with -e USER=username, and the password with - e PASSWORD=newpasswordhere. The default is to save the analysis files into the user directory.

To mount a folder (from your current working directory - here assumed to be `tmp`) in the docker container to your local system use the following in the above docker run command (as given mounts the whole `ncovutils` directory to `tmp`).

```{bash, eval = FALSE}
--mount type=bind,source=$(pwd)/tmp,target=/home/ncovutils
```

To access the command line run the following:

```{bash, eval = FALSE}
docker exec -ti ncovutils bash
```

Alternatively the package environment can be accessed via [binder](https://mybinder.org/v2/gh/epiforecasts/ncovutils/master?urlpath=rstudio).
