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

See the individual function documentation for full details of present functionality. The main function imports and cleans the linelist. 

```r
NCoVUtils::get_linelist()
```

Extract international case counts compiled by WHO using:

```r
NCoVUtils::get_who_cases()
```

Extract international case counts compiled by ECDC using:

```r
NCoVUtils::get_ecdc_cases()
```

Extract total global cases (and deaths) by country using:

```r
NCoVUtils::get_total_cases()
```

Extract non-pharmaceutical interventions by country using:

```r
NCoVUtils::get_interventions_data()
```

Extract country-level regional cases from:  

* Belgium  
* Canada  
* France  
* Germany  
* Italy  
* Spain  
* United Kingdom  
* United States  
* Japan  
* Korea
* Afghanistan

See the function reference for a full list of data sets that package can be used to extract.

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
