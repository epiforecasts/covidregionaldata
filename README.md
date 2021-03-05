Subnational data for the Covid-19 outbreak
================

[![badge](https://img.shields.io/badge/Launch-package-lightblue.svg)](https://mybinder.org/v2/gh/epiforecasts/covidregionaldata/master?urlpath=rstudio)
[![R-CMD-check](https://github.com/epiforecasts/covidregionaldata/workflows/R-CMD-check/badge.svg)](https://github.com/epiforecasts/covidregionaldata/actions)
[![Codecov test
coverage](https://codecov.io/gh/epiforecasts/covidregionaldata/branch/master/graph/badge.svg)](https://codecov.io/gh/epiforecasts/covidregionaldata?branch=master)
[![develVersion](https://img.shields.io/badge/devel%20version-0.8.2-green.svg?style=flat)](https://github.com/epiforecasts/covidregionaldata/)
[![DOI](https://zenodo.org/badge/271601189.svg)](https://zenodo.org/badge/latestdoi/271601189)
[![metacran
downloads](http://cranlogs.r-pkg.org/badges/grand-total/covidregionaldata?color=ff69b4)](https://cran.r-project.org/package=covidregionaldata)

An interface to subnational and national level Covid-19 data. For all
countries supported, this includes a daily time-series of cases.
Wherever available we also provide data on deaths, hospitalisations, and
tests. National level data is also supported using a range of data
sources as well as linelist data and links to intervention data sets.

## Installation

Install from CRAN:

``` r
install.packages("covidregionaldata")
```

Install the stable development version of the package with:

``` r
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

### National data

To get worldwide time-series data by country (sourced from the WHO), use
`get_national_data()`:

``` r
covidregionaldata::get_national_data()
```

``` r
allcountries <- covidregionaldata::get_national_data()
```

| date       | un\_region | who\_region | country        | iso\_code | cases\_new | cases\_total | deaths\_new | deaths\_total | recovered\_new | recovered\_total | hosp\_new | hosp\_total | tested\_new | tested\_total |
| :--------- | :--------- | :---------- | :------------- | :-------- | ---------: | -----------: | ----------: | ------------: | -------------: | ---------------: | --------: | ----------: | ----------: | ------------: |
| 2021-03-05 | Asia       | EMRO        | Afghanistan    | AF        |         52 |        55827 |           3 |          2449 |             NA |               NA |        NA |          NA |          NA |            NA |
| 2021-03-05 | Europe     | EURO        | Albania        | AL        |         NA |       109674 |          NA |          1856 |             NA |               NA |        NA |          NA |          NA |            NA |
| 2021-03-05 | Africa     | AFRO        | Algeria        | DZ        |         NA |       113593 |          NA |          2996 |             NA |               NA |        NA |          NA |          NA |            NA |
| 2021-03-05 | Oceania    | WPRO        | American Samoa | AS        |          0 |            0 |           0 |             0 |             NA |               NA |        NA |          NA |          NA |            NA |
| 2021-03-05 | Europe     | EURO        | Andorra        | AD        |         NA |        10948 |          NA |           112 |             NA |               NA |        NA |          NA |          NA |            NA |
| 2021-03-05 | Africa     | AFRO        | Angola         | AO        |         NA |        20923 |          NA |           510 |             NA |               NA |        NA |          NA |          NA |            NA |

All country Covid-19 data

Using this data we can compare case information between countries, for
example here is the number of deaths over time for each country in the
G7:

``` r
g7 = c("United States", "United Kingdom", "France", "Germany", "Italy", "Canada", "Japan")
allcountries %>%
  filter(country %in% g7) %>% 
  ggplot(aes(x=date, y=deaths_total, color=country))+
  geom_line()+
  labs(x=NULL, y="Reported Covid-19 deaths")+
  theme_minimal()
```

![](README_files/figure-gfm/plotg7-1.png)<!-- -->

### National regions

To get time-series data for national regions of a specific country, for
example by region in the UK, use `get_regional_data()`:

``` r
ukdata <- covidregionaldata::get_regional_data(country = "UK")
```

| date       | region          | ons\_region\_code | cases\_new | cases\_total | deaths\_new | deaths\_total | recovered\_new | recovered\_total | hosp\_new | hosp\_total | tested\_new | tested\_total | areaType | newCasesBySpecimenDate | cumCasesBySpecimenDate | newCasesByPublishDate | cumCasesByPublishDate | newDeaths28DaysByPublishDate | cumDeaths28DaysByPublishDate | newDeaths28DaysByDeathDate | cumDeaths28DaysByDeathDate | newPillarOneTestsByPublishDate | newPillarTwoTestsByPublishDate | newPillarThreeTestsByPublishDate | newPillarFourTestsByPublishDate |
| :--------- | :-------------- | :---------------- | ---------: | -----------: | ----------: | ------------: | -------------: | ---------------: | --------: | ----------: | ----------: | ------------: | :------- | ---------------------: | ---------------------: | --------------------: | --------------------: | ---------------------------: | ---------------------------: | -------------------------: | -------------------------: | -----------------------------: | -----------------------------: | -------------------------------: | :------------------------------ |
| 2021-03-04 | East Midlands   | E12000004         |         NA |       308649 |          NA |          9724 |             NA |               NA |        NA |          NA |          NA |            NA | region   |                     NA |                     NA |                   660 |                308649 |                           22 |                         9724 |                         NA |                         NA |                             NA |                             NA |                               NA | NA                              |
| 2021-03-04 | East of England | E12000006         |         NA |       373302 |          NA |         13080 |             NA |               NA |        NA |          NA |          NA |            NA | region   |                     NA |                     NA |                   476 |                373302 |                           24 |                        13080 |                         NA |                         NA |                             NA |                             NA |                               NA | NA                              |
| 2021-03-04 | England         | E92000001         |         NA |      3679671 |          NA |        109195 |             NA |               NA |        NA |      382479 |          NA |      78119542 | nation   |                     NA |                     NA |                  5643 |               3679671 |                          202 |                       109195 |                         NA |                         NA |                             NA |                             NA |                               NA | NA                              |
| 2021-03-04 | London          | E12000007         |         NA |       698405 |          NA |         15214 |             NA |               NA |        NA |          NA |          NA |            NA | region   |                     NA |                     NA |                   623 |                698405 |                           19 |                        15214 |                         NA |                         NA |                             NA |                             NA |                               NA | NA                              |
| 2021-03-04 | North East      | E12000001         |         NA |       183351 |          NA |          5837 |             NA |               NA |        NA |          NA |          NA |            NA | region   |                     NA |                     NA |                   367 |                183351 |                           10 |                         5837 |                         NA |                         NA |                             NA |                             NA |                               NA | NA                              |
| 2021-03-04 | North West      | E12000002         |         NA |       579427 |          NA |         17372 |             NA |               NA |        NA |          NA |          NA |            NA | region   |                     NA |                     NA |                   889 |                579427 |                           27 |                        17372 |                         NA |                         NA |                             NA |                             NA |                               NA | NA                              |

Uk regional Covid-19 data

Now we have the data we can create plots, for example the evolution of
the number of cases for each region:

``` r
ukdata %>%
  ggplot(aes(x=date, y=cases_total, color=region))+
  geom_line()+
  labs(x=NULL, y="Reported Covid-19 cases")+
  theme_minimal()
```

![](README_files/figure-gfm/plotukregions-1.png)<!-- -->

### Subnational regions

To get time-series data for subnational regions of a specific country,
for example by local authority in the UK, use `include_level_2_regions =
TRUE`:

``` r
ukdata_sub <- covidregionaldata::get_regional_data(country = "UK", include_level_2_regions = T)
```

| date       | authority        | ltla\_code | region        | ons\_region\_code | cases\_new | cases\_total | deaths\_new | deaths\_total | recovered\_new | recovered\_total | hosp\_new | hosp\_total | tested\_new | tested\_total | areaType | newCasesBySpecimenDate | cumCasesBySpecimenDate | newCasesByPublishDate | cumCasesByPublishDate | newDeaths28DaysByPublishDate | cumDeaths28DaysByPublishDate | newDeaths28DaysByDeathDate | cumDeaths28DaysByDeathDate |
| :--------- | :--------------- | :--------- | :------------ | :---------------- | ---------: | -----------: | ----------: | ------------: | -------------: | ---------------: | --------: | ----------: | ----------: | ------------: | :------- | ---------------------: | ---------------------: | --------------------: | --------------------: | ---------------------------: | ---------------------------: | -------------------------: | -------------------------: |
| 2021-03-04 | Derby            | E06000015  | East Midlands | E12000004         |         NA |        19308 |          NA |           649 |             NA |               NA |        NA |          NA |          NA |            NA | utla     |                     NA |                     NA |                    30 |                 19308 |                            2 |                          649 |                         NA |                         NA |
| 2021-03-04 | Derbyshire       | E10000007  | East Midlands | E12000004         |         NA |        46669 |          NA |          1799 |             NA |               NA |        NA |          NA |          NA |            NA | utla     |                     NA |                     NA |                   119 |                 46669 |                            4 |                         1799 |                         NA |                         NA |
| 2021-03-04 | Leicester        | E06000016  | East Midlands | E12000004         |         NA |        34218 |          NA |           740 |             NA |               NA |        NA |          NA |          NA |            NA | utla     |                     NA |                     NA |                    84 |                 34218 |                            1 |                          740 |                         NA |                         NA |
| 2021-03-04 | Leicestershire   | E10000018  | East Midlands | E12000004         |         NA |        41770 |          NA |          1290 |             NA |               NA |        NA |          NA |          NA |            NA | utla     |                     NA |                     NA |                    93 |                 41770 |                            4 |                         1290 |                         NA |                         NA |
| 2021-03-04 | Lincolnshire     | E10000019  | East Midlands | E12000004         |         NA |        38091 |          NA |          1542 |             NA |               NA |        NA |          NA |          NA |            NA | utla     |                     NA |                     NA |                    81 |                 38091 |                            4 |                         1542 |                         NA |                         NA |
| 2021-03-04 | Northamptonshire | E10000021  | East Midlands | E12000004         |         NA |        45477 |          NA |          1357 |             NA |               NA |        NA |          NA |          NA |            NA | utla     |                     NA |                     NA |                    82 |                 45477 |                            3 |                         1357 |                         NA |                         NA |

Uk sub-national regional Covid-19 data

With the sub-national data we can evaluate the evolution of cases for
local authorities within each region, for example the East Midlands:

``` r
ukdata_sub %>%
  filter(region %in% c("East Midlands")) %>% 
  ggplot(aes(x=date, y=cases_total, color=authority))+
  geom_line()+
  labs(x=NULL, y="Reported Covid-19 cases")+
  theme_minimal()
```

![](README_files/figure-gfm/plotuksubregions-1.png)<!-- -->

Subnational regions are only available in some countries. See below
section 4, “Coverage”.

For further demonstrations see [Quick start
vignette](https://github.com/epiforecasts/covidregionaldata/blob/master/vignettes/quickstart.Rmd#L84)

## Usage

### Worldwide data

#### Accessing national data

Both the World Health Organisation (WHO) and European Centre for Disease
Control (ECDC) provide worldwide national data. Access national level
data for any country using:

``` r
covidregionaldata::get_national_data()
```

This returns daily new and cumulative (total) cases, and where
available, deaths, hospitalisations, and tests. For a complete list of
variables returned, see section 5, “Data glossary” below.

This function takes 3 optional arguments: \* `country` (optional) - a
country name (in any language) for which to return national level data.
This argument permits any country in the United Nations and reported by
the specified data source (ECDC or WHO). If not specified, all countries
will be returned.

  - `source` (optional, default is “WHO”) - the data source for national
    data. Either “ECDC” or “WHO”.

  - `totals` (optional, default is FALSE) - a Boolean (TRUE/FALSE),
    denoting whether the data returned should be a table of total counts
    (one row per country) or time series data (one row per country/date
    combination).

This returns data in the same structure as `get_regional_data()`. This
means there are no gaps in the structure of the data by country over
time, and NAs fill in where data are not available.

#### Accessing national government interventions

A further function for worldwide data extracts non-pharmaceutical
interventions by country:

  - `covidregionaldata::get_interventions_data()`

#### Accessing a patient linelist

Patient linelist data is useful for exploring delays and lags in
reporting. An anonymised international patient linelist can be imported
and cleaned with:

  - `covidregionaldata::get_linelist()`

### Sub-national time-series data

#### Accessing sub-national data

Access sub-national level data for a specific country over time by using
`covidregionaldata::get_regional_data()`.

This returns daily new and cumulative (total) cases, and where
available, deaths, hospitalisations, and tests. For a complete list of
variables returned, see section 5, “Data glossary” below.

The function takes 3 arguments: \* `country` - the English name of the
country of interest. Not case sensitive \* `totals` (optional, default
is FALSE) - a Boolean (TRUE/FALSE), denoting whether the data returned
should be a table of total counts (one row per region) *or* time series
data (one row per region/date combination). \* `include_level_2_regions`
(optional, default is FALSE) - a Boolean (TRUE/FALSE), denoting whether
the data returned should be stratified by admin level 1 region (usually
the largest subregion available) or admin level 2 region (usually the
second largest).

This returns a dataset with one row for each region for each date. For
all regions, dates span from the first date until the last date that
data are available for any region in the country. This means there are
no gaps in the structure of the data, although NAs fill in where data
are not available.

For example, data for Belgium Level 1 regions over time can be accessed
using:

``` r
get_regional_data(country = "Belgium")
```

This returns a dataset in this format:

|  **date**  | **region** | **iso\_code** | **cases\_new** | **cases\_total** | **deaths\_new** | **deaths\_total** | **recovered\_new** | **recovered\_total** | **hosp\_new** | **hosp\_total** | **tested\_new** | **tested\_total** |
| :--------: | :--------: | :-----------: | :------------: | :--------------: | :-------------: | :---------------: | :----------------: | :------------------: | :-----------: | :-------------: | :-------------: | :---------------: |
| 2020-05-24 |  Wallonia  |    BE-WAL     |       24       |      18196       |       16        |       3251        |         NA         |          NA          |       8       |      5126       |       NA        |        NA         |
| 2020-05-25 |  Brussels  |    BE-BRU     |       26       |       5838       |        2        |       1421        |         NA         |          NA          |       6       |      2533       |       NA        |        NA         |
| 2020-05-25 |  Flanders  |    BE-VLG     |      183       |      32381       |       14        |       4681        |         NA         |          NA          |      29       |      9334       |       NA        |        NA         |

#### Level 1 and Level 2 regions

All countries included in the package (see below,“Coverage”) have data
for regions at the admin-1 level, the largest administrative unit of the
country (e.g. state in the USA). Some countries also have data for
smaller areas at the admin-2 level (e.g. county in the USA).

Data for Level 2 units can be returned by using the
`include_level_2_regions = TRUE` argument. The dataset will still show
the corresponding Level 1 region.

An example of a country with Level 2 units is Belgium, where Level 2
units are Belgian provinces:

``` r
covidregionaldata::get_regional_data("Belgium", include_level_2_regions = TRUE)
```

This returns a dataset with the format:

|  **date**  | **province** | **level\_2\_region\_code** | **region** | **iso\_code** | **cases\_new** | **cases\_total** | **deaths\_new** | **deaths\_total** | **recovered\_new** | **recovered\_total** | **hosp\_new** | **hosp\_total** | **tested\_new** | **tested\_total** |
| :--------: | :----------: | :------------------------: | :--------: | :-----------: | :------------: | :--------------: | :-------------: | :---------------: | :----------------: | :------------------: | :-----------: | :-------------: | :-------------: | :---------------: |
| 2020-05-24 |   Brussels   |           BE-BRU           |  Brussels  |    BE-BRU     |       7        |       5812       |       NA        |        NA         |         NA         |          NA          |       4       |      2527       |       NA        |        NA         |
| 2020-05-24 |  Antwerpen   |           BE-VAN           |  Flanders  |    BE-VLG     |       16       |       7905       |       NA        |        NA         |         NA         |          NA          |       5       |      2510       |       NA        |        NA         |
| 2020-05-24 |   Limburg    |           BE-VLI           |  Flanders  |    BE-VLG     |       14       |       6126       |       NA        |        NA         |         NA         |          NA          |       2       |      1848       |       NA        |        NA         |

#### Totals

For totalled data up to the most recent date available, use the `totals`
argument.

``` r
covidregionaldata::get_regional_data("Belgium", totals = TRUE)
```

This returns a dataset with one row for each region, in the same format:

| **region** | **iso\_code** | **cases\_total** | **deaths\_total** | **recovered\_total** | **hosp\_total** | **tested\_total** |
| :--------: | :-----------: | :--------------: | :---------------: | :------------------: | :-------------: | :---------------: |
|  Flanders  |    BE-VLG     |      34195       |       4878        |          0           |      9694       |         0         |
|  Wallonia  |    BE-WAL     |      19093       |       3362        |          0           |      5321       |         0         |
|  Brussels  |    BE-BRU     |       6229       |       1482        |          0           |      2657       |         0         |

## Sub-national coverage
<<<<<<< HEAD
=======
We include sub-national data in the following countries. These are the accepted country names when using `get_regional_data(country = "")`.

**Continent** | **Country**|**Level 1**|**Level 2**
:-----:| :-----:|:-----:|:-----:
Europe | Belgium | Region | Province
Europe | France  | Région | Département
Europe | Germany | Bundesland | Landkreis
Europe | UK | NHS region | Local authority
Europe | Italy | Region | NA
Europe | Lithuania | County | Municipality
Americas | Brazil | State | City
Americas | USA | State | County
Americas | Canada | Province | NA
Americas | Colombia | Department  | NA
Americas | Cuba | Province | NA
Americas | Mexico | Estado | Municipio 
Asia | Afghanistan | Province | NA
Asia | India | States | NA

We are hoping to expand over time (see below "Development").
>>>>>>> master

We include sub-national data in the following countries. These are the
accepted country names when using `get_regional_data(country = "")`.

| **Continent** | **Country** | **Level 1** |   **Level 2**   |
| :-----------: | :---------: | :---------: | :-------------: |
|    Europe     |   Belgium   |   Region    |    Province     |
|    Europe     |   France    |   Région    |   Département   |
|    Europe     |   Germany   | Bundesland  |    Landkreis    |
|    Europe     |     UK      | NHS region  | Local authority |
|    Europe     |    Italy    |   Region    |       NA        |
|   Americas    |   Brazil    |    State    |      City       |
|   Americas    |     USA     |    State    |     County      |
|   Americas    |   Canada    |  Province   |       NA        |
|   Americas    |  Colombia   | Department  |       NA        |
|   Americas    |    Cuba     |  Province   |       NA        |
|   Americas    |   Mexico    |   Estado    |    Municipio    |
|     Asia      | Afghanistan |  Province   |       NA        |
|     Asia      |    India    |   States    |       NA        |

We are hoping to expand over time (see below “Development”).

## Data glossary

#### Subnational data

The data columns that will be returned by `get_regional_data()` are
listed below.

To standardise across countries and regions, the columns returned for
each country will *always* be the same. If the corresponding data was
missing from the original source then that data field is filled with NA
values (or 0 if accessing totals data).

Note that Date is not included if the `totals` argument is set to TRUE.
Level 2 region/level 2 region code are not included if the
`include_level_2_regions` argument is set to FALSE.

  - `date`: the date that the counts were reported (YYYY-MM-DD).
  - `level 1 region`: the level 1 region name. This column will be named
    differently for different countries (e.g. state, province).
  - `level 1 region code`: a standard code for the level 1 region. The
    column name reflects the specific administrative code used.
    Typically data returns the iso\_3166\_2 standard, although where not
    available the column will be named differently to reflect its
    source.
  - `level 2 region`: the level 2 region name. This column will be named
    differently for different countries (e.g. city, county).
  - `level 2 region code`: a standard code for the level 2 region. The
    column will be named differently for different countries
    (e.g. `fips` in the USA).
  - `cases_new`: new reported cases for that day
  - `cases_total`: total reported cases up to and including that day
  - `deaths_new`: new reported deaths for that day
  - `deaths_total`: total reported deaths up to and including that day
  - `recovered_new`: new reported recoveries for that day
  - `recovered_total`: total reported recoveries up to and including
    that day
  - `hosp_new`: new reported hospitalisations for that day
  - `hosp_total`: total reported hospitalisations up to and including
    that day (note this is cumulative total of new reported, *not* total
    currently in hospital)
  - `tested_new`: tests for that day
  - `tested_total`: total tests completed up to and including that day

The exception to this is data for the UK. This is in its raw state, as
regions have separate and sometimes incompatible data reporting.

#### National data

In addition to the above, the following columns are included when using
`get_national_data()`. \* `un_region`: country geographical region
defined by the United Nations. \* `who_region`: only included when
`source = "WHO"`. Country geographical region defined by WHO. \*
`population_2019`: only included when `source = "ECDC"` (the default).
Total country population estimate in 2019.

## Development

We welcome contributions and new contributors\! We particularly
appreciate help adding new data sources for countries at sub-national
level, or work on priority problems in the
[issues](https://github.com/epiforecasts/covidregionaldata/issues).
Please check and add to the issues, and/or add a [pull
request](https://github.com/epiforecasts/covidregionaldata/pulls). For
more detail, please read the [System Maintenance
Guide](https://github.com/epiforecasts/covidregionaldata/blob/master/inst/smg/SMG.md).
