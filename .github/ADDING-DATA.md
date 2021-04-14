# Adding a new data set to `covidregionaldata`

Thank you for contributing to covidregionaldata! Please make sure you have read our contributing guide before reading on (see it [here](https://github.com/epiforecasts/covidregionaldata/tree/master/.github/CONTRIBUTING.md)).

## Adding a prototype function

Our datasets are implemented using R6 methods. You can read more about these methods [here](https://adv-r.hadley.nz/r6.html/). To help get you started we have provided a template [here](https://github.com/epiforecasts/covidregionaldata/blob/master/inst/CountryTemplate.R). As a first step copy this template into the `R` folder of your local copy of `covidregionaldata` and rename it to the country or data source you are adding support for. You should also rename all the `CountryTemplate` uses in the template (either using camel case if in code or using title case if written as text). For the next steps see [here](https://github.com/epiforecasts/covidregionaldata/blob/master/R/France.R) for an example simple dataset class and [here](https://github.com/epiforecasts/covidregionaldata/blob/master/R/UK.R) for a complex class. 


*This is a work in progress. Please comment in [this](https://github.com/epiforecasts/covidregionaldata/issues/268) issue if interested in expanding this guide.*
