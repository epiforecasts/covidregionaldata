# Adding a new data set to `covidregionaldata`

Thank you for contributing to covidregionaldata! This tutorial will take you through adding a new subnational dataset.

How to use this tutorial:
- Our datasets are implemented using R6 methods. If you are already very familiar with these, this guide may be too basic for you.
- First, clone the covidregionaldata repository and create a new branch. 
- Copy the [country template R script](./inst/CountryTemplate.R), rename it to the name of the country you would like to add, and save into the R/ folder.
- This guide will take you through how to add a data set using this script.
- When finished, please open a pull request. Our team will review the code and work with you to make any changes needed before we can merge the dataset into the package.
- Where possible, we use the tidyverse suite of packages for manipulating data.
