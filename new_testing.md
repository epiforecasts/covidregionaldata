
## New testing summary

**Note: This document should be moved into the wiki once this PR is
clear to be merged**

This PR moves testing into the class methods, allowing tests to be ran
interactively from within the country class object, and from within the
existing testing infrastructure.

`DataClass` is the parent class of all country classes, such as `Italy`,
`UK`, `USA`, etc. The generic testing function is the method `test`
within `DataClass`.

Interactively, tests can be ran by doing the following:

``` r
ukdata <- covidregionaldata::UK$new(level = "1", verbose = FALSE)
# could use anything but I used the acutal test one here for simplicity
ukdata$test(snapshot_path = "tests/testthat/custom_data/UK_level_1.rds")
```

    ## Test passed 😸
    ## Test passed 🎉
    ## Test passed 🎉
    ## Test passed 😸
    ## Test passed 😸
    ## Test passed 🌈
    ## Test passed 🌈
    ## Test passed 🥳
    ## Test passed 🎊
    ## Test passed 🎊

Here, I break down the different components of this function to walk
through how the tests are run.

``` r
test <- function(download = FALSE, snapshot_path = "",
                 test_all = FALSE, ...) {
  if (!grepl(".rds$", snapshot_path)) {
    stop("snapshot_path must be to an rds file")
  }
  self_copy <- self$clone()
  test_download(
    cntry_obj = self_copy,
    download = download,
    snapshot_path = snapshot_path
  )
  test_cleaning(
    cntry_obj = self_copy
  )
  test_processing(
    cntry_obj = self_copy,
    test_all = test_all
  )
  test_return(
    cntry_obj = self_copy
  )

  if ("specific_tests" %in% names(self_copy)) {
    specific <- paste0(
      "self$specific_tests(self_copy=self_copy, download=download, ...)"
    )
    eval(parse(text = specific))
  }
}
```

For a given country, such as the UK, you would make this by calling
something like `ukdata <- UK$new(level = "1")` and can run the steps one
by one by calling the respective methods: `ukdata$download();
ukdata$clean(); ukdata$process()`. To run the tests you would call
`ukdata$test(shapshot_path = "path/2/data.rds")`. Rather than running
tests on your active class, the code first makes a clone of your class
which is then used for tests: `self_copy <- self$clone()` The snapshot
path is the path to a rds file where you either have stored some raw
downloaded data to test, or where you want a downloaded snapshot of the
data to end up. This is handled by the function `test_download()`. The
cloned class is passed to this function, along with the download
parameter, which dictates whether to overwrite the snapshot file
provided.

``` r
test_download <- function(cntry_obj, download, snapshot_path) {
  if (!file.exists(snapshot_path)) {
    download <- TRUE
  }
  if (download) {
    testthat::test_that(paste0(cntry_obj$data_name, " downloads sucessfully"), {
      cntry_obj$download()
      walk(cntry_obj$data$raw, function(data) {
        testthat::expect_s3_class(data, "data.frame")
        testthat::expect_true(nrow(data) > 0)
        testthat::expect_true(ncol(data) >= 2)
      })
    })
    cntry_obj$data$raw <- map(cntry_obj$data$raw,
      slice_tail,
      n = 250
    )
    cntry_obj$data$raw <- map(
      cntry_obj$data$raw,
      ~ .[, 1:min(100, ncol(.))]
    )
    saveRDS(cntry_obj$data$raw, snapshot_path)
  } else {
    cntry_obj$data$raw <- readRDS(snapshot_path)
  }
}
```

As shown by the code, if the data is to be downloaded (either through
requesting this with the download parameter or by providing a path to a
none existent file) then the download method is called on the class copy
(`cntry_obj`): `cntry_obj$download()`. After this has downloaded the
code tests the data is a data.frame, is not empty and has at least 2
columns. The code then takes the first 250 rows to work on, as using the
whole data set could be very slow and for the purpose of testing is not
needed. This sliced data is then saved to the snapshot file provided for
use later on. If the data is not to be downloaded, the snapshot of data
saved in the snapshot path is loaded as the raw data
`cntry_obj$data$raw`.

Once the data is downloaded, the cleaning methods are then tested
`test_cleaning()`. Again this function takes the copied class as the
argument and runs through tests on the clean data. These tests check the
data is a data.frame, is not empty, has more than 2 columns and that the
method `avaliable_regions` returns a list of characters. In addition,
`expect_clean_cols` checks the date column is an s3 class and that
region level column is a character in the cleaned data (`data$clean`).

``` r
test_cleaning <- function(cntry_obj) {
  testthat::test_that(
    paste0(cntry_obj$data_name, " can be cleaned as expected"),
    {
      cntry_obj$clean()
      testthat::expect_s3_class(cntry_obj$data$clean, "data.frame")
      testthat::expect_true(nrow(cntry_obj$data$clean) > 0)
      testthat::expect_true(ncol(cntry_obj$data$clean) >= 2)
      expect_clean_cols(cntry_obj$data$clean, cntry_obj$level)
    }
  )
  testthat::test_that(
    paste0(cntry_obj$data_name, " can highlight available regions as expected"),
    {
      testthat::expect_error(cntry_obj$available_regions(), NA)
      testthat::expect_true(
        class(cntry_obj$available_regions()) %in% "character"
      )
    }
  )
}
```

Once cleaning has been tested `test_processing()` is called to process
the data and run tests to check it all works. Again this function takes
the clone of the class to work on, the same clone which has been called
with the preceding functions. These tests check the data is a
data.frame, is not empty, has more than 2 columns. In addition
`expect_processed_cols` checks that processed data columns date,
cases\_new, cases\_total, deaths\_new, deaths\_total and that region
level have the correct types.

In processing there is an extra parameter called `test_all`. This
parameter, if `TRUE` runs the processing step with both localised as
`TRUE` and `FALSE`, making another copy of the object to check the
localised data on so not to influence further tests. If `FALSE`
processing is ran with localised set to whatever it is set at prior to
`test()` being invoked.

``` r
test_processing <- function(cntry_obj, test_all = FALSE) {
  testthat::test_that(
    paste0(cntry_obj$data_name, " can be processed as expected"),
    {
      cntry_obj$process()
      testthat::expect_s3_class(cntry_obj$data$processed, "data.frame")
      testthat::expect_true(nrow(cntry_obj$data$processed) > 0)
      testthat::expect_true(ncol(cntry_obj$data$processed) >= 2)
      expect_processed_cols(
        cntry_obj$data$processed,
        level = cntry_obj$level,
        localised = cntry_obj$localise
      )
      if (test_all) {
        local_region <- cntry_obj$clone()
        local_region$localise <- FALSE
        local_region$process()
        expect_processed_cols(
          local_region$data$processed,
          level = local_region$level,
          localised = local_region$localise
        )
      }
    }
  )
}
```

After processing the return method is tested with `test_return` which
check the data is a data.frame, is not empty, has more than 2 columns
and that all columns contain data and are not just composed of NAs.

`{r, eval = FALSE, echo = TRUE test_return <- function(cntry_obj) {
testthat::test_that( paste0(cntry_obj$data_name, " can be returned as
expected"), { returned <- cntry_obj$return() if (any(class(returned)
%in% "data.frame")) { testthat::expect_s3_class(returned, "data.frame")
testthat::expect_true(nrow(returned) > 0)
testthat::expect_true(ncol(returned) >= 2) } } )
expect_columns_contain_data(cntry_obj) }`

These tests form the generic tests applied to all classes. However,
country specific tests are then called by calling the method
`specific_tests` if that country has specific tests defined. So for
`Italy`, where there are no specific tests, no specific tests are
called, but for `UK`, `WHO` and `ECDC` specific tests are ran though,
which are defined in their own country class. These functions should
take a clone of the class as an argument (`self_copy` ) and any
additional arguments they may need, such as a path to NHS included data
for `UK`.

## Integration with *testthat*

As well as interactive tests the `test()` method is also used by
`testthat` when conducting package level tests but with the argument
`test_all = TRUE`. This is done in the following code:

``` r
test_regional_dataset <- function(source, level, download = FALSE) {
  region <- eval(parse(
    text = paste0(
      source, "$new(level = '", level,
      "', verbose = FALSE)"
    )
  ))
  snapshot_path <- paste0(
    "custom_data/", source,
    "_level_", level, ".rds"
  )

  # UK nhs data snapshot
  nhs_snapshot_path <- paste0(
    "custom_data/", source,
    "_level_", level, "_nhs", ".rds"
  )
  region$test(
    download = download,
    snapshot_path = snapshot_path,
    test_all = TRUE,
    nhs_included_path = nhs_snapshot_path
  )
}
```
