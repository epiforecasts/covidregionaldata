test_that("new_covidregionaldata can correctly construct classes", {
    uk <- new_covidregionaldata("uk", "1", verbose = FALSE)
    expect_s3_class(
        suppressMessages(new_covidregionaldata("uk", "1", verbose = TRUE)),
        "crd_level_1"
    )
    expect_s3_class(
        suppressMessages(new_covidregionaldata("uk", "2", verbose = TRUE)),
        "crd_level_2"
    )
    expect_s3_class(
        suppressMessages(new_covidregionaldata("uk", "1", verbose = TRUE)),
        "crd_uk_1"
    )
    expect_s3_class(
        suppressMessages(new_covidregionaldata("ecdc", "1", verbose = TRUE)),
        "crd_ecdc_1"
    )
})

test_that("new_covidregionaldata can correctly construct lists", {
    mexico <- new_covidregionaldata("mexico", "1", verbose = FALSE)
    expect_type(mexico, "list")
    expect_equal(names(mexico), c("country", "level", "code", "codes_lookup"))
    expect_error(new_covidregionaldata("tmp", "1", verbose = FALSE))
    expect_error(new_covidregionaldata("tmp", "1", verbose = FALSE))
})

test_that("method defaults correctly handle unsupported data", {
    region <- new_covidregionaldata("uk", "1", verbose = FALSE)
    class(region) <- "list"
    region <- download_regional(region, verbose = FALSE)
    expect_true(is.na(region$raw))
    region <- clean_regional(region, verbose = FALSE)
    expect_true(is.na(region$clean))
    expect_error(process_regional(region, verbose = FALSE))
})

test_that("regional_return.default functions as expected", {
    tmp <- list(processed = "hello")
    expect_type(return_regional(tmp, steps = TRUE), "list")
    expect_type(return_regional(tmp, steps = FALSE), "character")
})
