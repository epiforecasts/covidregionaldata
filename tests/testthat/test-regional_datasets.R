# should data be downloaded or snapshot used

# is there a target dataset or should all datasets be tested

# loop overall target datasets

# loop over available levels

source <- "mexico"
level <- "1"
data_name <- paste0(source, "at ", level)
# setup data class and test it can be found
test_that(paste0(data_name, " can be defined as a class"), {
    expect_error(
        new_covidregionaldata(source, level = level, verbose = FALSE),
        NA
    )
})
region <- new_covidregionaldata(source, level = level, verbose = FALSE)

if (download) {
    test_that(paste0(date_name, " downloads sucessfully"), {
        
    })

}
# test data download (does it download)

# load stored data if present

# compare stored with downloaded data

# save new data snapshot

# test cleaning of data

# test processing of data

# test data return