source("custom_tests/test-regional-dataset.R")

# test your custom dataset and add a data snapshot using 
# test_regional_dataset(source = "region", level = c("1", "2"), download = TRUE)

# loops over all regional datasets and tests
download <- TRUE
source <- "ecdc"
level <- "1"

test_regional_dataset(source = source, level = level, download = TRUE)