library(covidregionaldata)

# set up a data cache
start_using_memoise()

# check for supported countries
get_available_datasets()

# get national level data from the WHO
get_national_data(source = "who")

# get national level data from Google
get_national_data(source = "google", verbose = FALSE)

# get national level data from the JHU
get_national_data(source = "JHU", verbose = FALSE)

# get regional level data for the UK with NHS regions data
# here we turn off all optional processing steps
get_regional_data("uk", nhsregions = TRUE, process_fns = c(), verbose = FALSE)

# initialise the Google data source for level 2 data
google <- initialise_dataclass("google", level = 2)

# download the Google source (here uses the cache defined above)
google$download()

# clean the Google data
google$clean()

# show available regions with level 2 data
google$available_regions()

# filter to UK data available from Google
google$filter("united kingdom")

# process UK data
google$process()

# return UK data
google$return()
