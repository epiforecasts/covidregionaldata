library(covidregionaldata)
library(hexSticker)
library(showtext)
library(ggplot2)
library(dplyr)
library(maps)
library(countrycode)
library(sf)
library(rnaturalearth)
library(rmapshaper)

# This code may fail if you do not have two more packages installed.
# They are not on CRAN, but can likely be installed with the following
# lines:
# devtools::install_github("ropensci/rnaturalearthdata") # nolint
# devtools::install_github("ropensci/rnaturalearthhires") # nolint
# Alternatively use the provided development dockerfile (optionally via VScode)

# font setup
font_add_google("Zilla Slab Highlight", "useme")

# get countries we have data for
regional_countries <- get_available_datasets() %>%
  filter(.data$type == "regional")

regional_countries_l2 <- regional_countries %>%
  filter(!(is.na(.data$level_2_region)))

regional_countries_l1 <- regional_countries %>%
  filter(is.na(.data$level_2_region))

# Approach using Natural Earth data

world_without_regions <- ne_countries(returnclass = "sf") %>%
  filter(sovereignt != "Antarctica")

# numberOfLevels should be less than half the number of colours in the
# divergent palette used (usually 7)
numberOfLevels <- 3

regional_maps_l1 <- ms_simplify(
  ne_states(gsub(" \\(.*\\)", "",
    regional_countries_l1$origin,
    perl = TRUE
  ), returnclass = "sf") %>%
    mutate(region_code = woe_id %% numberOfLevels),
  keep = 0.04
)

regional_maps_l2 <- ms_simplify(
  ne_states(gsub(" \\(.*\\)", "",
    regional_countries_l2$origin,
    perl = TRUE
  ), returnclass = "sf") %>%
    mutate(region_code = woe_id %% numberOfLevels + numberOfLevels + 1),
  keep = 0.04
)

regional_maps <- bind_rows(regional_maps_l1, regional_maps_l2)

# We keep 50% of the points of the country outlines because it's a
# finer scale map
# We add the US and the UK to the list because otherwise we don't
# successfully include them.
regional_outlines <- ms_lines(
  ms_simplify(
    ne_countries(
      country = c(
        gsub(" \\(.*\\)", "", regional_countries$origin, perl = TRUE),
        "United States", "United Kingdom"
      ),
      returnclass = "sf",
      scale = 50
    ),
    keep = 0.1
  )
)

covid_map <- ggplot() +
  ggspatial::layer_spatial(data = world_without_regions, size = 0.01) +
  ggspatial::layer_spatial(
    data = regional_maps,
    aes(fill = region_code), size = 0.02
  ) +
  ggspatial::layer_spatial(
    data = regional_outlines,
    aes(colour = factor(RANK)), size = 0.1
  ) +
  coord_sf(crs = 54016) +
  scale_fill_fermenter(palette = "RdBu") +
  theme_void() +
  scale_colour_manual(name = "", values = c("black", "black", "black")) +
  theme(legend.position = "none", axis.text.x = element_blank())

print(covid_map)

logo <- sticker(
  covid_map,
  package = "covidregionaldata",
  p_size = 48, s_x = 1, s_y = 0.8, s_width = 1.7, s_height = 1.7,
  p_y = 1.45,
  p_color = "white",
  p_family = "useme",
  h_color = "#646770",
  h_fill = "#24A7DF",
  filename = "man/figures/logo.png",
  u_size = 3.5,
  dpi = 1000
)
