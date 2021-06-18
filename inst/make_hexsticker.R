library(hexSticker)
library(showtext)
library(ggplot2)
library(dplyr)
library(maps)
library(sf)

# font setup
font_add_google("Zilla Slab Highlight", "useme")

# get countries we have data for
regional_countries <- get_available_datasets() %>%
  filter(type == "regional")

regional_countries_l2 <- regional_countries %>%
  filter(!(is.na(level_2_region)))

# get world data
world <- spData::world %>%
  st_as_sf()

# mark supported countries from the world data
supported_countries <- world %>%
  mutate(
    fill = case_when(
      name_long %in% countryname(regional_countries_l2[["origin"]], , destination = "country.name.en") ~ "Level 2",
      name_long %in% countryname(regional_countries[["origin"]], , destination = "country.name.en") ~ "Level 1",
      TRUE ~ "Unsupported"
    )
  )

# make covid map
covid_map_1 <- ggplot() +
  ggspatial::layer_spatial(data = supported_countries, aes(fill = fill, color = fill, size = fill)) +
  coord_sf(crs = "ESRI:54010") +
  scale_fill_manual(
    name = "",
    values = c("#0072b2", "#cc79a7", "grey80")
  ) +
  scale_color_manual(
    name = "",
    values = c("black", "black", "#666666")
  ) +
  scale_size_manual(
    name = "",
    values = c(0.1, 0.1, 0.018)
  ) +
  cowplot::theme_minimal_grid(color = "white", line_size = 0.05) +
  theme(legend.position = "none", axis.text.x = element_blank())

logo1 <- sticker(
  covid_map_1,
  package = "covidregionaldata",
  p_size = 48, s_x = 1, s_y = 0.8, s_width = 1.98, s_height = 1.98,
  p_y = 1.45,
  p_color = "white",
  p_family = "useme",
  h_color = "#646770",
  h_fill = "#24A7DF",
  filename = "man/figures/logo1.png",
  u_size = 3.5,
  dpi = 1000
)

# make logo 2
supported_countries <- supported_countries %>%
  filter(name_long != "Antarctica")

covid_map_2 <- ggplot() +
  ggspatial::layer_spatial(data = supported_countries, aes(fill = fill, size = fill, color = fill)) +
  coord_sf(crs = "ESRI:54016") +
  scale_fill_manual(
    name = "",
    values = c("#0072b2", "#cc79a7", "grey80")
  ) +
  scale_color_manual(
    name = "",
    values = c("black", "black", "#666666")
  ) +
  scale_size_manual(
    name = "",
    values = c(0.1, 0.1, 0.018)
  ) +
  theme_void() +
  theme(legend.position = "none", axis.text.x = element_blank())

print(covid_map_2)

logo2 <- sticker(
  covid_map_2,
  package = "covidregionaldata",
  p_size = 48, s_x = 0.96, s_y = 0.8, s_width = 1.7, s_height = 1.7,
  p_y = 1.45,
  p_color = "white",
  p_family = "useme",
  h_color = "#646770",
  h_fill = "#24A7DF",
  filename = "man/figures/logo2.png",
  u_size = 3.5,
  dpi = 1000
)

logo3 <- sticker(
  covid_map_2,
  package = "covidregionaldata",
  p_size = 48, s_x = 1, s_y = 0.8, s_width = 1.7, s_height = 1.7,
  p_y = 1.45,
  p_color = "white",
  p_family = "useme",
  h_color = "#646770",
  h_fill = "#24A7DF",
  filename = "man/figures/logo3.png",
  u_size = 3.5,
  dpi = 1000
)
