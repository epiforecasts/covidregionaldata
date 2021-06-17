library(hexSticker)
library(showtext)
library(ggplot2)
library(dplyr)
library(maps)
library(sf)

# font setup
font_add_google("Zilla Slab Highlight", "useme")

# des
regional_countries <- get_available_datasets() %>%
  filter(type == "regional")

regional_countries_l2 <- regional_countries %>%
  filter(!(is.na(level_2_region)))

world <- map_data("world")

supported_countries <- mutate(
  world,
  fill = case_when(
    region %in% regional_countries_l2[["class"]] ~ "Level 2",
    region %in% regional_countries[["class"]] ~ "Level 1",
    TRUE ~ "Unsupported"
  )
)

covid_map <- ggplot(
  supported_countries, aes(long, lat, fill = fill, group = group)
) +
  geom_polygon(color = "black", size = 0.05) +
  scale_fill_manual(
    name = "",
    values = c("#0072b2", "#cc79a7", "grey80")
  ) +
  theme_void() +
  theme(legend.position = "none") +
  coord_fixed(ratio = 1.7, ylim = c(-55, 80))

logo <- sticker(
  covid_map,
  package = "covidregionaldata",
  p_size = 48, s_x = 0.93, s_y = 0.8, s_width = 1.7, s_height = 2,
  p_y = 1.45,
  p_color = "white",
  p_family = "useme",
  h_color = "#646770",
  h_fill = "#24A7DF",
  filename = "man/figures/logo.png",
  u_size = 3.5,
  dpi = 1000
)


world <- spData::world %>%
  st_as_sf()

supported_countries <- world %>%
  mutate(
    fill = case_when(
      name_long %in% countryname(regional_countries_l2[["class"]], , destination = "country.name.en") ~ "Level 2",
      name_long %in% countryname(regional_countries[["class"]], , destination = "country.name.en") ~ "Level 1",
      TRUE ~ "Unsupported"
    )
  ) # %>%
# filter(name_long != "Antarctica")


covid_map_2 <- ggplot() +
  ggspatial::layer_spatial(data = supported_countries, aes(fill = fill), color = "black", size = 0.05) +
  coord_sf(crs = "ESRI:54010") +
  scale_fill_manual(
    name = "",
    values = c("#0072b2", "#cc79a7", "grey80")
  ) +
  cowplot::theme_minimal_grid(color = "white", line_size = 0.05) +
  theme(legend.position = "none", axis.text.x = element_blank())

logo2 <- sticker(
  covid_map_2,
  package = "covidregionaldata",
  p_size = 48, s_x = 1, s_y = 0.8, s_width = 1.98, s_height = 1.98,
  p_y = 1.45,
  p_color = "white",
  p_family = "useme",
  h_color = "#646770",
  h_fill = "#24A7DF",
  filename = "man/figures/logo2.png",
  u_size = 3.5,
  dpi = 1000
)


# logo 3

supported_countries <- world %>%
  mutate(
    fill = case_when(
      name_long %in% countryname(regional_countries_l2[["class"]], , destination = "country.name.en") ~ "Level 2",
      name_long %in% countryname(regional_countries[["class"]], , destination = "country.name.en") ~ "Level 1",
      TRUE ~ "Unsupported"
    )
  ) %>%
  filter(name_long != "Antarctica")

covid_map_3 <- ggplot() +
  ggspatial::layer_spatial(data = supported_countries, aes(fill = fill), color = "black", size = 0.05) +
  coord_sf(crs = "ESRI:54016") +
  scale_fill_manual(
    name = "",
    values = c("#0072b2", "#cc79a7", "grey80")
  ) +
  theme_void() +
  theme(legend.position = "none", axis.text.x = element_blank())

logo3 <- sticker(
  covid_map_3,
  package = "covidregionaldata",
  p_size = 48, s_x = 0.96, s_y = 0.8, s_width = 1.7, s_height = 1.7,
  p_y = 1.45,
  p_color = "white",
  p_family = "useme",
  h_color = "#646770",
  h_fill = "#24A7DF",
  filename = "man/figures/logo3.png",
  u_size = 3.5,
  dpi = 1000
)
