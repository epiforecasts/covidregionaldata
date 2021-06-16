library(hexSticker)
library(ggplot2)
library(dplyr)
library(maps)

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
  p_size = 50, s_x = 0.93, s_y = 0.8, s_width = 1.7, s_height = 2,
  p_y = 1.45,
  p_color = "white",
  h_color = "#646770",
  h_fill = "#24A7DF",
  filename = "man/figures/logo.png",
  u_size = 3.5,
  dpi = 1000
)
