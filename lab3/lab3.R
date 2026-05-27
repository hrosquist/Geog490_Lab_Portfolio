#-------------------------------------------------
# Lab 3: Census Mapping and Visualization
# Hailey Rosquist
# Purpose: Create 4 maps for King County / Seattle area:
# 1. Income choropleth
# 2. Total population choropleth
# 3. Population dot density map
# 4. Young persons graduated symbol map
#-------------------------------------------------

library(tidyverse)
library(tidycensus)
library(sf)
library(purrr)
library(scales)
library(tmap)
library(tigris)

options(tigris_use_cache = TRUE)
options(bitmapType = "quartz")

dir.create("figures", showWarnings = FALSE)

#-------------------------------------------------
# Data
#-------------------------------------------------

seattle_income <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "WA",
  county = "King",
  year = 2020,
  geometry = TRUE
) %>%
  st_transform(2926)

seattle_pop <- get_decennial(
  geography = "tract",
  variables = "P1_001N",
  state = "WA",
  county = "King",
  year = 2020,
  geometry = TRUE
) %>%
  st_transform(2926)

young_persons <- get_acs(
  geography = "tract",
  variables = "B09001_001",
  state = "WA",
  county = "King",
  year = 2020,
  geometry = TRUE
) %>%
  st_transform(2926)

#-------------------------------------------------
# Map 1: Median income choropleth
#-------------------------------------------------

income_map <- ggplot(seattle_income) +
  geom_sf(aes(fill = estimate), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(
    option = "plasma",
    labels = dollar
  ) +
  labs(
    title = "Median Household Income by Census Tract",
    subtitle = "King County, Washington — ACS 2020 5-year estimates",
    fill = "Median Income"
  ) +
  theme_minimal()

income_map

ggsave(
  "figures/seattle_income_choropleth.png",
  plot = income_map,
  width = 10,
  height = 8,
  dpi = 300
)

#-------------------------------------------------
# Map 2: Total population choropleth
#-------------------------------------------------

population_map <- ggplot(seattle_pop) +
  geom_sf(aes(fill = value), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(
    option = "magma",
    labels = comma
  ) +
  labs(
    title = "Total Population by Census Tract",
    subtitle = "King County, Washington — 2020 Decennial Census",
    fill = "Population"
  ) +
  theme_minimal()

population_map

ggsave(
  "figures/seattle_total_population_choropleth.png",
  plot = population_map,
  width = 10,
  height = 8,
  dpi = 300
)

#-------------------------------------------------
# Map 3: Dot density map
# 1 dot = 100 people
#-------------------------------------------------

seattle_dots <- map_dfr(1:nrow(seattle_pop), function(i) {
  n_dots <- floor(seattle_pop$value[i] / 100)

  if (n_dots > 0) {
    pts <- st_sample(seattle_pop[i, ], size = n_dots, exact = TRUE)
    st_sf(geometry = pts)
  }
})

dot_density_map <- ggplot() +
  geom_sf(
    data = seattle_pop,
    fill = "gray95",
    color = "gray80",
    linewidth = 0.2
  ) +
  geom_sf(
    data = seattle_dots,
    size = 0.15,
    alpha = 0.7
  ) +
  labs(
    title = "Dot Density Map of Total Population",
    subtitle = "King County, Washington — 1 dot = 100 people"
  ) +
  theme_minimal()

dot_density_map

ggsave(
  "figures/seattle_dot_density_map.png",
  plot = dot_density_map,
  width = 10,
  height = 8,
  dpi = 300
)

#-------------------------------------------------
# Map 4: Graduated symbol map
# Young persons = population under 18
# ACS B09001_001
#-------------------------------------------------

young_persons_points <- young_persons %>%
  st_point_on_surface()

# Create a King County outline for context
king_outline <- young_persons %>%
  summarise(geometry = st_union(geometry))

tmap_mode("plot")

young_persons_map <- tm_shape(king_outline) +
  tm_polygons(
    col = "grey96",
    border.col = "black",
    lwd = 2
  ) +
  tm_shape(young_persons) +
  tm_polygons(
    col = "grey88",
    border.col = "white",
    lwd = 0.6
  ) +
  tm_shape(young_persons_points) +
  tm_bubbles(
    size = "estimate",
    col = "navy",
    alpha = 0.65,
    border.col = "white",
    scale = 1.8,
    title.size = "Population under 18"
  ) +
  tm_compass(
    type = "arrow",
    position = c("right", "top"),
    size = 2
  ) +
  tm_scalebar(
    position = c("left", "bottom"),
    text.size = 0.9,
    lwd = 0.2
  ) +
  tm_layout(
    title = "Young Persons by Census Tract in King County, Washington",
    title.size = 1.4,
    frame = FALSE,
    legend.outside = TRUE,
    legend.outside.position = "right",
    inner.margins = c(0.04, 0.02, 0.08, 0.02)
  ) +
  tm_credits(
    "ACS 2020 5-year estimates, B09001_001: population under 18 years.",
    position = c("left", "bottom"),
    size = 0.5
  )

young_persons_map

# Save graduated symbols map without Cairo
png(
  filename = "figures/seattle_young_persons_graduated_symbols.png",
  width = 10,
  height = 8,
  units = "in",
  res = 300,
  type = "quartz"
)

print(young_persons_map)

dev.off()