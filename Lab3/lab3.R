# April 16, 2026
# Hailey Rosquist
# Purpose: Make exploratory census maps in R for Lab 3,
# including choropleth maps and a dot density map.

#-------------------------------------------------
# Libraries
library(tidyverse)
library(tidycensus)
library(sf)
library(purrr)
library(scales)

#-------------------------------------------------
# 1. Median household income by census tract
# Seattle, Washington (King County)

seattle_income <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "WA",
  county = "King",
  year = 2020,
  geometry = TRUE
)

# View the data
glimpse(seattle_income)

#-------------------------------------------------
# 2. Choropleth map: Median household income

ggplot(seattle_income) +
  geom_sf(aes(fill = estimate), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(
    option = "plasma",
    labels = dollar
  ) +
  labs(
    title = "Median Household Income by Census Tract",
    subtitle = "Seattle, Washington (King County ACS 2020)",
    fill = "Median Income"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 8),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  )

#-------------------------------------------------
# 3. Total population by census tract
# Seattle, Washington (King County)

seattle_pop <- get_decennial(
  geography = "tract",
  variables = "P1_001N",
  state = "WA",
  county = "King",
  year = 2020,
  geometry = TRUE
)

# View the data
glimpse(seattle_pop)

#-------------------------------------------------
# 4. Choropleth map: Total population

ggplot(seattle_pop) +
  geom_sf(aes(fill = value), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(option = "magma") +
  labs(
    title = "Total Population by Census Tract",
    subtitle = "Seattle, Washington (2020 Census)",
    fill = "Population"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 8),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  )

#-------------------------------------------------
# 5. Dot density map
# 1 dot = 100 people

seattle_pop_proj <- st_transform(seattle_pop, 2285)

seattle_dots <- map_df(1:nrow(seattle_pop_proj), function(i) {
  n_dots <- floor(seattle_pop_proj$value[i] / 100)

  if (n_dots > 0) {
    pts <- st_sample(seattle_pop_proj[i, ], size = n_dots, exact = TRUE)
    st_sf(geometry = pts)
  }
})

#-------------------------------------------------
# 6. Dot density map plot

ggplot() +
  geom_sf(
    data = seattle_pop,
    fill = "gray95",
    color = "gray80",
    linewidth = 0.2
  ) +
  geom_sf(
    data = seattle_dots,
    size = 0.2,
    alpha = 0.8
  ) +
  labs(
    title = "Dot Density Map of Population",
    subtitle = "Seattle, Washington — 1 dot = 100 people"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 8),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    plot.margin = margin(10, 10, 10, 10)
  )

#-------------------------------------------------
# Create a folder to save figures

dir.create("figures", showWarnings = FALSE)

#-------------------------------------------------
# 2. Choropleth map: Median household income

income_map <- ggplot(seattle_income) +
  geom_sf(aes(fill = estimate), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(
    option = "plasma",
    labels = dollar
  ) +
  labs(
    title = "Median Household Income by Census Tract",
    subtitle = "Seattle, Washington (King County ACS 2020)",
    fill = "Median Income"
  ) +
  theme_minimal()

# Save figure
ggsave(
  filename = "figures/seattle_income_map.png",
  plot = income_map,
  width = 10,
  height = 8,
  dpi = 300
)

# Display map
income_map

#-------------------------------------------------
# 4. Choropleth map: Total population

population_map <- ggplot(seattle_pop) +
  geom_sf(aes(fill = value), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(option = "magma") +
  labs(
    title = "Total Population by Census Tract",
    subtitle = "Seattle, Washington (2020 Census)",
    fill = "Population"
  ) +
  theme_minimal()

# Save figure
ggsave(
  filename = "figures/seattle_population_map.png",
  plot = population_map,
  width = 10,
  height = 8,
  dpi = 300
)

# Display map
population_map

#-------------------------------------------------
# 6. Dot density map plot

dot_density_map <- ggplot() +
  geom_sf(
    data = seattle_pop,
    fill = "gray95",
    color = "gray80",
    linewidth = 0.2
  ) +
  geom_sf(
    data = seattle_dots,
    size = 0.2,
    alpha = 0.8
  ) +
  labs(
    title = "Dot Density Map of Population",
    subtitle = "Seattle, Washington — 1 dot = 100 people"
  ) +
  theme_minimal()

# Save figure
ggsave(
  filename = "figures/seattle_dot_density_map.png",
  plot = dot_density_map,
  width = 10,
  height = 8,
  dpi = 300
)

# Display map
dot_density_map