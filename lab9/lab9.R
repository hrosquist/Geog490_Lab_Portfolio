#-------------------------------------------------
# Lab 9 - Spatial Regression
# Hailey Rosquist
# Spring 2026
# Purpose:
# Perform geographically weighted regression
# and k-means clustering for the Seattle MSA
#-------------------------------------------------

#-------------------------------------------------
# Libraries
#-------------------------------------------------

library(tidyverse)
library(tidycensus)
library(sf)
library(ggplot2)
library(GWmodel)
library(RColorBrewer)
library(ragg)

#-------------------------------------------------
# Settings
#-------------------------------------------------

setwd("~/Desktop/geog490/lab9")

dir.create("figures", showWarnings = FALSE)

options(tigris_use_cache = TRUE)

#-------------------------------------------------
# Geography of Interest:
# Seattle-Tacoma-Bellevue MSA
# Counties: King, Pierce, Snohomish
# Data: 2023 ACS 5-year Census tract data
#-------------------------------------------------

sea <- get_acs(
  geography = "tract",
  variables = c(
    median_value = "B25077_001",
    median_rooms = "B25018_001",
    total_population = "B01003_001",
    college_total = "B15003_001",
    bach = "B15003_022",
    masters = "B15003_023",
    prof = "B15003_024",
    phd = "B15003_025",
    foreign_total = "B05002_001",
    foreign_born = "B05002_013",
    white = "B03002_003",
    race_total = "B03002_001",
    median_age = "B01002_001",
    median_year_built = "B25035_001",
    housing_total = "B25003_001",
    owner_occupied = "B25003_002"
  ),
  state = "WA",
  county = c("King", "Pierce", "Snohomish"),
  year = 2023,
  survey = "acs5",
  geometry = TRUE,
  output = "wide"
)

#-------------------------------------------------
# Clean and create Walker-style variables
#-------------------------------------------------

sea_wide <- sea %>%
  rename_with(~ sub("E$", "", .x), everything()) %>%
  mutate(
    pct_college =
      100 * (bach + masters + prof + phd) / college_total,
    pct_foreign_born =
      100 * foreign_born / foreign_total,
    pct_white =
      100 * white / race_total,
    median_structure_age =
      2023 - median_year_built,
    percent_ooh =
      100 * owner_occupied / housing_total,
    area_sqmi =
      as.numeric(st_area(.)) / 2589988.11,
    pop_density =
      total_population / area_sqmi,
    log_median_value =
      log(median_value)
  ) %>%
  filter(
    median_value > 0,
    !is.na(log_median_value),
    !is.na(median_rooms),
    !is.na(pct_college),
    !is.na(pct_foreign_born),
    !is.na(pct_white),
    !is.na(median_age),
    !is.na(median_structure_age),
    !is.na(percent_ooh),
    !is.na(pop_density),
    !is.na(total_population),
    is.finite(log_median_value),
    is.finite(pct_college),
    is.finite(pct_foreign_born),
    is.finite(pct_white),
    is.finite(percent_ooh),
    is.finite(pop_density)
  )

#-------------------------------------------------
# Project data
# EPSG 26910 = UTM Zone 10N, good for western WA
#-------------------------------------------------

sea_projected <- st_transform(sea_wide, 26910)

#-------------------------------------------------
# PART 1:
# Geographically Weighted Regression
# Same variables as Walker, new MSA
#-------------------------------------------------

formula_gwr <- log_median_value ~ median_rooms +
  pct_college +
  pct_foreign_born +
  pct_white +
  median_age +
  median_structure_age +
  percent_ooh +
  pop_density +
  total_population

sea_sp <- as(sea_projected, "Spatial")

# Choose bandwidth
gwr_bandwidth <- bw.gwr(
  formula_gwr,
  data = sea_sp,
  approach = "AICc",
  kernel = "bisquare",
  adaptive = TRUE
)

# Run GWR model
gwr_model <- gwr.basic(
  formula_gwr,
  data = sea_sp,
  bw = gwr_bandwidth,
  kernel = "bisquare",
  adaptive = TRUE
)

print(gwr_model)

gwr_results <- st_as_sf(gwr_model$SDF)

#-------------------------------------------------
# Figure 1:
# GWR Local R-squared Map
#-------------------------------------------------

gwr_map <- ggplot(gwr_results) +
  geom_sf(aes(fill = Local_R2), color = NA) +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    name = "Local R²"
  ) +
  labs(
    title = "Geographically Weighted Regression",
    subtitle = "Local R² for Median Home Value, Seattle MSA",
    caption = "Data source: 2023 ACS 5-year estimates"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 8),
    legend.position = "right"
  )

gwr_map

ggsave(
  filename = "figures/figure1_gwr_income_map.png",
  plot = gwr_map,
  device = ragg::agg_png,
  width = 8,
  height = 6,
  dpi = 300
)

#-------------------------------------------------
# PART 2:
# K-means cluster analysis
# Same data and same geography as GWR
#-------------------------------------------------

cluster_data <- sea_projected %>%
  st_drop_geometry() %>%
  select(
    median_value,
    median_rooms,
    pct_college,
    pct_foreign_born,
    pct_white,
    median_age,
    median_structure_age,
    percent_ooh,
    pop_density,
    total_population
  ) %>%
  scale()

set.seed(123)

kmeans_result <- kmeans(
  cluster_data,
  centers = 6,
  nstart = 25
)

sea_clusters <- sea_projected %>%
  mutate(
    cluster = as.factor(kmeans_result$cluster)
  )

#-------------------------------------------------
# Figure 2:
# K-means Cluster Map
# Qualitative ColorBrewer palette: Set2
#-------------------------------------------------

cluster_map <- ggplot(sea_clusters) +
  geom_sf(aes(fill = cluster), color = NA) +
  scale_fill_brewer(
    palette = "Set2",
    name = "Cluster"
  ) +
  labs(
    title = "K-Means Cluster Analysis",
    subtitle = "Seattle MSA Census Tracts",
    caption = "Data source: 2023 ACS 5-year estimates"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 8),
    legend.position = "right"
  )

cluster_map

ggsave(
  filename = "figures/figure2_kmeans_cluster_map.png",
  plot = cluster_map,
  device = ragg::agg_png,
  width = 8,
  height = 6,
  dpi = 300
)

#-------------------------------------------------
# Check saved figures
#-------------------------------------------------

list.files("figures")