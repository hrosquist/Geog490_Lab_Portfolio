#-------------------------------------------------
# Lab 7 - Segregation and Diversity
# Hailey Rosquist
# Spring 2026
# Purpose:
# Calculate multigroup segregation and
# location quotient for a selected MSA
#-------------------------------------------------

library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(segregation)
library(tmap)
library(rmapshaper)
library(RColorBrewer)

#-------------------------------------------------
# Create figures folder
#-------------------------------------------------

dir.create("figures", showWarnings = FALSE)

#-------------------------------------------------
# Choose geography: Seattle area using King County, WA
#-------------------------------------------------

seattle_data <- get_acs(
  geography = "tract",
  variables = c(
    white = "B03002_003",
    black = "B03002_004",
    asian = "B03002_006",
    hispanic = "B03002_012"
  ),
  state = "WA",
  county = "King",
  geometry = TRUE,
  year = 2023,
  output = "wide"
)

seattle_data <- seattle_data %>%
  rename(
    white = whiteE,
    black = blackE,
    asian = asianE,
    hispanic = hispanicE
  ) %>%
  select(GEOID, white, black, asian, hispanic, geometry)

#-------------------------------------------------
# Prepare data for segregation analysis
#-------------------------------------------------

seattle_long <- seattle_data %>%
  st_drop_geometry() %>%
  pivot_longer(
    cols = c(white, black, asian, hispanic),
    names_to = "variable",
    values_to = "estimate"
  )

#-------------------------------------------------
# Calculate total multigroup segregation
#-------------------------------------------------

mutual_total(
  data = seattle_long,
  group = "variable",
  unit = "GEOID",
  weight = "estimate"
)

#-------------------------------------------------
# Calculate local multigroup segregation
#-------------------------------------------------

seattle_local <- mutual_local(
  data = seattle_long,
  group = "variable",
  unit = "GEOID",
  weight = "estimate",
  wide = TRUE
)

seattle_map <- seattle_data %>%
  left_join(seattle_local, by = "GEOID")

#-------------------------------------------------
# Map local multigroup segregation
#-------------------------------------------------

tmap_mode("plot")

seg_map <- tm_shape(seattle_map) +
  tm_polygons(
    fill = "ls",
    fill.scale = tm_scale(
      style = "quantile",
      values = "YlOrRd"
    ),
    col_alpha = 0
  ) +
  tm_title(
    "Local Multigroup Segregation Index\nKing County, WA"
  ) +
  tm_layout(
    frame = FALSE,
    inner.margins = c(0.08, 0.08, 0.12, 0.08)
  )

seg_map

# Save segregation map without using cairo/X11
png(
  filename = "figures/seattle_segregation_map.png",
  width = 8,
  height = 6,
  units = "in",
  res = 300,
  type = "quartz"
)

seg_map

dev.off()

#-------------------------------------------------
# Calculate Asian location quotient
#-------------------------------------------------

seattle_lq <- seattle_data %>%
  mutate(
    tract_total = white + black + asian + hispanic
  )

total_asian <- sum(seattle_lq$asian, na.rm = TRUE)
total_pop <- sum(seattle_lq$tract_total, na.rm = TRUE)

seattle_lq <- seattle_lq %>%
  mutate(
    asian_lq =
      (asian / tract_total) /
      (total_asian / total_pop)
  )

#-------------------------------------------------
# Map Asian location quotient
#-------------------------------------------------

lq_map <- tm_shape(seattle_lq) +
  tm_polygons(
    fill = "asian_lq",
    fill.scale = tm_scale(
      style = "quantile",
      values = "PuBuGn"
    ),
    col_alpha = 0
  ) +
  tm_title(
    "Asian Location Quotient\nKing County, WA"
  ) +
  tm_layout(
    frame = FALSE,
    inner.margins = c(0.08, 0.08, 0.12, 0.08)
  )

lq_map

# Save location quotient map without using cairo/X11
png(
  filename = "figures/seattle_asian_lq_map.png",
  width = 8,
  height = 6,
  units = "in",
  res = 300,
  type = "quartz"
)

lq_map

dev.off()