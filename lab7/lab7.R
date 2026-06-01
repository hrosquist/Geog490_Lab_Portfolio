#-------------------------------------------------
# Lab 7 - Segregation and Diversity
# Hailey Rosquist
# Spring 2026
# Purpose:
# Calculate multigroup segregation and
# location quotient for the Seattle-Tacoma-Bellevue MSA
#-------------------------------------------------

#-------------------------------------------------
# Libraries
#-------------------------------------------------

library(tidyverse)
library(tidycensus)
library(sf)
library(segregation)
library(ggplot2)
library(RColorBrewer)
library(ragg)

#-------------------------------------------------
# Settings
#-------------------------------------------------

setwd("~/Desktop/geog490/lab7")

dir.create("figures", showWarnings = FALSE)

# Remove old figures
file.remove(list.files("figures", full.names = TRUE))

#-------------------------------------------------
# Geography of Interest:
# Seattle-Tacoma-Bellevue MSA
# Counties: King, Pierce, Snohomish
# Data: 2023 ACS 5-year estimates, Census tracts
#-------------------------------------------------

seattle_data <- get_acs(
  geography = "tract",
  variables = c(
    total = "B03002_001",
    white = "B03002_003",
    black = "B03002_004",
    asian = "B03002_006",
    hispanic = "B03002_012"
  ),
  state = "WA",
  county = c("King", "Pierce", "Snohomish"),
  geometry = TRUE,
  year = 2023,
  survey = "acs5",
  output = "wide"
)

#-------------------------------------------------
# Clean data
#-------------------------------------------------

seattle_data <- seattle_data %>%
  rename(
    total = totalE,
    white = whiteE,
    black = blackE,
    asian = asianE,
    hispanic = hispanicE
  ) %>%
  select(
    GEOID,
    NAME,
    total,
    white,
    black,
    asian,
    hispanic,
    geometry
  ) %>%
  filter(total > 0)

#-------------------------------------------------
# Prepare data for multigroup segregation
#-------------------------------------------------

seattle_long <- seattle_data %>%
  st_drop_geometry() %>%
  select(
    GEOID,
    white,
    black,
    asian,
    hispanic
  ) %>%
  pivot_longer(
    cols = c(
      white,
      black,
      asian,
      hispanic
    ),
    names_to = "group",
    values_to = "estimate"
  ) %>%
  filter(
    !is.na(estimate),
    estimate >= 0
  )

#-------------------------------------------------
# 1. Multigroup segregation index
#-------------------------------------------------

total_segregation <- mutual_total(
  data = seattle_long,
  group = "group",
  unit = "GEOID",
  weight = "estimate"
)

print(total_segregation)

write_csv(
  as_tibble(total_segregation),
  "figures/seattle_total_multigroup_segregation.csv"
)

#-------------------------------------------------
# Local segregation scores
#-------------------------------------------------

seattle_local <- mutual_local(
  data = seattle_long,
  group = "group",
  unit = "GEOID",
  weight = "estimate",
  wide = TRUE
)

seattle_map <- seattle_data %>%
  left_join(seattle_local, by = "GEOID")

#-------------------------------------------------
# Figure 1: Local Multigroup Segregation Map
#-------------------------------------------------

seg_map <- ggplot(seattle_map) +
  geom_sf(
    aes(fill = ls),
    color = NA
  ) +
  coord_sf(crs = 26910) +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    name = "Local\nSegregation"
  ) +
  labs(
    title = "Local Multigroup Segregation Index",
    subtitle = "Seattle-Tacoma-Bellevue MSA, 2023 ACS 5-Year Estimates",
    caption = "Data source: U.S. Census Bureau ACS 2023 5-year estimates"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 8),
    legend.position = "right"
  )

seg_map

ggsave(
  filename = "figures/figure1_seattle_msa_segregation.png",
  plot = seg_map,
  device = ragg::agg_png,
  width = 8,
  height = 6,
  dpi = 300
)

#-------------------------------------------------
# 2. Asian Location Quotient
#-------------------------------------------------

total_asian <- sum(seattle_map$asian, na.rm = TRUE)
total_pop <- sum(seattle_map$total, na.rm = TRUE)

seattle_lq <- seattle_map %>%
  mutate(
    asian_lq =
      (asian / total) /
      (total_asian / total_pop),
    asian_lq = ifelse(
      is.infinite(asian_lq),
      NA,
      asian_lq
    )
  )

#-------------------------------------------------
# Figure 2: Asian Location Quotient Map
#-------------------------------------------------

lq_map <- ggplot(seattle_lq) +
  geom_sf(
    aes(fill = asian_lq),
    color = NA
  ) +
  coord_sf(crs = 26910) +
  scale_fill_distiller(
    palette = "PuBuGn",
    direction = 1,
    name = "Asian\nLQ"
  ) +
  labs(
    title = "Asian Location Quotient",
    subtitle = "Seattle-Tacoma-Bellevue MSA, 2023 ACS 5-Year Estimates",
    caption = "Data source: U.S. Census Bureau ACS 2023 5-year estimates"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 8),
    legend.position = "right"
  )

lq_map

ggsave(
  filename = "figures/figure2_seattle_msa_asian_lq.png",
  plot = lq_map,
  device = ragg::agg_png,
  width = 8,
  height = 6,
  dpi = 300
)

#-------------------------------------------------
# Check saved figures
#-------------------------------------------------

list.files("figures")