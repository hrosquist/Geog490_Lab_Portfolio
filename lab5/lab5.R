#-------------------------------------------------
# Lab 5: Spatial Analysis and Modeling in R
# Hailey Rosquist
# Purpose: spatial join, erase_water, and hotspot analysis
# Seattle, WA
#-------------------------------------------------

library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(spdep)
library(crsuggest)
library(scales)

options(tigris_use_cache = TRUE)

#-------------------------------------------------
# Set working directory to Lab 5 folder
#-------------------------------------------------

setwd("~/Desktop/geog490/lab5")

# Create figures folder inside Lab 5 folder
dir.create("figures", showWarnings = FALSE)

#-------------------------------------------------
# PART 1: Cross-state metro area
# Seattle-Tacoma-Bellevue, WA
#-------------------------------------------------

wa_tracts <- tracts(
  state = "WA",
  cb = TRUE,
  year = 2020
)

seattle_metro <- core_based_statistical_areas(
  cb = TRUE,
  year = 2020
) %>%
  filter(str_detect(NAME, "Seattle-Tacoma-Bellevue"))

# Find suggested CRS
suggest_crs(seattle_metro)

# Use Washington North State Plane
wa_tracts <- st_transform(wa_tracts, 2926)
seattle_metro <- st_transform(seattle_metro, 2926)

seattle_tracts <- wa_tracts %>%
  st_filter(seattle_metro, .predicate = st_within)

nrow(seattle_tracts)

#-------------------------------------------------
# Figure 1: Seattle metro census tracts
#-------------------------------------------------

figure1 <- ggplot() +
  geom_sf(data = wa_tracts, fill = "grey95", color = "white", size = 0.05) +
  geom_sf(data = seattle_tracts, fill = "lightblue", color = "white", size = 0.08) +
  geom_sf(data = seattle_metro, fill = NA, color = "red", linewidth = 0.8) +
  coord_sf(expand = TRUE) +
  theme_void() +
  labs(
    title = "Census Tracts in the Seattle-Tacoma-Bellevue Metro Area",
    subtitle = "Seattle metropolitan region shown within Washington",
    caption = "Data: 2020 TIGER/Line Census Tracts and CBSA boundaries"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold", margin = margin(b = 8)),
    plot.subtitle = element_text(size = 12, margin = margin(b = 14)),
    plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 12)),
    plot.margin = margin(25, 35, 25, 35)
  )

figure1

ggsave(
  filename = "figures/figure1_seattle_metro_tracts.png",
  plot = figure1,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white"
)

#-------------------------------------------------
# PART 2: erase_water() workflow
# King County, Washington
#-------------------------------------------------

king_income <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "WA",
  county = "King",
  year = 2020,
  geometry = TRUE,
  cb = FALSE
) %>%
  st_transform(2926)

king_income_erase <- erase_water(
  king_income,
  area_threshold = 0.80
)

#-------------------------------------------------
# Figure 2: King County income with water erased
#-------------------------------------------------

figure2 <- ggplot(king_income_erase) +
  geom_sf(aes(fill = estimate), color = NA) +
  scale_fill_viridis_c(
    labels = scales::label_dollar(),
    name = "Median household\nincome"
  ) +
  coord_sf(expand = TRUE) +
  theme_void() +
  labs(
    title = "Median Household Income by Census Tract",
    subtitle = "King County, Washington, with major water areas erased",
    caption = "Data: ACS 2020 5-year estimates, table B19013"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold", margin = margin(b = 8)),
    plot.subtitle = element_text(size = 12, margin = margin(b = 14)),
    plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 12)),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.margin = margin(l = 15),
    plot.margin = margin(25, 45, 25, 35)
  )

figure2

ggsave(
  filename = "figures/figure2_king_county_erase_water.png",
  plot = figure2,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white"
)

#-------------------------------------------------
# PART 3: Getis-Ord hotspot analysis
# Variable: Median household income
# 2020 ACS 5-year
#-------------------------------------------------

seattle_income <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "WA",
  county = c("King", "Pierce", "Snohomish"),
  year = 2020,
  geometry = TRUE
) %>%
  st_transform(2926) %>%
  st_filter(seattle_metro, .predicate = st_within) %>%
  filter(!is.na(estimate))

neighbors <- poly2nb(
  seattle_income,
  queen = TRUE
)

localg_weights <- nb2listw(
  include.self(neighbors),
  style = "W",
  zero.policy = TRUE
)

seattle_income$localG <- localG(
  seattle_income$estimate,
  localg_weights,
  zero.policy = TRUE
)

seattle_income <- seattle_income %>%
  mutate(
    hotspot = case_when(
      localG >= 2.56 ~ "High income cluster",
      localG <= -2.56 ~ "Low income cluster",
      TRUE ~ "Not significant"
    )
  )

#-------------------------------------------------
# Figure 3: Income hotspot analysis
#-------------------------------------------------

figure3 <- ggplot(seattle_income) +
  geom_sf(aes(fill = hotspot), color = "grey80", size = 0.05) +
  scale_fill_manual(
    values = c(
      "High income cluster" = "red",
      "Low income cluster" = "blue",
      "Not significant" = "grey85"
    ),
    name = "Cluster type"
  ) +
  coord_sf(expand = TRUE) +
  theme_void() +
  labs(
    title = "Getis-Ord Hotspot Analysis of Median Household Income",
    subtitle = "Seattle-Tacoma-Bellevue metro area, 2020 ACS 5-year estimates",
    caption = "Hot and cold spots are based on local Getis-Ord Gi* statistics"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold", margin = margin(b = 8)),
    plot.subtitle = element_text(size = 12, margin = margin(b = 14)),
    plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 12)),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.margin = margin(l = 15),
    plot.margin = margin(25, 45, 25, 35)
  )

figure3

ggsave(
  filename = "figures/figure3_seattle_income_hotspots.png",
  plot = figure3,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white"
)

list.files("figures")