#-------------------------------------------------
# Lab 5: Spatial Analysis and Modeling in R
# Hailey Rosquist
# Purpose: spatial join, erase_water, and hotspot analysis
#-------------------------------------------------

library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(spdep)
library(crsuggest)
library(scales)
library(viridis)

options(tigris_use_cache = TRUE)

#-------------------------------------------------
# Set working directory to Lab 5 folder
#-------------------------------------------------

setwd("~/Desktop/geog490/lab5")

# Create figures folder
dir.create("figures", showWarnings = FALSE)

#-------------------------------------------------
# PART 1: Cross-state metro area
# Portland-Vancouver-Hillsboro, OR-WA
#-------------------------------------------------

or_tracts <- tracts(
  state = "OR",
  cb = TRUE,
  year = 2020
)

wa_tracts <- tracts(
  state = "WA",
  cb = TRUE,
  year = 2020
)

all_tracts <- bind_rows(or_tracts, wa_tracts)

portland_metro <- core_based_statistical_areas(
  cb = TRUE,
  year = 2020
) %>%
  filter(str_detect(NAME, "Portland-Vancouver-Hillsboro"))

# Suggested CRS
suggest_crs(portland_metro)

# Oregon Statewide Lambert, good for Oregon/Washington metro area
all_tracts <- st_transform(all_tracts, 2913)
portland_metro <- st_transform(portland_metro, 2913)

portland_tracts <- all_tracts %>%
  st_filter(portland_metro, .predicate = st_intersects)

# Number of census tracts
nrow(portland_tracts)

#-------------------------------------------------
# Figure 1: Portland-Vancouver metro census tracts
#-------------------------------------------------

figure1 <- ggplot() +
  geom_sf(
    data = portland_tracts,
    aes(fill = STATEFP),
    color = "white",
    size = 0.08
  ) +
  geom_sf(
    data = portland_metro,
    fill = NA,
    color = "black",
    linewidth = 0.9
  ) +
  scale_fill_manual(
    values = c(
      "41" = "darkseagreen3",
      "53" = "lightsteelblue"
    ),
    labels = c(
      "41" = "Oregon tracts",
      "53" = "Washington tracts"
    ),
    name = "State"
  ) +
  coord_sf(expand = TRUE) +
  theme_void() +
  labs(
    title = "Census Tracts in the Portland-Vancouver-Hillsboro Metro Area",
    subtitle = "A cross-state metropolitan area spanning Oregon and Washington",
    caption = "Data: 2020 TIGER/Line Census Tracts and CBSA boundaries"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold", margin = margin(b = 8)),
    plot.subtitle = element_text(size = 12, margin = margin(b = 14)),
    plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 12)),
    legend.position = "right",
    plot.margin = margin(25, 45, 25, 35)
  )

figure1

ggsave(
  filename = "figures/figure1_portland_vancouver_metro_tracts.png",
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
)

# Suggested CRS
suggest_crs(king_income)

# Washington North State Plane
king_income <- king_income %>%
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
# ACS 2020 5-year, table B19013
# Seattle-Tacoma-Bellevue metro area
#-------------------------------------------------

wa_tracts_income <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "WA",
  county = c("King", "Pierce", "Snohomish"),
  year = 2020,
  geometry = TRUE
)

seattle_metro <- core_based_statistical_areas(
  cb = TRUE,
  year = 2020
) %>%
  filter(str_detect(NAME, "Seattle-Tacoma-Bellevue")) %>%
  st_transform(2926)

seattle_income <- wa_tracts_income %>%
  st_transform(2926) %>%
  st_filter(seattle_metro, .predicate = st_intersects) %>%
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
      "High income cluster" = "firebrick2",
      "Low income cluster" = "steelblue3",
      "Not significant" = "grey85"
    ),
    name = "Cluster type"
  ) +
  coord_sf(expand = TRUE) +
  theme_void() +
  labs(
    title = "Getis-Ord Hotspot Analysis of Median Household Income",
    subtitle = "Seattle-Tacoma-Bellevue metro area, ACS 2020 5-year estimates",
    caption = "Hot and cold spots are based on local Getis-Ord Gi* statistics"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold", margin = margin(b = 8)),
    plot.subtitle = element_text(size = 12, margin = margin(b = 14)),
    plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 12)),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
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

# Check exported figures
list.files("figures")