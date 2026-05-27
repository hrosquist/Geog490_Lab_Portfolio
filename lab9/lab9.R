#-------------------------------------------------
# Lab 9: Introduction to Regression
# Hailey Rosquist
# Purpose: Explore linear regression, log transformations,
# and principal component analysis using census data in R.
# Seattle, Washington
#-------------------------------------------------

#-------------------------------------------------
# Libraries

library(tidyverse)
library(tidycensus)
library(sf)
library(ggplot2)
library(patchwork)
library(tmap)

options(tigris_use_cache = TRUE)

#-------------------------------------------------
# Create folder for exported figures

dir.create("figures", showWarnings = FALSE)

#-------------------------------------------------
# PART 1: Linear regression with GDP data
# Data from Llaudet & Imai textbook

# Read in data
co <- read.csv(
  "https://raw.githubusercontent.com/ellaudet/DSS/master/countries.csv"
)

# View first observations
head(co)

#-------------------------------------------------
# Create log-transformed variables

co$log_gdp <- log(co$gdp)
co$log_prior_gdp <- log(co$prior_gdp)

#-------------------------------------------------
# Histograms of original and transformed variables

hist_gdp <- ggplot(co, aes(x = gdp)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of GDP",
    x = "GDP",
    y = "Frequency"
  ) +
  theme_minimal()

hist_log_gdp <- ggplot(co, aes(x = log_gdp)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of Logged GDP",
    x = "Logged GDP",
    y = "Frequency"
  ) +
  theme_minimal()

#-------------------------------------------------
# Scatterplot with regression line

fit <- lm(log_gdp ~ log_prior_gdp, data = co)

scatter_plot <- ggplot(
  co,
  aes(x = log_prior_gdp, y = log_gdp)
) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Logged GDP vs Logged Prior GDP",
    x = "Logged Prior GDP",
    y = "Logged GDP"
  ) +
  theme_minimal()

#-------------------------------------------------
# Combine plots with patchwork

gdp_combined <- hist_log_gdp + scatter_plot

# Display figure
gdp_combined

# Save figure
ggsave(
  filename = "figures/logged_gdp_analysis.png",
  plot = gdp_combined,
  width = 12,
  height = 6,
  dpi = 300
)

#-------------------------------------------------
# PART 2: Principal Component Analysis
# Seattle census tract socioeconomic variables

sea <- get_acs(
  geography = "tract",
  variables = c(
    income = "B19013_001",
    poverty = "B17001_002",
    poverty_total = "B17001_001",
    college = "B15003_022",
    college_total = "B15003_001",
    unemployed = "B23025_005",
    labor_force = "B23025_003"
  ),
  state = "WA",
  county = "King",
  year = 2020,
  geometry = TRUE
)

#-------------------------------------------------
# Reshape ACS data

sea_wide <- sea %>%
  st_drop_geometry() %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(
    names_from = variable,
    values_from = estimate
  )

#-------------------------------------------------
# Create percentages and rates

sea_wide <- sea_wide %>%
  mutate(
    poverty_rate = 100 * poverty / poverty_total,
    college_rate = 100 * college / college_total,
    unemployment_rate = 100 * unemployed / labor_force
  )

#-------------------------------------------------
# PCA variables

pca_vars <- sea_wide %>%
  select(
    income,
    poverty_rate,
    college_rate,
    unemployment_rate
  ) %>%
  na.omit()

#-------------------------------------------------
# Run PCA

sea_pca <- prcomp(
  pca_vars,
  scale. = TRUE
)

# View summary
summary(sea_pca)

#-------------------------------------------------
# Scree plot for first five principal components

pca_df <- data.frame(
  PC = paste0("PC", 1:5),
  Variance = sea_pca$sdev[1:5]^2
)

pca_plot <- ggplot(pca_df, aes(x = PC, y = Variance)) +
  geom_col() +
  labs(
    title = "Variance Explained by Principal Components",
    x = "Principal Component",
    y = "Variance"
  ) +
  theme_minimal()

#-------------------------------------------------
# Add PCA scores back to spatial data

sea_scores <- as.data.frame(sea_pca$x)

sea_map <- sea %>%
  distinct(GEOID, geometry) %>%
  left_join(
    sea_wide %>%
      select(GEOID),
    by = "GEOID"
  ) %>%
  bind_cols(sea_scores)

#-------------------------------------------------
# Map Principal Component 1

pc1_map <- ggplot(sea_map) +
  geom_sf(aes(fill = PC1), color = NA) +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = "Principal Component 1",
    subtitle = "Seattle Census Tracts",
    fill = "PC1"
  ) +
  theme_minimal()

#-------------------------------------------------
# Combine PCA plots

pca_combined <- pca_plot + pc1_map

# Display figure
pca_combined

# Save figure
ggsave(
  filename = "figures/pca_analysis.png",
  plot = pca_combined,
  width = 12,
  height = 6,
  dpi = 300
)