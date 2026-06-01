#-------------------------------------------------
# Lab 9: Introduction to Regression
# Hailey Rosquist
# Purpose: Explore linear regression, log transformations,
# and principal component analysis using census data in R.
# Seattle, Washington
#-------------------------------------------------

#-------------------------------------------------
# Libraries
#-------------------------------------------------

library(tidyverse)
library(tidycensus)
library(sf)
library(ggplot2)
library(tmap)

options(tigris_use_cache = TRUE)

#-------------------------------------------------
# Create folder for exported figures
#-------------------------------------------------

dir.create("figures", showWarnings = FALSE)

#-------------------------------------------------
# PART 1: Linear regression with GDP data
# Data from Llaudet & Imai textbook
#-------------------------------------------------

co <- read.csv(
  "https://raw.githubusercontent.com/ellaudet/DSS/master/countries.csv"
)

head(co)

#-------------------------------------------------
# Create log-transformed variables
#-------------------------------------------------

co$log_gdp <- log(co$gdp)
co$log_prior_gdp <- log(co$prior_gdp)

#-------------------------------------------------
# Histograms of original and transformed variables
#-------------------------------------------------

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
#-------------------------------------------------

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

# Display individual GDP plots
hist_gdp
hist_log_gdp
scatter_plot

# Save individual GDP figures
ggsave(
  filename = "figures/gdp_histogram.png",
  plot = hist_gdp,
  width = 7,
  height = 5,
  dpi = 300
)

ggsave(
  filename = "figures/logged_gdp_histogram.png",
  plot = hist_log_gdp,
  width = 7,
  height = 5,
  dpi = 300
)

ggsave(
  filename = "figures/logged_gdp_scatterplot.png",
  plot = scatter_plot,
  width = 7,
  height = 5,
  dpi = 300
)

#-------------------------------------------------
# PART 2: Principal Component Analysis
# Seattle census tract socioeconomic variables
#-------------------------------------------------

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
#-------------------------------------------------

sea_wide <- sea %>%
  st_drop_geometry() %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(
    names_from = variable,
    values_from = estimate
  )

#-------------------------------------------------
# Create percentages and rates
#-------------------------------------------------

sea_wide <- sea_wide %>%
  mutate(
    poverty_rate = 100 * poverty / poverty_total,
    college_rate = 100 * college / college_total,
    unemployment_rate = 100 * unemployed / labor_force
  )

#-------------------------------------------------
# PCA variables
#-------------------------------------------------

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
#-------------------------------------------------

sea_pca <- prcomp(
  pca_vars,
  scale. = TRUE
)

summary(sea_pca)

#-------------------------------------------------
# Scree plot for first principal components
#-------------------------------------------------

pca_df <- data.frame(
  PC = paste0("PC", seq_along(sea_pca$sdev)),
  Variance = sea_pca$sdev^2
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
#-------------------------------------------------

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
#-------------------------------------------------

pc1_map <- ggplot(sea_map) +
  geom_sf(aes(fill = PC1), color = NA) +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = "Principal Component 1",
    subtitle = "Seattle Census Tracts",
    fill = "PC1"
  ) +
  theme_minimal()

# Display PCA plots separately
pca_plot
pc1_map

# Save PCA figures separately
ggsave(
  filename = "figures/pca_scree_plot.png",
  plot = pca_plot,
  width = 7,
  height = 5,
  dpi = 300
)

ggsave(
  filename = "figures/pc1_map.png",
  plot = pc1_map,
  width = 7,
  height = 5,
  dpi = 300
)

#-------------------------------------------------
# Check saved figures
#-------------------------------------------------

list.files("figures")