# ============================================================
# Title: NMDS analysis on soil granulometry
#
# Description:
# This script performs a Non-metric Multidimensional Scaling (NMDS)
# to visualize differences in soil granulometric composition
# (sand, silt, clay) between soil types.
#
# Input:
# - CSV file containing soil sample data with:
#   coordinates and granulometric variables
#
# Output:
# - NMDS plot with points colored by soil type
# - Ellipses representing group dispersion
#
# Dependencies:
# - vegan
# - ggplot2
# ============================================================

# ---- 0. Load packages ----
library(vegan)
library(ggplot2)
library(readr)

# ---- 1. Import data ----
# Use relative path for GitHub compatibility
data <- read_csv("data/coodSoilSamples.csv")

# ---- 2. Check required columns ----
required_cols <- c("sand", "silt", "clay", "SoilType")
stopifnot(all(required_cols %in% names(data)))

# ---- 3. Compute distance matrix ----
dist_mat <- vegdist(
  data[, c("sand", "silt", "clay")],
  method = "euclidean"
)

# ---- 4. Run NMDS (2 dimensions) ----
set.seed(123)  # reproducibility
nmds <- metaMDS(
  dist_mat,
  k = 2,
  trymax = 100
)

# ---- 5. Extract scores ----
scores_df <- as.data.frame(scores(nmds))
scores_df$SoilType <- data$SoilType

# ---- 6. Plot NMDS ----
ggplot(scores_df, aes(x = NMDS1, y = NMDS2, color = factor(SoilType))) +
  geom_point(size = 3) +
  stat_ellipse(aes(group = SoilType), linetype = 2) +
  theme_minimal() +
  labs(
    color = "Soil Type",
    title = "NMDS of granulometric composition",
    subtitle = "Ellipses represent group dispersion"
  )
