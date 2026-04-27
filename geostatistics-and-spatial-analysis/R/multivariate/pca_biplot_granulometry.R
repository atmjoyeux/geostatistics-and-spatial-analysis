# ============================================================
# Title: PCA analysis on soil granulometry
#
# Description:
# This script performs a Principal Component Analysis (PCA)
# to explore variation in soil granulometric composition
# (sand, silt, clay) and visualize relationships between
# samples and variables using a biplot.
#
# Input:
# - CSV file containing soil sample data with
#   granulometric variables and soil type classification
#
# Output:
# - PCA biplot showing:
#   * samples colored by soil type
#   * variable vectors (sand, silt, clay)
#
# Dependencies:
# - vegan
# - ggplot2
# - readr
# ============================================================

# ---- 0. Load packages ----
library(vegan)
library(ggplot2)
library(readr)
library(grid)  # for arrow()

# ---- 1. Import data ----
# Use relative path for GitHub compatibility
data <- read_csv("data/coodSoilSamples.csv")

# ---- 2. Check required columns ----
required_cols <- c("sand", "silt", "clay", "SoilType")
stopifnot(all(required_cols %in% names(data)))

# ---- 3. Run PCA ----
# scale = TRUE standardizes variables
pca <- rda(
  data[, c("sand", "silt", "clay")],
  scale = TRUE
)

# ---- 4. Extract scores ----
# Samples (sites)
scores_samples <- as.data.frame(scores(pca, display = "sites"))
scores_samples$SoilType <- data$SoilType

# Variables (species)
scores_vars <- as.data.frame(scores(pca, display = "species"))
scores_vars$Variable <- rownames(scores_vars)

# ---- 5. Plot PCA biplot ----
ggplot() +
  # Samples
  geom_point(
    data = scores_samples,
    aes(x = PC1, y = PC2, color = factor(SoilType)),
    size = 3
  ) +
  # Variable arrows
  geom_segment(
    data = scores_vars,
    aes(x = 0, y = 0, xend = PC1 * 2, yend = PC2 * 2),
    arrow = arrow(length = unit(0.2, "cm")),
    color = "black"
  ) +
  # Variable labels
  geom_text(
    data = scores_vars,
    aes(x = PC1 * 2.2, y = PC2 * 2.2, label = Variable),
    color = "black",
    size = 4
  ) +
  theme_minimal() +
  labs(
    title = "PCA biplot of granulometric composition",
    color = "Soil Type"
  ) +
  coord_equal()

# ---- Notes ----
# - Arrows indicate the direction of increasing values for each variable.
# - Longer arrows indicate stronger contribution to variance.
# - Samples located in the direction of an arrow have higher values for that variable.
# - SoilType colors help visualize group structure.
