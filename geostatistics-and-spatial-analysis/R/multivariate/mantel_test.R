# ============================================================
# Title: Mantel test on spatial and soil granulometry distances
#
# Description:
# This script performs a Mantel test to assess the correlation
# between spatial distances (based on coordinates) and
# environmental distances (based on soil granulometry:
# sand, silt, clay).
#
# Input:
# - CSV file containing soil sample data with coordinates
#   and granulometric variables
#
# Output:
# - Mantel test results printed in console
#
# Dependencies:
# - vegan
# - readr
# ============================================================

library(vegan)
library(readr)

# ---- 1. Import data ----
# Utilise un chemin relatif si possible (important pour GitHub)
points <- read_csv("data/coodSoilSamples.csv")

# ---- 2. Check required columns ----
required_cols <- c("x", "y", "sand", "silt", "clay")
stopifnot(all(required_cols %in% names(points)))

# ---- 3. Distance matrices ----
coords <- points[, c("x", "y")]
geo_dist <- dist(coords, method = "euclidean")

env_vars <- points[, c("sand", "silt", "clay")]
env_dist <- dist(env_vars, method = "euclidean")

# ---- 4. Mantel test ----
set.seed(123)  # reproductibilité
mantel_result <- mantel(
  geo_dist,
  env_dist,
  method = "pearson",
  permutations = 9999
)

# ---- 5. Output ----
print(mantel_result)

