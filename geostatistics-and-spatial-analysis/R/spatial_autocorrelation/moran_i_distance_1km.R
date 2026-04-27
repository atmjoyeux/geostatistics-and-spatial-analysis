# ============================================================
# Title: Moran's I spatial autocorrelation analysis
#
# Description:
# This script computes Moran's I statistic to assess spatial
# autocorrelation for multiple environmental and soil variables
# using a distance-based neighborhood structure.
#
# Input:
# - Shapefile containing point data with associated variables
#
# Output:
# - Table of Moran's I statistics and p-values
# - CSV file with results
#
# Dependencies:
# - sf
# - spdep
# ============================================================

# ---- 0. Load packages ----
library(sf)
library(spdep)

# ---- 1. Import spatial data ----
# Use relative path for GitHub compatibility
points <- st_read("data/coodSoilSamples.shp")

# ---- 2. Create spatial weights (distance-based) ----
coords <- st_coordinates(points)

# Define neighborhood within a given distance (in map units, e.g., meters)
nb <- dnearneigh(coords, 0, 1000)

# Convert to spatial weights list
lw <- nb2listw(nb, style = "W")

# ---- 3. Define variables to test ----
vars <- c(
  "ALD", "spericity", "LOI", "CACO3",
  "humidity", "morpho", "Sand", "Silt", "Clay", "Soil"
)

# ---- 4. Initialize results ----
results <- data.frame()

# ---- 5. Loop through variables ----
for (v in vars) {
  
  # Convert variable to numeric
  vec <- as.numeric(points[[v]])
  
  # Check for missing values
  if (any(is.na(vec))) {
    message(paste("Warning:", v, "contains NA values (ignored in analysis)"))
  }
  
  # Moran's I test with permutations
  mc <- moran.mc(
    vec,
    lw,
    nsim = 9999
  )
  
  # Store results
  results <- rbind(results, data.frame(
    Variable = v,
    Moran_I = as.numeric(mc$statistic),
    P_value = mc$p.value
  ))
}

# ---- 6. Output ----
print(results)

# Save results to CSV
write.csv(
  results,
  file = "outputs/moran_results.csv",
  row.names = FALSE
)  
  # Moran's test with permutation
  mc <- moran.mc(vec, lw, nsim = 9999)
  
  # store the results
  res <- data.frame(
    variable = v,
    moran_I = mc$statistic,
    p_value = mc$p.value
  )
  results <- rbind(results, res)
}

# export CSV
write.csv(results, "moran_results.csv", row.names = FALSE)

print(results)
