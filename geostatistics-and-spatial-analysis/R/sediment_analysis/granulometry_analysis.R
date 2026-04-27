# ============================================================
# Title: Grain size analysis and Folk & Ward parameters
#
# Description:
# This script processes multiple CSV files containing grain size
# distribution data and computes:
# - Percentiles (Folk & Ward method)
# - Statistical parameters (mean, sorting, skewness, kurtosis)
# - Sand, silt, and clay proportions
#
# Input:
# - Folder containing CSV files with grain size distributions
#
# Output:
# - Aggregated table of granulometric parameters
# - CSV file with computed results
#
# Dependencies:
# - dplyr
# ============================================================

# ---- 0. Load packages ----
library(dplyr)

# ---- 1. Define input/output paths ----
# Use relative paths for GitHub compatibili
folder_path <- "data/grain_size_csv"
output_file <- "outputs/granulometry_results.csv"

# ---- 2. List CSV files ----
files <- list.files(
  path = folder_path,
  pattern = "*.csv",
  full.names = TRUE
)

# ---- 3. Interpolation functions ----
interp_x_at_y <- function(x, y, target_y) {
  approx(y, x, xout = target_y, ties = mean)$y
}

interp_y_at_x <- function(x, y, target_x) {
  approx(x, y, xout = target_x, ties = mean)$y
}

# ---- 4. Initialize results ----
results <- data.frame()

# ---- 5. Loop through files ----
for (i in seq_along(files)) {
  
  data <- read.csv(files[i], header = TRUE, sep = ";")
  
  # Extract and clean columns (grain size and cumulative %)
  grain <- as.numeric(gsub(",", ".", gsub("[^0-9.,]", "", data[[3]]))) / 1000  # µm → mm
  perc  <- as.numeric(gsub(",", ".", gsub("[^0-9.,]", "", data[[4]])))
  
  # Remove invalid values
  valid <- !is.na(grain) & !is.na(perc)
  grain <- grain[valid]
  perc  <- perc[valid]
  
  # Sort values (required for interpolation)
  ord <- order(grain)
  grain <- grain[ord]
  perc  <- perc[ord]
  
  # Convert to phi scale
  phi <- -log2(grain)
  
  # ---- Percentiles (Folk & Ward) ----
  phi5  <- interp_x_at_y(phi, perc, 5)
  phi16 <- interp_x_at_y(phi, perc, 16)
  phi25 <- interp_x_at_y(phi, perc, 25)
  phi50 <- interp_x_at_y(phi, perc, 50)
  phi75 <- interp_x_at_y(phi, perc, 75)
  phi84 <- interp_x_at_y(phi, perc, 84)
  phi95 <- interp_x_at_y(phi, perc, 95)
  
  # ---- Folk & Ward parameters ----
  mean_phi <- (phi16 + phi50 + phi84) / 3
  
  sorting <- (phi84 - phi16)/4 + (phi95 - phi5)/6.6
  
  skewness <- ((phi16 + phi84 - 2*phi50)/(2*(phi84 - phi16))) +
    ((phi5 + phi95 - 2*phi50)/(2*(phi95 - phi5)))
  
  kurtosis <- (phi95 - phi5) / (2.44 * (phi75 - phi25))
  
  # ---- Sand / Silt / Clay proportions ----
  P63 <- interp_y_at_x(grain, perc, 0.063)  # % finer than 63 µm
  
  clay <- interp_y_at_x(grain, perc, 0.002)
  silt <- P63 - clay
  sand <- 100 - P63
  
  # ---- Sample name ----
  sample_name <- tools::file_path_sans_ext(basename(files[i]))
  
  # ---- Store results ----
  results <- rbind(results, data.frame(
    Name = sample_name,
    Sand = sand,
    Silt = silt,
    Clay = clay,
    Phi5 = phi5,
    Phi16 = phi16,
    Phi50 = phi50,
    Phi84 = phi84,
    Phi95 = phi95,
    Mean = mean_phi,
    Sorting = sorting,
    Skewness = skewness,
    Kurtosis = kurtosis
  ))
}

# ---- 6. Output ----
print(results)

# Save results to CSV
write.csv(results, file = output_file, row.names = FALSE)  valid <- !is.na(grain) & !is.na(perc)
  grain <- grain[valid]
  perc  <- perc[valid]
  
  # Trier (important pour approx)
  ord <- order(grain)
  grain <- grain[ord]
  perc  <- perc[ord]
  
  # Passage en phi
  phi <- -log2(grain)
  
  # =========================
  # Percentiles (Folk & Ward)
  # =========================
  phi5  <- interp_x_at_y(phi, perc, 5)
  phi16 <- interp_x_at_y(phi, perc, 16)
  phi25 <- interp_x_at_y(phi, perc, 25)
  phi50 <- interp_x_at_y(phi, perc, 50)
  phi75 <- interp_x_at_y(phi, perc, 75)
  phi84 <- interp_x_at_y(phi, perc, 84)
  phi95 <- interp_x_at_y(phi, perc, 95)
  
  # =========================
  # Paramètres Folk & Ward
  # =========================
  mean_phi <- (phi16 + phi50 + phi84) / 3
  
  sorting <- (phi84 - phi16)/4 + (phi95 - phi5)/6.6
  
  skewness <- ((phi16 + phi84 - 2*phi50)/(2*(phi84 - phi16))) +
    ((phi5 + phi95 - 2*phi50)/(2*(phi95 - phi5)))
  
  kurtosis <- (phi95 - phi5) / (2.44 * (phi75 - phi25))  
  
  # =========================
  # Sand / Silt / Clay
  # =========================
  
  P63 <- interp_y_at_x(grain, perc, 0.063)  # % finer que 63 µm
  
  clay <- interp_y_at_x(grain, perc, 0.002)
  silt <- P63 - clay
  sand <- 100 - P63
  
# ******************************************************************************
# 6. Création d'un tableau final des résultats ---------------------------------
# ******************************************************************************
  
  
  results <- rbind(results, data.frame(
    Name = sample_names[[i]],
    Sand = sand,
    Silt = silt,
    Clay = clay,
    Phi5 = phi5,
    Phi16 = phi16,
    Phi50 = phi50,
    Phi84 = phi84,
    Phi95 = phi95,
    Mean = mean_phi,
    Sorting = sorting,
    Skewness = skewness
  ))
}

print(results)


# ******************************************************************************
# 7. Enregistrement des résultats dans un fichier CSV agrégé -------------------
# ******************************************************************************

write.csv(results, file = "/Users/adelejoyeux/Downloads/L10_CSV/L10-Granulo-Adele.csv", row.names = FALSE)
