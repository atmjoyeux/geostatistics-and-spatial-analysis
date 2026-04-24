# ============================================================
# Title: PERMANOVA analysis on soil granulometry
#
# Description:
# This script performs a PERMANOVA (Adonis) test to assess
# differences in granulometric composition (sand, silt, clay)
# between soil types.
#
# Input:
# - CSV file containing soil sample data
#
# Output:
# - PERMANOVA results printed in console
#
# Dependencies:
# - vegan
# ============================================================

# Author: Adele Joyeux
# Context: PhD research - environmental spatial analysis

# -------------------------
# LIBRARIES
# -------------------------
library(vegan)

# -------------------------
# PARAMETERS
# -------------------------
data_file <- "/path/to/your/data.csv"

# -------------------------
# LOAD DATA
# -------------------------
data <- read.csv(data_file)

# -------------------------
# DATA PREPARATION
# -------------------------

# Select granulometric variables
granulo_vars <- data[, c("sand", "silt", "clay")]

# Convert soil type to factor
data$SoilType <- as.factor(data$SoilType)

# -------------------------
# ANALYSIS
# -------------------------

# Compute distance matrix (Euclidean distance)
dist_mat <- vegdist(granulo_vars, method = "euclidean")

# Run PERMANOVA
set.seed(123)
adonis_res <- adonis2(dist_mat ~ SoilType, data = data, permutations = 9999)

# -------------------------
# OUTPUT
# -------------------------
print(adonis_res)
