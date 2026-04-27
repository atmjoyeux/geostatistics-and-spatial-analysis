# ============================================================
# Title: Hierarchical clustering on sediment proxy data
#
# Description:
# This script performs hierarchical clustering on scaled
# sediment proxy variables using Euclidean distance and
# average linkage. It produces dendrograms to visualize
# relationships between samples.
#
# Input:
# - CSV file containing proxy variables
#
# Output:
# - Dendrogram (hierarchical clustering)
# - Unrooted tree representation
#
# Dependencies:
# - stats (base R)
# - ape (for phylogenetic tree visualization)
# ============================================================

# ---- 0. Load packages ----
library(ape)

# ---- 1. Import data ----
# Use relative path for GitHub compatibility
data <- read.csv("data/al-shape-pies_pour_R3.csv")

# ---- 2. Rename columns ----
feature_names <- c(
  "GSM", "LOI", "CaCO3", "ALD", "NSP",
  "LW", "Distribution", "sigma_phy_16", "Texture"
)
colnames(data) <- feature_names

# ---- 3. Explore data ----
str(data)
summary(data)

if (any(is.na(data))) {
  message("Warning: dataset contains missing values")
}

# ---- 4. Scale data ----
data_scaled <- as.data.frame(scale(data))

# ---- 5. Compute distance matrix ----
dist_mat <- dist(
  data_scaled,
  method = "euclidean"
)

# ---- 6. Hierarchical clustering ----
hclust_avg <- hclust(
  dist_mat,
  method = "average"
)

# ---- 7. Plot dendrogram ----
plot(
  hclust_avg,
  hang = -1,
  cex = 0.6,
  main = "Hierarchical clustering (average linkage)"
)

# ---- 8. Cut tree into clusters ----
clusters <- cutree(
  hclust_avg,
  k = 5
)

# Optional: highlight clusters
# rect.hclust(hclust_avg, k = 5, border = 2:6)

# ---- 9. Alternative visualization (unrooted tree) ----
plot(
  as.phylo(hclust_avg),
  type = "unrooted",
  cex = 0.6,
  no.margin = TRUE,
  main = "Unrooted tree representation"
)
