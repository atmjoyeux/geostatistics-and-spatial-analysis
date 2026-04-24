# Load packages
library(vegan)
library(ggplot2)

# Example: your data frame "data" has columns:
# Sand, Silt, Clay = numeric granulometry composition
# SoilType = factor (1, 2, 3)

# Load your data
data <- read.csv("/Users/adelejoyeux/Downloads/CB24/Carte geomorpho 2024-25/coodSoilSamples.csv")


# Step 1: Prepare distance matrix
dist_mat <- vegdist(data[, c("sand","silt","clay")], method = "euclidean")

# Step 2: Run NMDS (2 dimensions)
nmds <- metaMDS(dist_mat, k = 2, trymax = 100)

# Step 3: Extract NMDS scores + soil type
scores <- as.data.frame(scores(nmds))
scores$SoilType <- data$SoilType

# Step 4: Plot
ggplot(scores, aes(x = NMDS1, y = NMDS2, color = factor(SoilType))) +
  geom_point(size = 3) +
  stat_ellipse(aes(group = SoilType), linetype = 2) +
  theme_minimal() +
  labs(color = "Soil Type",
       title = "NMDS of granulometric composition",
       subtitle = "Ellipses = group dispersion")
