# Load packages
library(ggplot2)
library(vegan)

# Load your data
data <- read.csv("/Users/adelejoyeux/Downloads/CB24/Carte geomorpho 2024-25/coodSoilSamples.csv")

# Step 1: PCA on the granulometry columns
pca <- rda(data[, c("sand","silt","clay")], scale = TRUE)  # scale = TRUE standardizes variables

# Step 2: Extract PCA scores for samples
scores_samples <- as.data.frame(scores(pca, display = "sites"))
scores_samples$SoilType <- data$SoilType

# Step 3: Extract PCA scores for variables
scores_vars <- as.data.frame(scores(pca, display = "species"))

# Step 4: Make the biplot
ggplot() +
  # Samples
  geom_point(data = scores_samples, aes(x = PC1, y = PC2, color = factor(SoilType)), size = 3) +
  # Variable arrows
  geom_segment(data = scores_vars, aes(x = 0, y = 0, xend = PC1*2, yend = PC2*2), 
               arrow = arrow(length = unit(0.2,"cm")), color = "black") +
  # Variable labels
  geom_text(data = scores_vars, aes(x = PC1*2.2, y = PC2*2.2, label = rownames(scores_vars)),
            color = "black", size = 4) +
  theme_minimal() +
  labs(title = "PCA biplot of granulometric composition",
       color = "Soil Type") +
  coord_equal()

# note : Arrows point in the direction each variable increases. The longer the arrow, the more that variable explains variance.
# Samples near an arrow are high in that variable.
# SoilType colors help visualize group separation.
