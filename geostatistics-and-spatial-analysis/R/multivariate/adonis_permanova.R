library(vegan)

# Load your data
data <- read.csv("/Users/adelejoyeux/Downloads/CB24/Carte geomorpho 2024-25/coodSoilSamples.csv")

# Select your granulometric variables
vars <- data[, c("sand", "silt", "clay")]

# Convert soil type to factor
data$SoilType <- as.factor(data$SoilType)  # column with 1, 2, 3

# Compute distance matrix
dist_mat <- vegdist(vars, method = "euclidean")

# Run PERMANOVA
set.seed(123)
adonis_res <- adonis2(dist_mat ~ SoilType, data = data, permutations = 9999)

# Show results
print(adonis_res)
