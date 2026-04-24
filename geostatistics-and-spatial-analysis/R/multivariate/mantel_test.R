# Install required packages if needed
# install.packages(c("vegan", "readxl"))

library(vegan)
library(readxl)

# Import Excel file
points <- read.csv("/Users/adelejoyeux/Downloads/CB24/Carte geomorpho 2024-25/coodSoilSamples.csv")   # or read_excel("your_file.xlsx") 

# Step 1: Geographic distance matrix
coords <- as.matrix(points[, c("x", "y")])   # adapt column names
geo_dist <- dist(coords)

#  Step 2: Environmental distance with 3 variables (sand, silt, clay)
env_matrix <- as.matrix(points[, c("sand", "silt", "clay")])
env_dist <- dist(env_matrix, method = "euclidean")   # or "manhattan"

# Step 3: Mantel test
mantel_result <- mantel(geo_dist, env_dist, method = "pearson", permutations = 9999)
print(mantel_result)

