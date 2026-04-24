#install.packages("sf")
#install.packages("spdep")


library(sf)
library(spdep)


# loading a local shapefile
points <- st_read( "/Users/adelejoyeux/Downloads/CB24/Carte geomorpho 2024-25/coodSoilSamples.shp")

# Neighborhood by distance
coords <- st_coordinates(points)
nb <- dnearneigh(coords, 0, 1000)  # ici 50 km de rayon
lw <- nb2listw(nb, style="W")

# compute Moran I
#moran.mc(points$ALD, lw, nsim = 999)

library(spdep)

# list of variables to test
vars <- c("ALD", "spericity", "LOI", "CACO3", "humidity", "morpho", "Sand", "Silt", "Clay", "Soil")  # <-- adapt with your column names

# dataframe to store the results
results <- data.frame()

# loop over each variable
for (v in vars) {
  # explicitly convert to numeric
  vec <- as.numeric(points[[v]])
  
  # optional: warn if there are missing values
  if (any(is.na(vec))) {
    cat("⚠️ Attention :", v, "contient des NA (ils seront ignorés)\n")
  }
  
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