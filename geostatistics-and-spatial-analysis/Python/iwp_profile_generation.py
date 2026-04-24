import geopandas as gpd
import rasterio
import pandas as pd
import numpy as np
from pathlib import Path

# ============================================================
# Description:
# Generate elevation profiles along line geometries by sampling
# a raster (e.g., DSM) at regular intervals.
#
# Input:
# - Line shapefile
# - Raster file
#
# Output:
# - One CSV file per line with X, Y, distance, and elevation (Z)
# ============================================================

# Author: Adele Joyeux
# Context: PhD research - environmental spatial analysis

# -------------------------
# PARAMETERS
# -------------------------
shp_file = "/path/to/your/lines.shp"
raster_file = "/path/to/your/raster.tif"
output_folder = "/path/to/output/folder/"
spacing = 0.05  # sampling distance (same unit as shapefile)

# Ensure output folder exists
Path(output_folder).mkdir(parents=True, exist_ok=True)

# -------------------------
# LOAD DATA
# -------------------------
lines = gpd.read_file(shp_file)

# -------------------------
# PROCESSING
# -------------------------
with rasterio.open(raster_file) as src:
    for idx, row in lines.iterrows():
        geom = row.geometry

        # Generate regularly spaced points along the line
        line_length = geom.length
        distances = np.arange(0, line_length + spacing, spacing)
        points = [geom.interpolate(d) for d in distances]

        # Extract coordinates
        coords = [(p.x, p.y) for p in points]

        # Sample raster values
        values = [val[0] for val in src.sample(coords)]

        # Create DataFrame
        df = pd.DataFrame({
            "line_id": idx,
            "point_id": range(len(points)),
            "distance": distances,
            "x": [p.x for p in points],
            "y": [p.y for p in points],
            "z": values
        })

        # Save output
        output_file = Path(output_folder) / f"profile_line_{idx}.csv"
        df.to_csv(output_file, index=False)

print("Profiles successfully generated.")
