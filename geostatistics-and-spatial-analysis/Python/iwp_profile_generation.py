import geopandas as gpd
import rasterio
from rasterio import sample
import pandas as pd
from shapely.geometry import LineString, Point
import numpy as np

# PARAMETERS
shp_file = "'/Users/adelejoyeux/Downloads/CB24/article 1/IWP/rivier_profiles.shp'"
raster_file = "/Users/adelejoyeux/Downloads/CB24/article 1/IWP/adele_v512_survol_bizarre_dsm.tif"
spacing = 0.05  # 5 cm in the same units as your shapefile
output_folder = "/Users/adelejoyeux/Downloads/CB24/article 1/IWP/11aaaaaa_data/profiles/profiles_piscine_output"

# Load lines
lines = gpd.read_file(shp_file)

# Open raster
with rasterio.open(raster_file) as src:
    for idx, line in lines.iterrows():
        geom = line.geometry
        # Generate points along line every 'spacing' units
        line_length = geom.length
        distances = np.arange(0, line_length + spacing, spacing)
        points = [geom.interpolate(d) for d in distances]
        
        # Prepare coordinates for raster sampling
        coords = [(p.x, p.y) for p in points]
        
        # Sample raster values
        values = [val[0] for val in src.sample(coords)]
        
        # Build DataFrame
        df = pd.DataFrame({
            "line_ID": idx,
            "point_ID": range(len(points)),
            "dist": distances,
            "X": [p.x for p in points],
            "Y": [p.y for p in points],
            "Z": values
        })
        
        # Save CSV per line
        df.to_csv(f"{output_folder}profile_line_{idx}.csv", index=False)
