# Load required libraries
library(rayshader)
library(raster)
library(elevatr)
library(sf)
library(dplyr)

# Define the Nördlinger Ries area
# Approximate center coordinates: 48.885°N, 10.607°E
# The crater is roughly 25 km in diameter

# Create a bounding box for the area
ries_bbox <- data.frame(
  x = c(10.45, 10.75),  # longitude
  y = c(48.75, 49.02)   # latitude
)

ries_coords <- data.frame(
  lon = c(10.45, 10.75, 10.75, 10.45, 10.45),
  lat = c(48.75, 48.75, 49.02, 49.02, 48.75)
)
# Convert to sf object for elevation data
ries_polygon <- ries_coords %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

# Get elevation data
ries_elev <- get_elev_raster(ries_polygon, z = 10, src = "aws")

# Convert to matrix for rayshader
elev_matrix <- raster_to_matrix(ries_elev)

elev_matrix %>%
  height_shade(texture = colorRampPalette(c("#2d4a22", "#5d7c3c", 
                                            "#8faa54", "#b8c77d", 
                                            "#dce6a6", "#f0f4d3"))(256)) %>%
  add_overlay(sphere_shade(elev_matrix, texture = "desert"), 
              alphalayer = 0.3) %>%
  add_shadow(ray_shade(elev_matrix, zscale = 5, maxsearch = 300), 0.6) %>%
  add_shadow(ambient_shade(elev_matrix, zscale = 5), 0.4) %>%
  plot_3d(elev_matrix, 
          zscale = 8, 
          fov = 0, 
          theta = 135, 
          zoom = 0.8, 
          phi = 30,
          windowsize = c(1200, 900),
          background = "white")

# Render high-quality image
render_highquality(
  filename = "nordlinger_ries.png",
  samples = 400,
  width = 1200,
  height = 800
)
