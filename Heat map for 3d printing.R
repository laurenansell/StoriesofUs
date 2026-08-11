library(leaflet)
library(leaflet.extras)
library(raster)
library(rgl)
library(MASS)
library(rayshader)


leaflet(crime_data_stopandsearch_hastings) %>%
  addTiles() %>%  # Add default OpenStreetMap background
  addHeatmap(
    lng = ~Longitude, 
    lat = ~Latitude, 
    blur = 20, 
    max = 0.05, 
    radius = 15
  )




k <- kde2d(
  x = crime_data_stopandsearch_hastings$Longitude,
  y = crime_data_stopandsearch_hastings$Latitude,
  n = 300
)

k$x   # longitude grid
k$y   # latitude grid
k$z   # density values



heightmap <- k$z

hillshade <- rayshader::height_shade(heightmap)

plot_3d(
  heightmap = heightmap,
  hillshade = hillshade,
  zscale = 0.01,
  solid = TRUE)


save_obj("heatmap.obj")


k <- kde2d(crime_data_stopandsearch_hastings$Longitude,
           crime_data_stopandsearch_hastings$Latitude, n = 300)

persp3d(
  x = k$x,
  y = k$y,
  z = k$z,
  aspect = c(1, 1, 0.2)
)

writeOBJ("heatmap.obj")
