# Code to make a biome plot of all the places I've done fieldwork
# Exa mple code from: https://rawgit.com/valentinitnelav/plotbiomes/master/html/Whittaker_biomes_examples.html

# Install Package if needed

# devtools::install_github("valentinitnelav/plotbiomes")
# if need to install package

# Load packages 
library(plotbiomes)
library(tidyverse)
library(raster)
library(maptools)

# TODO: Make data frame with names and lat/long for field sites

# ===== Prepare raster stack

# Read temperature and precipitation as raster stack.
# Low resolution raster datasets come with 'plotbiomes' package.
path <- system.file("extdata", "temp_pp.tif", package = "plotbiomes")
temp_pp <- raster::stack(path)
names(temp_pp) <- c("temperature", "precipitation")

# ===== Generate random locations

data(wrld_simpl) # load world polygons from maptools

# Eliminate Antarctica - it is not represented in the raster datasets.
wrld_simpl <- wrld_simpl[wrld_simpl$NAME != "Antarctica", ]

# Create random locations within world's polygons.
set.seed(66) # random number generator
points <- sp::spsample(x = wrld_simpl, n = 50, type = "random")

# My attempt at making file to extract points from my field sites

# data frame for coordinates
loc.dat <- matrix(
        data = c(-143.5022, -124.0535, - 81.3521, -92.5251, -115.1398, -76.5564, -123.2620, -117.7198, #long
                 65.000, 44.6368, 27.1828, 46.7048, 36.1699, 38.9016, 44.5646, 34.0967 # lat
                 ),
          
        # Yukon Charley Rivers NP
        # Newport, OR
        # Archbold Biological Station
        # Cloquet Forestry Center
        # Las Vegas 
        # SERC
        # Corvallis, OR
        # Claremont, CA
        nrow = 8, 
        ncol = 2
)

# create SpatialPoints class from data to extract temp and precip

points <- SpatialPoints(
      coords = loc.dat, 
      proj4string = CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs") 
      #specifies coordinate reference system
      )


# ===== Extract from raster stack

# Extract temperature and precipitation values from the raster datasets
extractions <- raster::extract(temp_pp, points, df = TRUE)

# Adjust temperature values to normal scale because WorldClim temperature data
# has a scale factor of 10 (integer storage for saving space).
extractions$temperature <- extractions$temperature/10

# Convert precipitation from mm to cm
extractions$precipitation <- extractions$precipitation/10

# see points on world map
plot(temp_pp[[1]]/10); points(points)
plot(temp_pp[[2]]); points(points)


# Field sites oon whittaker biome plot
whittaker_base_plot() +
  # add the temperature - precipitation data points
  geom_point(data = extractions, 
             aes(x = temperature, 
                 y = precipitation), 
             size   = 4,
             shape  = 21,
             colour = "gray95", 
             fill   = "black",
             stroke = 1,
             alpha  = 0.5) +
  scale_y_continuous(name = "Precipitation (cm)", position = "right")+
  theme_bw()+
  theme(
    plot.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(), 
    # axis.line = element_line()
    axis.ticks = element_blank(), 
    legend.position = c(0, 1),
    legend.justification = c(0,1)
    # TODO figure out how to get clear sans font into plot
  )




