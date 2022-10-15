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
        data = c(-143.5022, -124.0535, - 81.3521, -92.5251, -115.1398, -76.5564, -123.2620, -117.7198, 35.7328, #long
                 65.000, 44.6368, 27.1828, 46.7048, 36.1699, 38.9016, 44.5646, 34.0967, -3.3127 # lat
                 ),
          
        # Yukon Charley Rivers NP
        # Newport, OR
        # Archbold Biological Station
        # Cloquet Forestry Center
        # Las Vegas 
        # SERC
        # Corvallis, OR
        # Claremont, CA
        # Rhotia, TZ
        nrow = 9, 
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


# TODO add label information to extractions data set
# e.g. site name, time worked 

extractions$location <- c(
     "Yukon-Charley Rivers National Preserve",
     "Pacific Coastal Ecology Branch, US EPA",
     "Archbold Biological Station",
     "Cloquet Forestry Center, UMN",
     "USGS Las Vegas Field Office", 
     "Smithsonian Environmental Reserach Center",
     "Institute for Applied Ecology", 
     "Bernard Field Station",
     "School for Field Studies, Tanzania"
                          )

# Field sites oon whittaker biome plot
biome_plot <- whittaker_base_plot() +
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



# Try writing plot function without the use of "expression" 

utils::data("Whittaker_biomes", envir = environment())
utils::data("Ricklefs_colors", envir = environment())

color_palette <- Ricklefs_colors

plt <- ggplot() + 
     geom_polygon(data = Whittaker_biomes, aes(x = temp_c, y = precp_cm, fill = biome), 
                  colour = "gray98", size = 1) +
     scale_fill_manual(name = "Whittaker biomes", breaks = names(color_palette), 
                       labels = names(color_palette), values = color_palette) + 
     scale_x_continuous("Temperature (degrees C)") + 
     scale_y_continuous("Precipitation (cm)", position = "right")


biome_plot <- plt + geom_point(data = extractions, 
           aes(x = temperature, 
               y = precipitation, 
               label = location), 
           size   = 4,
           shape  = 21,
           colour = "gray95", 
           fill   = "black",
           stroke = 1,
           alpha  = 0.5) +
     theme_bw()+
     guides(fill = guide_legend(ncol = 2, byrow = F))+
     #geom_text(data = extractions, aes(label = location, x = temperature, y = precipitation))+
     ggtitle("Whittaker Biomes of Past Research Sites")+
     theme(legend.background = element_blank(),
          # plot.background = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          # axis.line = element_line()
          axis.ticks = element_blank(), 
          legend.position = c(0,1),
          legend.justification = c(0,1),
          legend.text = element_text(size = 8), legend.title = element_blank(),
          # TODO figure out how to get clear sans font into plot
     ) 

plotly::ggplotly(biome_plot)

biome_plot

ggsave("biome2.png", device = "png", path = "output_plots/", units = "in", width = 6, height = 5, dpi = 300)
