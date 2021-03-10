
setwd("C:/Users/mathias/Desktop/Climate")

########## METEOROLOGICAL DATA AVAILABILITY

library(tidyverse)
library(sp)
library(raster)
library(cowplot)
library(data.table)
library(sf)
library(tmap)

data(World)

# Load CRU raster data
# Source: https://http://data.ceda.ac.uk/badc/cru/data/cru_ts/cru_ts_4.04/data/pre
rast <- raster("cru_ts4.04.1901.2019.tmx.dat.nc")

# Load Nigeria shapefile, transform to raster CRS, and simplify for performance
# Source: https://gadm.org/download_country_v3.html
poly <- st_read("new_lga_nigeria_2003.shp") %>% 
  st_transform(proj4string(rast)) %>%
  st_simplify(0.01, preserveTopology = TRUE)

# Crop global raster to extent of polygon
rast <- crop(rast, extent(poly))

rast_df <- as.data.frame(rast, xy = TRUE)
names(rast_df)[3] <- "value"

# Import household geovars and spatialise
hh_data = fread("nga_householdgeovariables_y1.csv")
hh_data = as.data.frame(hh_data)
coordinates(hh_data) = c("lon_dd_mod", "lat_dd_mod")
crs.geo1 = CRS("+proj=longlat")
proj4string(hh_data) = crs.geo1

# Make a plot with the shapefile and boxes for the raster cells, where gray cells indicate no data.
p <- ggplot(data = rast_df) + 
  geom_sf(data = poly, fill = NA, colour = "black", size = 0.25) + 
  geom_tile(data = rast_df %>% filter(is.na(value)), mapping = aes(x = x, y = y), size = 0.25, fill = NA, color = alpha("gray", 0.25)) +
  geom_tile(data = rast_df %>% filter(!is.na(value)), mapping = aes(x = x, y = y), size = 0.25, fill = NA, color = alpha("red", 0.5)) +
  theme_map()

save_plot("map.png", plot = p)

tmap_mode("view")

tm_shape(poly)+
  tm_borders("black", lwd = .5)+
  tm_shape(rast)+
  tm_raster("near.surface.temperature.maximum", palette = cm.colors(2),)+
  tm_shape(hh_data)+
  tm_dots(col="steelblue", size= .05)


