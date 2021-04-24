setwd("C:/Users/mathias/ROOT/EDU/UM/MA_MPP/MTH4911/MPP_Thesis")

########## METEOROLOGICAL DATA AVAILABILITY

library(tidyverse)
library(sp)
library(raster)
library(cowplot)
library(data.table)
library(sf)
library(tmap) # I can make maps with this.

# Load CRU raster data
# Source: https://http://data.ceda.ac.uk/badc/cru/data/cru_ts/cru_ts_4.04/data/...
tmx <- raster("inputs/climate_data/cru_data/cru_ts4.04.1901.2019.tmx.dat.nc")
tmn <- raster("inputs/climate_data/cru_data/cru_ts4.04.1901.2019.tmn.dat.nc")
tmp <- raster("inputs/climate_data/cru_data/cru_ts4.04.1901.2019.tmp.dat.nc")
pre <- raster("inputs/climate_data/cru_data/cru_ts4.04.1901.2019.pre.dat.nc")

# Load Nigeria shapefile, transform to raster CRS, and simplify for performance
# Source: https://gadm.org/download_country_v3.html
poly <- readRDS("inputs/shapes/gadm36_NGA_1_sf.rds") %>% 
  st_transform(proj4string(tmx)) #%>%
st_simplify(0.01, preserveTopology = TRUE)

extent(poly)
e <- extent(2,15,4,14)

# Crop global raster to extent of polygon
tmx <- crop(tmx, e)
tmx <- mask(tmx, poly)
tmn <- crop(tmn, e)
#tmn <- mask(tmn, poly)
tmp <- crop(tmp, e)
#tmp <- mask(tmp, poly)
pre <- crop(pre, e)
#pre <- mask(pre, poly)



tmx_df <- as.data.frame(tmx, xy = TRUE)
names(tmx_df)[3] <- "value"
tmn_df <- as.data.frame(tmn, xy = TRUE)
names(tmn_df)[3] <- "value"
tmp_df <- as.data.frame(tmp, xy = TRUE)
names(tmp_df)[3] <- "value"
pre_df <- as.data.frame(pre, xy = TRUE)
names(pre_df)[3] <- "value"

# Import household geovars and spatialise
load("outputs/NGA_panel.Rda")
hh_data <- NGA_panel
sp::coordinates(hh_data) = c("LON_DD_MOD", "LAT_DD_MOD")
crs.geo1 = CRS("+proj=longlat")
proj4string(hh_data) = crs.geo1

# Make a plot with the shapefile and boxes for the raster cells, where gray cells indicate no data.
tmx_p <- ggplot(data = tmx_df) + 
  geom_sf(data = poly, fill = NA, colour = "black", size = 0.25) + 
  geom_tile(data = tmx_df %>% filter(is.na(value)), mapping = aes(x = x, y = y), size = 0.25, fill = NA, color = alpha("gray", 0.25)) +
  geom_tile(data = tmx_df %>% filter(!is.na(value)), mapping = aes(x = x, y = y), size = 0.25, fill = NA, color = alpha("red", 0.5)) +
  theme_map() + ggtitle("Max. Temp. coverage (0.5°x 0.5° grid)")

tmn_p <- ggplot(data = tmn_df) + 
  geom_sf(data = poly, fill = NA, colour = "black", size = 0.25) + 
  geom_tile(data = tmn_df %>% filter(is.na(value)), mapping = aes(x = x, y = y), size = 0.25, fill = NA, color = alpha("gray", 0.25)) +
  geom_tile(data = tmn_df %>% filter(!is.na(value)), mapping = aes(x = x, y = y), size = 0.25, fill = NA, color = alpha("blue", 0.5)) +
  theme_map() + ggtitle("Min. Temp. coverage (0.5°x 0.5° grid)")

tmp_p <- ggplot(data = tmp_df) + 
  geom_sf(data = poly, fill = NA, colour = "black", size = 0.25) + 
  geom_tile(data = tmp_df %>% filter(is.na(value)), mapping = aes(x = x, y = y), size = 0.25, fill = NA, color = alpha("gray", 0.25)) +
  geom_tile(data = tmp_df %>% filter(!is.na(value)), mapping = aes(x = x, y = y), size = 0.25, fill = NA, color = alpha("green", 0.5)) +
  theme_map() + ggtitle("Temp. coverage (0.5°x 0.5° grid)")

pre_p <- ggplot(data = pre_df) + 
  geom_sf(data = poly, fill = NA, colour = "black", size = 0.25) + 
  geom_tile(data = pre_df %>% filter(is.na(value)), mapping = aes(x = x, y = y), size = 0.5, fill = NA, color = alpha("gray", 0.25)) +
  geom_tile(data = pre_df %>% filter(!is.na(value)), mapping = aes(x = x, y = y), size = 0.5, fill = NA, color = alpha("cyan", 0.5)) +
  theme_map() + ggtitle("Precip. coverage (0.5°x 0.5° grid)")

tmx_p
tmn_p
tmp_p
pre_p

save_plot("outputs/cover_tmx.png", plot = tmx_p)
save_plot("outputs/cover_tmn.png", plot = tmn_p)
save_plot("outputs/cover_tmp.png", plot = tmp_p)
save_plot("outputs/cover_pre.png", plot = pre_p)

tmap_mode("view")

tm_shape(poly)+
  tm_borders("black", lwd = .5) + tm_shape(tmx) + tm_raster("near.surface.temperature.maximum", palette = cm.colors(2),) + tm_shape(hh_data) + tm_dots(col="steelblue", size= .2)
