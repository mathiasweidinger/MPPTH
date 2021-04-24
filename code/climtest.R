# first set the working directory
climate_dir <- "C:/Users/mathias/ROOT/EDU/UM/MA_MPP/MTH4911/MPP_Thesis/inputs/climate_data/cru_data"
setwd(climate_dir)

# load required packages

library(sp) # spatial package
library(raster) # spatial raster
library(rasterVis) # raster visualization
library(sf) # spatial analysis
library(R.utils) # R utilities
library(tidyverse) # Data manipulation and visualization
library(ncdf4) # read and edit ncdf files
library(lattice) # lattice for raster
library(RColorBrewer) # colors for visualization
library(cowplot) # plot_themes
library(rgdal) # reading in spatial data
library(maptools) # mapping
library(maps) # mapping, again
library(rgeos) # spatial analysis
library(magrittr) # double pipe operators


file_paths <- fs::dir_ls() # query the directory for list of items in it
ncin <- lapply(file_paths, raster::brick) # load NetCDF files into SpatialRaster
poly <- readRDS("C:/Users/mathias/ROOT/EDU/UM/MA_MPP/MTH4911/MPP_Thesis/inputs/shapes/gadm36_NGA_1_sf.rds") %>% 
  st_transform(proj4string(ncin$cru_ts4.05.1901.2020.cld.dat.nc))# %>% st_simplify(0.01, preserveTopology = TRUE)

extent(poly)

ncin %<>% lapply(raster::crop, poly, snap = "out") #clip raster outside the poly border
# 10 raster bricks with dimensions (lat, lon, t) (20,25,1440)

# The time frame of interest starts on "2000-01-16" and ends on "2021-12-16".
# timing of t 1 to 6

# t={1,3,5} is post-planting: September thru November YYYY.
# t={2,4,6} is post-harvest: February thru April YYYY+1

# t=1   -> 8:10 , 2010 -> midpoint = 2010-09
# t=2   -> 2:4  , 2011 -> midpoint = 2011-03
# t=3   -> 8:10 , 2012 -> midpoint = 2012-09
# t=4   -> 2:4  , 2013 -> midpoint = 2013-03
# t=5   -> 8:10 , 2015 -> midpoint = 2015-09
# t=10  -> 2:4  , 2016 -> midpoint = 2016-03

tm_nga = c(1177:1440)
ncin %<>% lapply(raster::subset, tm_nga) # crop to time frame
# 10 raster bricks with dimensions (lat, lon, t) (20,25,264)
names(ncin) <- (substring(names(ncin),22,24)) # rename list items
load ("C:/Users/mathias/ROOT/EDU/UM/MA_MPP/MTH4911/MPP_Thesis/outputs/NGA_panel.Rda") # load household data 

# plot households to see whether all parts display correctly
plot(ncin$tmp$X2020.12.16, main="HH-locations")
plot(poly$geometry, add = TRUE)
points(NGA_panel$LON_DD_MOD, NGA_panel$LAT_DD_MOD, pch=19, cex=0.5, col = 2)
## plotting details: cex = point size, pch 0 = square


ncin$tmp$X2020.12.16@crs


## create SPDF: SpatialPointsDataFrame()
# specify the easting (column 4) & northing (columns 3) in that order
# specify CRS proj4string: borrow CRS from chm 
# specify raster
NGA_spatial <- SpatialPointsDataFrame(
  NGA_panel[,108:107], proj4string=ncin$cru_ts4.05.1901.2020.cld.dat.nc$X2020.12.16@crs, NGA_panel)

# next, extract climate data at a given location...

# extract circular, 20,000m (20KM) buffer

cent_mean <- raster::extract(ncin$cru_ts4.05.1901.2020.cld.dat.nc$X2020.12.16, # raster layer
                            NGA_spatial,     # SPDF with centroids for buffer
                            buffer = 20,     # buffer size, units depend on CRS
                            fun=mean,         # what to value to extract
                            df=TRUE)         # return a dataframe? 

# view
cent_mean











# # OTHER GISTS
# 
# 
# tas <- tmp.array [, , 1400]  #take a slice from the data 
# 
# mapCDFtemp <- function(lat,lon,tas) # define mapping function
#   
# {
#   
#   titletext <- "title"
#   
#   expand.grid(lon, lat) %>%
#     
#     rename(lon = Var1, lat = Var2) %>%
#     
#      mutate(lon = ifelse(lon > 180, -(360 - lon), lon),
#            
#            tas = as.vector(tas)) %>% 
#     
#     # (tas = convert_temperature(tas, "k", "c")) %>%
#     
#     
#     
#     ggplot() + 
#     
#     geom_point(aes(x = lon, y = lat, color = tas),
#                
#                size = 0.8) + 
#     
#     borders("world", colour="black", fill=NA) + 
#     
#     scale_color_viridis(na.value="white",name = "Temperature") + 
#     
#     theme(legend.direction="vertical", legend.position="right", legend.key.width=unit(0.4,"cm"), legend.key.heigh=unit(2,"cm")) + 
#     
#     coord_quickmap() + 
#     
#     ggtitle(titletext) +
#     
#     xlim(-180,180) +
#     
#     ylim(-90,90) +
#     
#     theme_minimal()
#   
# }
# 
# 
# mapCDFtemp(lon, lat, tas)



#remotes::install_github("mikejohnson51/AOI", force = T)
#remotes::install_github("mikejohnson51/climateR", force =T)

#library(climateR)
#library(AOI)

# 
# NGA = aoi_get(country = "Nigeria")
# tc1 = getTerraClim(NGA, param = "tmax", startDate = "1990-01-01")
# tc2 = getTerraClim(NGA, param = "tmax", startDate = "2000-01-01")
# tc3 = getTerraClim(NGA, param = "tmax", startDate = "2010-01-01")
# tc4 = getTerraClim(NGA, param = "tmax", startDate = "2019-01-01")
# 
# 
# p1 = levelplot(tc1$terraclim_tmax, par.settings = BuRdTheme, main = "January 2010; TerraClim", margin = FALSE) +
#   layer(sp.lines(as_Spatial(NGA), col="white", lwd=3))
# p2 = levelplot(tc2$terraclim_tmax, par.settings = BuRdTheme, main = "January 2011; TerraClim", margin = FALSE) +
#   layer(sp.lines(as_Spatial(NGA), col="white", lwd=3))
# p3 = levelplot(tc3$terraclim_tmax, par.settings = BuRdTheme, main = "January 2012; TerraClim", margin = FALSE) +
#   layer(sp.lines(as_Spatial(NGA), col="white", lwd=3))
# p4 = levelplot(tc4$terraclim_tmax, par.settings = BuRdTheme, main = "January 2013; TerraClim", margin = FALSE) +
#   layer(sp.lines(as_Spatial(NGA), col="white", lwd=3))
# 
# gridExtra::grid.arrange(p1,p2,p3,p4, nrow = 2)


