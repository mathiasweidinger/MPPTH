
 
setwd("C:/Users/mathias/Desktop/NEON-DS-Field-Site-Spatial-Data")

install.packages("raster")
library(raster)
install.packages("sp")
library(sp)
install.packages("rgdal")
library(rgdal)
install.packages("maptools")
library(maptools)
install.packages("rgeos")
library(rgeos)
library(dplyr)
library(ggplot2)

# Method 3: Shapefiles
  
  # import the centroid data and the vegetation structure
  # this means all strings of letter coming in will remain in character

# read in plot centroids
centroids <- readr::read_csv(paste0("SJER/PlotCentroids/SJERPlotCentroids.csv"))
str(centroids)

# read in vegetation heights
vegStr <- readr::read_csv(paste0("SJER/VegetationData/D17_2013_vegStr.csv"))
str(vegStr)

# import digital terrain model
chm <- raster::raster(paste0("SJER/CHM_SJER.tif"))

# plot raster
plot(chm, main="Lidar Canopy Heights Model \n SJER, California")

## Overlay the centroid points and the stem locations on CHM plot

#plot the chm

myCol <- terrain.colors(6)
plot(chm, col=myCol, main="Plot & Tree Locations", breaks=c(-2,0,2,10,40))

## plotting details: cex = point size, pch 0 = square

#plot square around centroid
points(centroids$easting, centroids$northing, pch=0, cex=2)

#plot location of each tree measured
points(vegStr$easting, vegStr$northing, pch=19, cex=0.5, col=2)

# check CHM crs
chm@crs

## create a SpatialPointsDataFrame
# specify the easting (column 4) and northing (column 3) in that order
# specify CRS proj4string: borrow CRS from chm
#specify raster
centroid_spdf <- SpatialPointsDataFrame(centroids[,4:3], proj4string=chm@crs, centroids)

# check centroid crs
# note SPDFs do not have a crs slot so 'object@crs' won't work
centroid_spdf

# extract circular, 20m buffer

cent_max <- raster::extract(chm,
    centroid_spdf,  # SPDF with centroids for buffer)
    buffer = 20,    # buffer, sice, units depend on CRS
    fun=max,        # what value to extract
    df=TRUE)        # return a data frame?

cent_max

# grap the names of the plots from the centroid spdf
cent_max$plot_id <- centroid_spdf$Plot_ID

# fix the column names
names(cent_max) <- c("ID", "chmMaxHeight", "plot_id")

cent_max

# merge the chm data into the centroids data.frame
centroids <- merge(centroids, cent_max, by.x = 'Plot_ID', by.y = 'plot_id')

head(centroids)

# Instead of only taking the max-height, extract all
cent_heightList <- raster::extract(chm, centroid_spdf, buffer =20)
#create histograms for the first 5 plots of data using a for loop
par(mfrow=c(6,3))
for (i in 1:18){
  hist(cent_heightList[[i]], main=(paste("plot",i)))
  }

# find max stem height for eachplot
maxStemHeight_d <- vegStr %>% 
  group_by(plotid) %>% 
  summarise(max = max(stemheight))

head(maxStemHeight_d)

# fix names
names(maxStemHeight_d) <-c("plotid", "insituMaxHeight")

# one line of nested commands, 95% height value
insitu_d <- vegStr %>% 
  filter(plotid %in% centroids$Plot_ID) %>% 
  group_by(plotid) %>% 
  summarise(max = max(stemheight), quant = quantile(stemheight, .95))

head(insitu_d)

# merge insitu data into the centroids data.frame
centroids <- merge(centroids, maxStemHeight_d, by.x ='Plot_ID', by.y = 'plotid')


head(centroids)
centroids[,9] <- centroids$insituMaxHeight-centroids$chmMaxHeight
names(centroids$V9) = "diff"

# create plot

ggplot(centroids,aes(x=chmMaxHeight, y =insituMaxHeight )) + 
  geom_abline(slope=1, intercept = 0, alpha=.5, lty=2)+ # plotting our "1:1" line
  geom_point() + 
  theme_bw() + 
  ylab("Maximum measured height") + 
  xlab("Maximum lidar pixel")

# plot with regression

ggplot(centroids, aes(x=chmMaxHeight, y=insituMaxHeight)) +
  geom_abline(slope=1, intercept=0, alpha=.5, lty=2) + #plotting our "1:1" line
  geom_point() +
  geom_smooth(method = lm) + # add regression line and confidence interval
  ggtitle("Lidar CHM-derived vs. Measured Tree Height") +
  ylab("Maximum Measured Height") +
  xlab("Maximum Lidar Height") +
  theme(panel.background = element_rect(colour = "grey"),
        plot.title = element_text(family="sans", face="bold", size=20, vjust=1.19),
        axis.title.x = element_text(family="sans", face="bold", size=14, angle=00, hjust=0.54, vjust=-.2),
        axis.title.y = element_text(family="sans", face="bold", size=14, angle=90, hjust=0.54, vjust=1))
