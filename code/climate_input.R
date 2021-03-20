setwd("C:/Users/mathias/ROOT/EDU/UM/MA_MPP/MTH4911/MPP_Thesis")

library(ncdf4)
library(fields)
fn <- "inputs/climate_data/cru_data/cru_ts4.04.1901.2019.pre.dat.nc"
nc <- nc_open(fn)

(nc) # data dimension is 720x350x1428

lat = ncvar_get(nc, "lat")
lon = ncvar_get(nc, "lon")
tm = ncvar_get(nc, "time")
tm = as.Date(tm, origin ="1900-01-01", tz ="UTC")

# timing of t 1 to 6

# t={1,3,5} is post-planting: September thru November YYYY.
# t={2,4,6} is post-harvest: February thru April YYYY+1

  # t=1   -> 9:11 , 2010
  # t=2   -> 2:4  , 2011
  # t=3   -> 9:11 , 2012
  # t=4   -> 2:4  , 2013
  # t=5   -> 9:11 , 2015
  # t=10  -> 2:4  , 2016


tm_1 = tm[l:r]
tm_2 = tm[l:r]
tm_3 = tm[l:r]
tm_4 = tm[l:r]
tm_5 = tm[l:r]
tm_6 = tm[l:r]



precip = ncvar_get(nc, "pre", start = c(543, 200, 1200), count = c(3,3,228))

plot(precip)

