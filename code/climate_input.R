setwd("C:/Users/mathias/ROOT/EDU/UM/MA_MPP/MTH4911/MPP_Thesis")

library(ncdf4)
library(fields)
fn <- "inputs/climate_data/cru_data/cru_ts4.04.1901.2019.pre.dat.nc"
nc <- nc_open(fn)

(nc) # data dimension is 720x350x1428

lat = ncvar_get(nc, "lat")
lon = ncvar_get(nc, "lon")
tm = ncvar_get(nc, "time")
tm = as.Date(tm, origin ="1800-01-01", tz ="UTC")

precip = ncvar_get(nc, "pre", start = c(543, 200, 1200), count = c(3,3,228))

plot(precip)

