# Download up-to-date climate variable data from CRU

climate_dir <- "C:/Users/mathias/ROOT/EDU/UM/MA_MPP/MTH4911/MPP_Thesis/inputs/climate_data/cru_data" # climate data will be saved here!
setwd(climate_dir) # set working directory

# I use climate data from the University of East Anglia's CRU TS monthly high-res dataset
# The paper describing the data is here: https://www-nature-com.ezproxy.ub.unimaas.nl/articles/s41597-020-0453-3

# Download the climate data to working directory (this might take a while)
download.file("http://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.05/cruts.2103051243.v4.05/cld/cru_ts4.05.1901.2020.cld.dat.nc.gz",
             destfile = "cru_ts4.05.1901.2020.cld.dat.nc.gz")
download.file("http://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.05/cruts.2103051243.v4.05/dtr/cru_ts4.05.1901.2020.dtr.dat.nc.gz",
             destfile = "cru_ts4.05.1901.2020.dtr.dat.nc.gz")
download.file("http://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.05/cruts.2103051243.v4.05/frs/cru_ts4.05.1901.2020.frs.dat.nc.gz",
             destfile = "cru_ts4.05.1901.2020.frs.dat.nc.gz")
download.file("http://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.05/cruts.2103051243.v4.05/pet/cru_ts4.05.1901.2020.pet.dat.nc.gz",
             destfile = "cru_ts4.05.1901.2020.pet.dat.nc.gz")
download.file("https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.05/cruts.2103051243.v4.05/pre/cru_ts4.05.1901.2020.pre.dat.nc.gz",
             destfile = "cru_ts4.05.1901.2020.pre.dat.nc.gz")
download.file("http://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.05/cruts.2103051243.v4.05/tmn/cru_ts4.05.1901.2020.tmn.dat.nc.gz",
             destfile = "cru_ts4.05.1901.2020.tmn.dat.nc.gz")
download.file("http://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.05/cruts.2103051243.v4.05/tmp/cru_ts4.05.1901.2020.tmp.dat.nc.gz",
             destfile = "cru_ts4.05.1901.2020.tmp.dat.nc.gz")
download.file("http://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.05/cruts.2103051243.v4.05/tmx/cru_ts4.05.1901.2020.tmx.dat.nc.gz",
             destfile = "cru_ts4.05.1901.2020.tmx.dat.nc.gz")
download.file("http://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.05/cruts.2103051243.v4.05/vap/cru_ts4.05.1901.2020.vap.dat.nc.gz",
             destfile = "cru_ts4.05.1901.2020.vap.dat.nc.gz")
download.file("http://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.05/cruts.2103051243.v4.05/wet/cru_ts4.05.1901.2020.wet.dat.nc.gz",
             destfile = "cru_ts4.05.1901.2020.wet.dat.nc.gz")
file_paths <- fs::dir_ls() # query the directory for list of items in it
lapply(file_paths, gunzip, remove = TRUE, overwrite = TRUE) # unzip and overwrite files

# DONE