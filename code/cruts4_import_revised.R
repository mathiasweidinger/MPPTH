#-------------- setting up----------------------------------------------------------------------------------------------

# set wd
setwd("C:/Users/mathias/ROOT/EDU/UM/MA_MPP/MTH4911/MPP_Thesis")

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
library(tictoc) # capture run time

load ("C:/Users/mathias/ROOT/EDU/UM/MA_MPP/MTH4911/MPP_Thesis/outputs/NGA_panel.Rda") # load household data 

#-------------- create DV Child Labour ---------------------------------------------------------------------------------

NGA_panel %<>%
  mutate(childchores = case_when(
    !is.na(chores) & chores <= 1200 & age < 18 ~ (chores * 7)/60,
    !is.na(chores) & chores > 1200 & age < 18 ~ NA_real_,
    is.na(chores) & age < 18 ~ NA_real_,
    age >= 18 ~ NA_real_
  ))
NGA_panel %<>%
  mutate(childwork = case_when(
    !is.na(pri_hrs_7d) & !is.na(sec_hrs) & pri_hrs_7d + sec_hrs <= 140 & age < 18 ~ as.numeric(pri_hrs_7d + sec_hrs),
    !is.na(pri_hrs_7d) & !is.na(sec_hrs) & pri_hrs_7d + sec_hrs > 140 & age < 18 ~ 140,
    !is.na(pri_hrs_7d) & is.na(sec_hrs) & pri_hrs_7d <= 140 & age < 18 ~ as.numeric(pri_hrs_7d),
    !is.na(pri_hrs_7d) & is.na(sec_hrs) & pri_hrs_7d > 140 & age < 18 ~ 140,
    is.na(pri_hrs_7d) & !is.na(sec_hrs) & sec_hrs <= 140 & age < 18 ~ as.numeric(sec_hrs),
    is.na(pri_hrs_7d) & !is.na(sec_hrs) & sec_hrs > 140 & age <18 ~ 140,
    !is.na(pri_hrs_7d) & !is.na(pri_hrs_7d) ~ NA_real_,
    age >= 18 ~ NA_real_
  ))

NGA_panel %<>%
  mutate(childworkchores = case_when(
    !is.na(childwork) & !is.na(childchores) ~ as.numeric(childwork+childchores),
    !is.na(childwork) & is.na(childchores) ~ childwork,
    is.na(childwork) & !is.na(childchores) ~ childchores,
    is.na(childwork) & is.na(childchores) ~ NA_real_
  ))


#-------------- code the MPI -------------------------------------------------------------------------------------------

## import data for food
file_paths <- fs::dir_ls("inputs/lsms_data/NGA_2010_GHSP-W1_v03_M_CSV/PPW1/Household")
lst <- lapply(file_paths, read.csv)

food_1 <- as.data.frame(lst[[16]]) %>% select("zone":"hhid","s9q5")

file_paths <- fs::dir_ls("inputs/lsms_data/NGA_2010_GHSP-W1_v03_M_CSV/PHW1/Household")
lst <- lapply(file_paths, read.csv)

food_2 <- as.data.frame(lst[[11]]) %>% select("zone":"hhid", "s12q5")

file_paths <- fs::dir_ls("inputs/lsms_data/NGA_2012_GHSP-W2_v02_M_CSV/Post Planting Wave 2/Household")
lst <- lapply(file_paths, read.csv)

food_3 <- as.data.frame(lst[[21]]) %>% select("zone":"hhid", "s9q5")

file_paths <- fs::dir_ls("inputs/lsms_data/NGA_2015_GHSP-W3_v02_M_CSV/P")
lst <- lapply(file_paths, read.csv)

food_5 <- as.data.frame(lst[[28]]) %>% select("zone":"hhid", "s9q5")

## import data for bednets

file_paths <- fs::dir_ls("inputs/lsms_data/NGA_2010_GHSP-W1_v03_M_CSV/PHW1/Household")
lst <- lapply(file_paths, read.csv)
bednets_2 <- as.data.frame(lst[[22]]) %>% select("zone":"indiv", "s4aq37")

file_paths <- fs::dir_ls("inputs/lsms_data/NGA_2012_GHSP-W2_v02_M_CSV/Post Harvest Wave 2/Household")
lst <- lapply(file_paths, read.csv)
bednets_4 <- as.data.frame(lst[[22]]) %>% select("zone":"indiv", "s4aq37")

file_paths <- fs::dir_ls("inputs/lsms_data/NGA_2015_GHSP-W3_v02_M_CSV/H")
lst <- lapply(file_paths, read.csv)
bednets_6 <- as.data.frame(lst[[22]]) %>% select("zone":"indiv", "s4aq37")

## import HH amenities

file_paths <- fs::dir_ls("inputs/lsms_data/NGA_2010_GHSP-W1_v03_M_CSV/PPW1/Household")
lst <- lapply(file_paths, read.csv)
amenities_1 <- cbind(t=1, as.data.frame(lst[[7]]))
amenities_1 <- amenities_1[,c(7,8)] %>% group_by(hhid) %>% summarise(item_cd = paste(item_cd, collapse = ","))

amenities_1 %<>% mutate(
  sofa = case_when(grepl("301", item_cd, fixed = T)==T ~ 1,
                   grepl("301", item_cd, fixed = T)==F ~ 0),
  chairs = case_when(grepl("302", item_cd, fixed = T)==T ~ 1,
                     grepl("302", item_cd, fixed = T)==F ~ 0),
  tables = case_when(grepl("303", item_cd, fixed = T)==T ~ 1,
                     grepl("303", item_cd, fixed = T)==F ~ 0),
  mattress = case_when(grepl("304", item_cd, fixed = T)==T ~ 1,
                       grepl("304", item_cd, fixed = T)==F ~ 0),
  bed = case_when(grepl("305", item_cd, fixed = T)==T ~ 1,
                  grepl("305", item_cd, fixed = T)==F ~ 0),
  mat = case_when(grepl("306", item_cd, fixed = T)==T ~ 1,
                  grepl("306", item_cd, fixed = T)==F ~ 0),
  sewingmachine = case_when(grepl("307", item_cd, fixed = T)==T ~ 1,
                            grepl("307", item_cd, fixed = T)==F ~ 0),
  gascooker = case_when(grepl("308", item_cd, fixed = T)==T ~ 1,
                        grepl("308", item_cd, fixed = T)==F ~ 0),
  st_electric = case_when(grepl("309", item_cd, fixed = T)==T ~ 1,
                          grepl("309", item_cd, fixed = T)==F ~ 0),
  st_gastable = case_when(grepl("310", item_cd, fixed = T)==T ~ 1,
                          grepl("310", item_cd, fixed = T)==F ~ 0),
  st_kerosene = case_when(grepl("311", item_cd, fixed = T)==T ~ 1,
                          grepl("311", item_cd, fixed = T)==F ~ 0),
  fridge = case_when(grepl("312", item_cd, fixed = T)==T ~ 1,
                     grepl("312", item_cd, fixed = T)==F ~ 0),
  freezer = case_when(grepl("313", item_cd, fixed = T)==T ~ 1,
                      grepl("313", item_cd, fixed = T)==F ~ 0),
  aircon = case_when(grepl("314", item_cd, fixed = T)==T ~ 1,
                     grepl("314", item_cd, fixed = T)==F ~ 0),
  washingmachine = case_when(grepl("315", item_cd, fixed = T)==T ~ 1,
                             grepl("315", item_cd, fixed = T)==F ~ 0),
  dryer = case_when(grepl("316", item_cd, fixed = T)==T ~ 1,
                    grepl("316", item_cd, fixed = T)==F ~ 0),
  bike = case_when(grepl("317", item_cd, fixed = T)==T ~ 1,
                   grepl("317", item_cd, fixed = T)==F ~ 0),
  motorbike = case_when(grepl("318", item_cd, fixed = T)==T ~ 1,
                        grepl("318", item_cd, fixed = T)==F ~ 0),
  car = case_when(grepl("319", item_cd, fixed = T)==T ~ 1,
                  grepl("319", item_cd, fixed = T)==F ~ 0),
  generator = case_when(grepl("320", item_cd, fixed = T)==T ~ 1,
                        grepl("320", item_cd, fixed = T)==F ~ 0),
  fan = case_when(grepl("321", item_cd, fixed = T)==T ~ 1,
                  grepl("321", item_cd, fixed = T)==F ~ 0),
  radio = case_when(grepl("322", item_cd, fixed = T)==T ~ 1,
                    grepl("322", item_cd, fixed = T)==F ~ 0),
  cassette = case_when(grepl("323", item_cd, fixed = T)==T ~ 1,
                       grepl("323", item_cd, fixed = T)==F ~ 0),
  hifi = case_when(grepl("324", item_cd, fixed = T)==T ~ 1,
                   grepl("324", item_cd, fixed = T)==F ~ 0),
  microwave = case_when(grepl("325", item_cd, fixed = T)==T ~ 1,
                        grepl("325", item_cd, fixed = T)==F ~ 0),
  iron = case_when(grepl("326", item_cd, fixed = T)==T ~ 1,
                   grepl("326", item_cd, fixed = T)==F ~ 0),
  tv = case_when(grepl("327", item_cd, fixed = T)==T ~ 1,
                 grepl("327", item_cd, fixed = T)==F ~ 0),
  pc = case_when(grepl("328", item_cd, fixed = T)==T ~ 1,
                 grepl("328", item_cd, fixed = T)==F ~ 0),
  dvd = case_when(grepl("329", item_cd, fixed = T)==T ~ 1,
                  grepl("329", item_cd, fixed = T)==F ~ 0),
  sat = case_when(grepl("330", item_cd, fixed = T)==T ~ 1,
                  grepl("330", item_cd, fixed = T)==F ~ 0),
  musicinst = case_when(grepl("331", item_cd, fixed = T)==T ~ 1,
                        grepl("331", item_cd, fixed = T)==F ~ 0),
  mobile = case_when(grepl("332", item_cd, fixed = T)==T ~ 1,
                     grepl("332", item_cd, fixed = T)==F ~ 0),
  inverter = case_when(grepl("333", item_cd, fixed = T)==T ~ 1,
                       grepl("333", item_cd, fixed = T)==F ~ 0),
)
amenities_1$amenities <- rowSums(amenities_1[,c(3:35)])

file_paths <- fs::dir_ls("inputs/lsms_data/NGA_2012_GHSP-W2_v02_M_CSV/Post Planting Wave 2/Household")
lst <- lapply(file_paths, read.csv)
amenities_3 <- cbind(t=3, as.data.frame(lst[[11]]))[-c(9,11)]
amenities_3 %<>% filter(s5q1 != 0)
amenities_3 <- amenities_3[,c(7,8)] %>% group_by(hhid) %>% summarise(item_cd = paste(item_cd, collapse = ","))
amenities_3 %<>% mutate(
  sofa = case_when(grepl("301", item_cd, fixed = T)==T ~ 1,
                   grepl("301", item_cd, fixed = T)==F ~ 0),
  chairs = case_when(grepl("302", item_cd, fixed = T)==T ~ 1,
                     grepl("302", item_cd, fixed = T)==F ~ 0),
  tables = case_when(grepl("303", item_cd, fixed = T)==T ~ 1,
                     grepl("303", item_cd, fixed = T)==F ~ 0),
  mattress = case_when(grepl("304", item_cd, fixed = T)==T ~ 1,
                       grepl("304", item_cd, fixed = T)==F ~ 0),
  bed = case_when(grepl("305", item_cd, fixed = T)==T ~ 1,
                  grepl("305", item_cd, fixed = T)==F ~ 0),
  mat = case_when(grepl("306", item_cd, fixed = T)==T ~ 1,
                  grepl("306", item_cd, fixed = T)==F ~ 0),
  sewingmachine = case_when(grepl("307", item_cd, fixed = T)==T ~ 1,
                            grepl("307", item_cd, fixed = T)==F ~ 0),
  gascooker = case_when(grepl("308", item_cd, fixed = T)==T ~ 1,
                        grepl("308", item_cd, fixed = T)==F ~ 0),
  st_electric = case_when(grepl("309", item_cd, fixed = T)==T ~ 1,
                          grepl("309", item_cd, fixed = T)==F ~ 0),
  st_gastable = case_when(grepl("310", item_cd, fixed = T)==T ~ 1,
                          grepl("310", item_cd, fixed = T)==F ~ 0),
  st_kerosene = case_when(grepl("311", item_cd, fixed = T)==T ~ 1,
                          grepl("311", item_cd, fixed = T)==F ~ 0),
  fridge = case_when(grepl("312", item_cd, fixed = T)==T ~ 1,
                     grepl("312", item_cd, fixed = T)==F ~ 0),
  freezer = case_when(grepl("313", item_cd, fixed = T)==T ~ 1,
                      grepl("313", item_cd, fixed = T)==F ~ 0),
  aircon = case_when(grepl("314", item_cd, fixed = T)==T ~ 1,
                     grepl("314", item_cd, fixed = T)==F ~ 0),
  washingmachine = case_when(grepl("315", item_cd, fixed = T)==T ~ 1,
                             grepl("315", item_cd, fixed = T)==F ~ 0),
  dryer = case_when(grepl("316", item_cd, fixed = T)==T ~ 1,
                    grepl("316", item_cd, fixed = T)==F ~ 0),
  bike = case_when(grepl("317", item_cd, fixed = T)==T ~ 1,
                   grepl("317", item_cd, fixed = T)==F ~ 0),
  motorbike = case_when(grepl("318", item_cd, fixed = T)==T ~ 1,
                        grepl("318", item_cd, fixed = T)==F ~ 0),
  car = case_when(grepl("319", item_cd, fixed = T)==T ~ 1,
                  grepl("319", item_cd, fixed = T)==F ~ 0),
  generator = case_when(grepl("320", item_cd, fixed = T)==T ~ 1,
                        grepl("320", item_cd, fixed = T)==F ~ 0),
  fan = case_when(grepl("321", item_cd, fixed = T)==T ~ 1,
                  grepl("321", item_cd, fixed = T)==F ~ 0),
  radio = case_when(grepl("322", item_cd, fixed = T)==T ~ 1,
                    grepl("322", item_cd, fixed = T)==F ~ 0),
  cassette = case_when(grepl("323", item_cd, fixed = T)==T ~ 1,
                       grepl("323", item_cd, fixed = T)==F ~ 0),
  hifi = case_when(grepl("324", item_cd, fixed = T)==T ~ 1,
                   grepl("324", item_cd, fixed = T)==F ~ 0),
  microwave = case_when(grepl("325", item_cd, fixed = T)==T ~ 1,
                        grepl("325", item_cd, fixed = T)==F ~ 0),
  iron = case_when(grepl("326", item_cd, fixed = T)==T ~ 1,
                   grepl("326", item_cd, fixed = T)==F ~ 0),
  tv = case_when(grepl("327", item_cd, fixed = T)==T ~ 1,
                 grepl("327", item_cd, fixed = T)==F ~ 0),
  pc = case_when(grepl("328", item_cd, fixed = T)==T ~ 1,
                 grepl("328", item_cd, fixed = T)==F ~ 0),
  dvd = case_when(grepl("329", item_cd, fixed = T)==T ~ 1,
                  grepl("329", item_cd, fixed = T)==F ~ 0),
  sat = case_when(grepl("330", item_cd, fixed = T)==T ~ 1,
                  grepl("330", item_cd, fixed = T)==F ~ 0),
  musicinst = case_when(grepl("331", item_cd, fixed = T)==T ~ 1,
                        grepl("331", item_cd, fixed = T)==F ~ 0),
  mobile = case_when(grepl("332", item_cd, fixed = T)==T ~ 1,
                     grepl("332", item_cd, fixed = T)==F ~ 0),
  inverter = case_when(grepl("333", item_cd, fixed = T)==T ~ 1,
                       grepl("333", item_cd, fixed = T)==F ~ 0),
)
amenities_3$amenities <- rowSums(amenities_3[,c(3:35)])

file_paths <- fs::dir_ls("inputs/lsms_data/NGA_2015_GHSP-W3_v02_M_CSV/P")
lst <- lapply(file_paths, read.csv)
amenities_5 <- cbind(t=5, as.data.frame(lst[[21]]))[-c(9,10,12:15)]
amenities_5 %<>% filter(s5q1 != 0)
amenities_5 <- amenities_5[,c(7,8)] %>% group_by(hhid) %>% summarise(item_cd = paste(item_cd, collapse = ","))
amenities_5 %<>% mutate(
  sofa = case_when(grepl("301", item_cd, fixed = T)==T ~ 1,
                   grepl("301", item_cd, fixed = T)==F ~ 0),
  chairs = case_when(grepl("302", item_cd, fixed = T)==T ~ 1,
                     grepl("302", item_cd, fixed = T)==F ~ 0),
  tables = case_when(grepl("303", item_cd, fixed = T)==T ~ 1,
                     grepl("303", item_cd, fixed = T)==F ~ 0),
  mattress = case_when(grepl("304", item_cd, fixed = T)==T ~ 1,
                       grepl("304", item_cd, fixed = T)==F ~ 0),
  bed = case_when(grepl("305", item_cd, fixed = T)==T ~ 1,
                  grepl("305", item_cd, fixed = T)==F ~ 0),
  mat = case_when(grepl("306", item_cd, fixed = T)==T ~ 1,
                  grepl("306", item_cd, fixed = T)==F ~ 0),
  sewingmachine = case_when(grepl("307", item_cd, fixed = T)==T ~ 1,
                            grepl("307", item_cd, fixed = T)==F ~ 0),
  gascooker = case_when(grepl("308", item_cd, fixed = T)==T ~ 1,
                        grepl("308", item_cd, fixed = T)==F ~ 0),
  st_electric = case_when(grepl("309", item_cd, fixed = T)==T ~ 1,
                          grepl("309", item_cd, fixed = T)==F ~ 0),
  st_gastable = case_when(grepl("310", item_cd, fixed = T)==T ~ 1,
                          grepl("310", item_cd, fixed = T)==F ~ 0),
  st_kerosene = case_when(grepl("311", item_cd, fixed = T)==T ~ 1,
                          grepl("311", item_cd, fixed = T)==F ~ 0),
  fridge = case_when(grepl("312", item_cd, fixed = T)==T ~ 1,
                     grepl("312", item_cd, fixed = T)==F ~ 0),
  freezer = case_when(grepl("313", item_cd, fixed = T)==T ~ 1,
                      grepl("313", item_cd, fixed = T)==F ~ 0),
  aircon = case_when(grepl("314", item_cd, fixed = T)==T ~ 1,
                     grepl("314", item_cd, fixed = T)==F ~ 0),
  washingmachine = case_when(grepl("315", item_cd, fixed = T)==T ~ 1,
                             grepl("315", item_cd, fixed = T)==F ~ 0),
  dryer = case_when(grepl("316", item_cd, fixed = T)==T ~ 1,
                    grepl("316", item_cd, fixed = T)==F ~ 0),
  bike = case_when(grepl("317", item_cd, fixed = T)==T ~ 1,
                   grepl("317", item_cd, fixed = T)==F ~ 0),
  motorbike = case_when(grepl("318", item_cd, fixed = T)==T ~ 1,
                        grepl("318", item_cd, fixed = T)==F ~ 0),
  car = case_when(grepl("319", item_cd, fixed = T)==T ~ 1,
                  grepl("319", item_cd, fixed = T)==F ~ 0),
  generator = case_when(grepl("320", item_cd, fixed = T)==T ~ 1,
                        grepl("320", item_cd, fixed = T)==F ~ 0),
  fan = case_when(grepl("321", item_cd, fixed = T)==T ~ 1,
                  grepl("321", item_cd, fixed = T)==F ~ 0),
  radio = case_when(grepl("322", item_cd, fixed = T)==T ~ 1,
                    grepl("322", item_cd, fixed = T)==F ~ 0),
  cassette = case_when(grepl("323", item_cd, fixed = T)==T ~ 1,
                       grepl("323", item_cd, fixed = T)==F ~ 0),
  hifi = case_when(grepl("324", item_cd, fixed = T)==T ~ 1,
                   grepl("324", item_cd, fixed = T)==F ~ 0),
  microwave = case_when(grepl("325", item_cd, fixed = T)==T ~ 1,
                        grepl("325", item_cd, fixed = T)==F ~ 0),
  iron = case_when(grepl("326", item_cd, fixed = T)==T ~ 1,
                   grepl("326", item_cd, fixed = T)==F ~ 0),
  tv = case_when(grepl("327", item_cd, fixed = T)==T ~ 1,
                 grepl("327", item_cd, fixed = T)==F ~ 0),
  pc = case_when(grepl("328", item_cd, fixed = T)==T ~ 1,
                 grepl("328", item_cd, fixed = T)==F ~ 0),
  dvd = case_when(grepl("329", item_cd, fixed = T)==T ~ 1,
                  grepl("329", item_cd, fixed = T)==F ~ 0),
  sat = case_when(grepl("330", item_cd, fixed = T)==T ~ 1,
                  grepl("330", item_cd, fixed = T)==F ~ 0),
  musicinst = case_when(grepl("331", item_cd, fixed = T)==T ~ 1,
                        grepl("331", item_cd, fixed = T)==F ~ 0),
  mobile = case_when(grepl("332", item_cd, fixed = T)==T ~ 1,
                     grepl("332", item_cd, fixed = T)==F ~ 0),
  inverter = case_when(grepl("333", item_cd, fixed = T)==T ~ 1,
                       grepl("333", item_cd, fixed = T)==F ~ 0),
)
amenities_5$amenities <- rowSums(amenities_5[,c(3:35)])

amenities_1 <- amenities_1[,-c(2:35)]
amenities_3 <- amenities_3[,-c(2:35)]
amenities_5 <- amenities_5[,-c(2:35)]

file_paths <- fs::dir_ls("inputs/lsms_data/NGA_2010_GHSP-W1_v03_M_CSV/PHW1/Household")
lst <- lapply(file_paths, read.csv)
pov_2 <- as.data.frame(lst[[27]]) %>% select("zone":"hhid","s8q6", "s8q7", "s8q8","s8q11",
                                             "s8q17", "s8q34a", "s8q34b", "s8q36a", "s8q37")

file_paths <- fs::dir_ls("inputs/lsms_data/NGA_2012_GHSP-W2_v02_M_CSV/Post Harvest Wave 2/Household")
lst <- lapply(file_paths, read.csv)
pov_4 <- as.data.frame(lst[[27]]) %>% select("zone":"hhid","s8q6", "s8q7", "s8q8","s8q11",
                                             "s8q17", "s8q34a", "s8q34b", "s8q36", "s8q37")
pov_4 <- pov_4[,-6]

file_paths <- fs::dir_ls("inputs/lsms_data/NGA_2015_GHSP-W3_v02_M_CSV/P")
lst <- lapply(file_paths, read.csv)
pov_5 <- as.data.frame(lst[[12]]) %>% select("zone":"hhid","s11q6", "s11q7", "s11q8","s11q11",
                                             "s11q17b", "s11q34a", "s11q34b", "s11q36", "s11q37")

names(pov_2)[7:15] <- c("material_wall", "material_roof", "material_floor", "fuel_cooking", "electricity",
                        "drinking_time", "drinking_unit", "toilet_type", "toilet_shared")

names(pov_4)[7:15] <- c("material_wall", "material_roof", "material_floor", "fuel_cooking", "electricity",
                        "drinking_time", "drinking_unit", "toilet_type", "toilet_shared")

names(pov_5)[7:15] <- c("material_wall", "material_roof", "material_floor", "fuel_cooking", "electricity",
                        "drinking_time", "drinking_unit", "toilet_type", "toilet_shared")

## divide panel into cross sections

T1 <- NGA_panel[which(NGA_panel$t==1),]
T2 <- NGA_panel[which(NGA_panel$t==2),]
T3 <- NGA_panel[which(NGA_panel$t==3),]
T4 <- NGA_panel[which(NGA_panel$t==4),]
T5 <- NGA_panel[which(NGA_panel$t==5),]
T6 <- NGA_panel[which(NGA_panel$t==6),]

## Merge in food

names(food_1)[7] <- "enough_food"
names(food_2)[7] <- "enough_food"
names(food_3)[7] <- "enough_food"
names(food_5)[7] <- "enough_food"

T1 <- left_join(T1, food_1, by = c("zone", "state", "lga", "sector", "ea", "hhid")) #23397
T2 <- left_join(T2, food_2, by = c("zone", "state", "lga", "sector", "ea", "hhid")) #23530
T3 <- left_join(T3, food_3, by = c("zone", "state", "lga", "sector", "ea", "hhid")) #27194
T4 <- left_join(T4, food_3, by = c("zone", "state", "lga", "sector", "ea", "hhid")) #27352
T5 <- left_join(T5, food_5, by = c("zone", "state", "lga", "sector", "ea", "hhid")) #26471
T6 <- left_join(T6, food_5, by = c("zone", "state", "lga", "sector", "ea", "hhid")) #26171

## merge in bednets

names(bednets_2)[8] <- "bednet"
names(bednets_4)[8] <- "bednet"
names(bednets_6)[8] <- "bednet"

T1 <- left_join(T1, bednets_2, by = c("zone", "state", "lga", "sector", "ea", "hhid", "indiv"))
T2 <- left_join(T2, bednets_2, by = c("zone", "state", "lga", "sector", "ea", "hhid", "indiv"))
T3 <- left_join(T3, bednets_2, by = c("zone", "state", "lga", "sector", "ea", "hhid", "indiv"))
T4 <- left_join(T4, bednets_4, by = c("zone", "state", "lga", "sector", "ea", "hhid", "indiv"))
T5 <- left_join(T5, bednets_4, by = c("zone", "state", "lga", "sector", "ea", "hhid", "indiv"))
T6 <- left_join(T6, bednets_6, by = c("zone", "state", "lga", "sector", "ea", "hhid", "indiv"))

## merge in HH amenities

T1 <- left_join(T1, amenities_1, by = c("hhid"))
T2 <- left_join(T2, amenities_1, by = c("hhid"))
T3 <- left_join(T3, amenities_3, by = c("hhid"))
T4 <- left_join(T4, amenities_3, by = c("hhid"))
T5 <- left_join(T5, amenities_5, by = c("hhid"))
T6 <- left_join(T6, amenities_5, by = c("hhid"))

## merge in pov

T1 <- left_join(T1, pov_2, by = c("zone", "state", "lga", "sector", "ea", "hhid")) #23397
T2 <- left_join(T2, pov_2, by = c("zone", "state", "lga", "sector", "ea", "hhid")) #23530
T3 <- left_join(T3, pov_4, by = c("zone", "state", "lga", "sector", "ea", "hhid")) #27194
T4 <- left_join(T4, pov_4, by = c("zone", "state", "lga", "sector", "ea", "hhid")) #27352
T5 <- left_join(T5, pov_5, by = c("zone", "state", "lga", "sector", "ea", "hhid")) #26471
T6 <- left_join(T6, pov_5, by = c("zone", "state", "lga", "sector", "ea", "hhid")) #26171

## household's maximum education
hhmaxeduc <- aggregate(highest_educ ~ t + hhid, data = NGA_panel, max, na.rm=T)

e1 <- hhmaxeduc[hhmaxeduc$t==1,]
names(e1)[3] <- "maxeduc"

e3 <- hhmaxeduc[hhmaxeduc$t==3,]
names(e3)[3] <- "maxeduc"

e6 <- hhmaxeduc[hhmaxeduc$t==6,]
names(e6)[3] <- "maxeduc"

T1 <- left_join(T1, e1, by=c("t", "hhid"))
T2 <- left_join(T2, e1, by=c("t", "hhid"))
T3 <- left_join(T3, e3, by=c("t", "hhid"))
T4 <- left_join(T4, e3, by=c("t", "hhid"))
T5 <- left_join(T5, e6, by=c("t", "hhid"))
T6 <- left_join(T6, e6, by=c("t", "hhid"))

## household's children's minimum attendance
hhminatt <- aggregate(NGA_panel$school_current ~ t + hhid, data = NGA_panel, max, na.rm=T)

e1 <- hhminatt[hhminatt$t==1,]
names(e1)[3] <- "minatt"
e2 <- hhminatt[hhminatt$t==2,]
names(e2)[3] <- "minatt"
e3 <- hhminatt[hhminatt$t==3,]
names(e3)[3] <- "minatt"
e4 <- hhminatt[hhminatt$t==4,]
names(e4)[3] <- "minatt"
e6 <- hhminatt[hhminatt$t==6,]
names(e6)[3] <- "minatt"

T1 <- left_join(T1, e1, by=c("t", "hhid"))
T2 <- left_join(T2, e2, by=c("t", "hhid"))
T3 <- left_join(T3, e3, by=c("t", "hhid"))
T4 <- left_join(T4, e4, by=c("t", "hhid"))
T5 <- left_join(T5, e4, by=c("t", "hhid"))
T6 <- left_join(T6, e6, by=c("t", "hhid"))

## piece panel back together

NGA_panel <- rbind(T1, T2, T3, T4, T5, T6)

## recode drinking water indicator
NGA_panel %<>% mutate(drinking_water = case_when(
  drinking_unit == 2 ~ as.numeric(drinking_time*120),
  drinking_unit == 1 ~ as.numeric(drinking_time*2))
)

## recode building structure

NGA_panel %<>% mutate(
  roof = case_when(
    material_roof == 1 |
      material_roof == 5 |
      material_roof == 6 |
      material_roof == 7 ~ 1,
    material_roof == 2 |
      material_roof == 3 |
      material_roof == 4 ~ 0,
    is.na(material_roof) ~ NA_real_),
  wall = case_when(
    material_wall < 5 ~ 1,
    material_wall > 4 ~ 0,
    is.na(material_wall) ~ NA_real_),
  floor = case_when(
    material_floor < 3 ~ 1,
    material_floor > 2 ~ 0,
    is.na(material_floor) ~ NA_real_)
)

## declare factor variables

NGA_panel$ID <-paste(as.character(NGA_panel$hhid),as.character(NGA_panel$indiv))
NGA_panel$zone <- factor(NGA_panel$zone, levels = c(1:6), labels = c("north central", "north east", "north west", "south east", "south south", "south west"))
NGA_panel$sector <- factor(NGA_panel$sector, levels = c(1,2), labels = c("urban", "rural"))
NGA_panel$sex <- factor(NGA_panel$sex, levels = c(1,2), labels = c("male", "female"))
NGA_panel %<>%
  mutate(visit = case_when(t==1 | t==3 | t==5 ~ 1,
                           t==2 | t==4 | t==6 ~ 2,
  ))

NGA_panel %<>% mutate( notenoughfood = case_when(
  !is.na(enough_food) &  enough_food > 1 ~ 2,
  !is.na(enough_food) &  enough_food == 1 ~ 1,
  is.na(enough_food) ~ NA_real_
))

NGA_panel$notenoughfood <- factor(NGA_panel$notenoughfood, levels = c(1,2), labels(c("yes","no")))
NGA_panel <- NGA_panel[,-366]

## compose MPI binary indicators (1=poor,0=not poor)

NGA_panel %<>% mutate(
  mpi_hunger = case_when(notenoughfood == 1 ~ 1,
                         notenoughfood == 2 ~ 0),
  mpi_educ_sixyrs = case_when(maxeduc <  16 ~ 1,
                              maxeduc >= 16 ~ 0,
                              is.na(maxeduc)~ NA_real_),
  mpi_educ_att = case_when(minatt==2 ~ 1,
                           minatt==1 ~ 0,
                           is.na(minatt) ~ NA_real_),
  mpi_fuel = case_when(fuel_cooking < 5 ~ 1,
                       fuel_cooking > 4 ~ 0,
                       is.na(fuel_cooking) ~ NA_real_),
  mpi_electricity = case_when(electricity==2 ~ 1,
                              electricity==1 ~ 0,
                              is.na(electricity) ~ NA_real_),
  # improved sanitation: poor is none, bucket, uncovered pit, other, or shared!
  mpi_sanitation = case_when(toilet_type == 0 |
                               toilet_type== 5|
                               toilet_type== 7|
                               toilet_type== 9 ~ 1,
                             toilet_type== 2 & toilet_shared==2 ~ 1,
                             toilet_type== 3 & toilet_shared==2 ~ 1,
                             toilet_type== 4 & toilet_shared==2 ~ 1,
                             toilet_type== 6 & toilet_shared==2 ~ 1,
                             toilet_type== 8 & toilet_shared==2 ~ 1,
                             toilet_type== 2 & toilet_shared!=2 ~ 0,
                             toilet_type== 3 & toilet_shared!=2 ~ 0,
                             toilet_type== 4 & toilet_shared!=2 ~ 0,
                             toilet_type== 6 & toilet_shared!=2 ~ 0,
                             toilet_type== 8 & toilet_shared!=2 ~ 0,
                             is.na(toilet_type) ~ NA_real_),
  mpi_water = case_when(drinking_water >= 30 ~ 1,
                        drinking_water < 30 ~ 0,
                        is.na(drinking_water) ~ NA_real_),
  mpi_building = case_when(roof == 1 | wall == 1 | floor == 1 ~ 1,
                           roof == 0 & wall == 0 & floor == 0 ~ 0),
  mpi_assets = case_when(amenities < 6 ~ 1,
                         amenities > 5 ~ 0)
) 

### compute MPI dimension scores

NGA_panel$MPI_health <- NGA_panel$mpi_hunger
NGA_panel$MPI_education <- NGA_panel$mpi_educ_att*0.5 + NGA_panel$mpi_educ_sixyrs*0.5
NGA_panel$MPI_poverty <- NGA_panel$mpi_fuel*(1/6) + NGA_panel$mpi_electricity*(1/6) +
  NGA_panel$mpi_sanitation*(1/6) + NGA_panel$mpi_water*(1/6) +
  NGA_panel$mpi_building*(1/6) + NGA_panel$mpi_assets*(1/6)

### aggregate MPI

NGA_panel$MPI <- (rowSums(NGA_panel[,(length(NGA_panel)-2):length(NGA_panel)], na.rm = T))
NGA_panel %<>% mutate(MPI = case_when(
  is.na(MPI_health) & is.na(MPI_education) & is.na(MPI_poverty) ~ NA_real_,
  is.na(MPI_health) & is.na(MPI_education) & !is.na(MPI_poverty) ~ NA_real_,#MPI_poverty,
  is.na(MPI_health) & !is.na(MPI_education) & is.na(MPI_poverty) ~ NA_real_,#MPI_education,
  !is.na(MPI_health) & is.na(MPI_education) & is.na(MPI_poverty) ~ NA_real_,#MPI_health,
  is.na(MPI_health) & !is.na(MPI_education) & !is.na(MPI_poverty) ~ (MPI_poverty + MPI_education)/2,
  !is.na(MPI_health) & !is.na(MPI_education) & is.na(MPI_poverty) ~ (MPI_health + MPI_education)/2,
  !is.na(MPI_health) & is.na(MPI_education) & !is.na(MPI_poverty) ~ (MPI_poverty + MPI_health)/2,
  !is.na(MPI_health) & !is.na(MPI_education) & !is.na(MPI_poverty) ~ (MPI_poverty + MPI_health +MPI_education)/3))

ggplot(data = NGA_panel, aes(x=MPI)) +
  geom_histogram(alpha=1, bins = 15)


#-------------- adding climate metrics ---------------------------------------------------------------------------------

# first reset the working directory
climate_dir <- "C:/Users/mathias/ROOT/EDU/UM/MA_MPP/MTH4911/MPP_Thesis/inputs/climate_data/cru_data"
setwd(climate_dir)
set.seed(2)
file_paths <- fs::dir_ls() # query the directory for list of items in it
ncin <- lapply(file_paths, raster::brick) # load NetCDF files into SpatialRaster
poly <- readRDS("C:/Users/mathias/ROOT/EDU/UM/MA_MPP/MTH4911/MPP_Thesis/inputs/shapes/gadm36_NGA_1_sf.rds") %>% 
  st_transform(proj4string(ncin$cru_ts4.05.1901.2020.cld.dat.nc))# %>% st_simplify(0.01, preserveTopology = TRUE)

extent(poly)

ncin %<>% lapply(raster::crop, poly, snap = "out") # clip raster outside the poly border
# 10 raster bricks with dimensions (lat, lon, t) (20,25,1440)

# The time frame of interest starts on "2000-01-16" and ends on "2021-12-16".

tm_nga = c(1177:1440)
ncin %<>% lapply(raster::subset, tm_nga) # crop to time frame
# 10 raster bricks with dimensions (lat, lon, t) (20,25,264)
names(ncin) <- (substring(names(ncin),22,24)) # rename list items

NGA_spatial <- SpatialPointsDataFrame(                                 # create SPDF: SpatialPointsDataFrame()
  NGA_panel[108:107], proj4string=ncin$cld$X2020.12.16@crs, NGA_panel) # specify CRS proj4string: borrow CRS from chm 


#!!! FOR TESTING
# NGA_spatial <- NGA_spatial[sample(nrow(NGA_spatial), 1000), ] # LIMIT to 100 random rows  ###
#!!! FOR TESTING

library(doParallel) # uncomment for parallel computing below
ncores <- detectCores()
cl <- makePSOCKcluster(ncores)
registerDoParallel(cl)

tic() # start benchmarking

# extract values for households
data <- ncin %>% lapply(raster::extract,NGA_spatial,     # SPDF with centroids for circular buffer
                                 buffer = 20000,  # buffer size 20 KM
                                 fun=mean,        # what statistic to extract
                                 df=TRUE)         # create a data frame?
toc() # end benchmarking

stopCluster(cl) #uncomment for parallel computing above


# construct biannual variables:

rsomeImport <- function(dat, fun, nam){ # the function takes two inputs:
                              # the source and the desired function...
                              # fun is one of {sum, mean, min, max}
  
    temp <- select(dat, c("X2010.08.16":"X2010.12.16", # visit 1
                          "X2011.03.16":"X2011.07.16", # visit 2
                          "X2012.08.16":"X2012.12.16", # visit 3
                          "X2013.03.16":"X2013.07.16", # visit 4
                          "X2015.08.16":"X2015.12.16", # visit 5
                          "X2016.03.16":"X2016.07.16") # visit 6
                 )
  
  temp[, 1:5 ] %>% apply(1, FUN = fun) %>% tibble() -> var1
  temp[, 6:10] %>% apply(1, FUN = fun) %>% tibble() -> var2
  temp[,11:15] %>% apply(1, FUN = fun) %>% tibble() -> var3
  temp[,16:20] %>% apply(1, FUN = fun) %>% tibble() -> var4
  temp[,21:25] %>% apply(1, FUN = fun) %>% tibble() -> var5
  temp[,26:30] %>% apply(1, FUN = fun) %>% tibble() -> var6
  
  vars <-cbind(var1, var2, var3, var4, var5, var6)
  names(vars) <- nam
  vars
}

wet <- rsomeImport(data$wet, sum, c("wet_1", "wet_2", "wet_3", "wet_4", "wet_5", "wet_6")) # sum of monthly wet days
pre <- rsomeImport(data$pre, sum, c("pre_1", "pre_2", "pre_3", "pre_4", "pre_5", "pre_6")) # sum  monthly precipitation
tmp <- rsomeImport(data$tmp, mean,c("tmp_1", "tmp_2", "tmp_3", "tmp_4", "tmp_5", "tmp_6")) # mean monthly average temps
tmx <- rsomeImport(data$tmx, max, c("tmx_1", "tmx_2", "tmx_3", "tmx_4", "tmx_5", "tmx_6")) # max of monthly max temps
tmn <- rsomeImport(data$tmn, min, c("tmn_1", "tmn_2", "tmn_3", "tmn_4", "tmn_5", "tmn_6")) # min of monthly min temps

merged <- cbind(NGA_spatial, wet, pre, tmp, tmx, tmn) %>% as.data.frame()

merged %<>% mutate(
  wet = case_when(t==1 ~ wet_1,t==2 ~ wet_2,t==3 ~ wet_3,t==4 ~ wet_4,t==5 ~ wet_5,t==6 ~ wet_6),
  pre = case_when(t==1 ~ pre_1,t==2 ~ pre_2,t==3 ~ pre_3,t==4 ~ pre_4,t==5 ~ pre_5,t==6 ~ pre_6),
  tmp = case_when(t==1 ~ tmp_1,t==2 ~ tmp_2,t==3 ~ tmp_3,t==4 ~ tmp_4,t==5 ~ tmp_5,t==6 ~ tmp_6),
  tmx = case_when(t==1 ~ tmx_1,t==2 ~ tmx_2,t==3 ~ tmx_3,t==4 ~ tmx_4,t==5 ~ tmx_5,t==6 ~ tmx_6),
  tmn = case_when(t==1 ~ tmn_1,t==2 ~ tmn_2,t==3 ~ tmn_3,t==4 ~ tmn_4,t==5 ~ tmn_5,t==6 ~ tmn_6))
NGA_panel <- merged[,-(184:215)]

#-------------- save for analysis --------------------------------------------------------------------------------------

setwd("C:/Users/mathias/ROOT/EDU/UM/MA_MPP/MTH4911/MPP_Thesis/outputs")
save(NGA_panel, file="NEW_PANEL_final.RData")
library(foreign)
write.dta(NGA_panel, "NEW_PANEL_final.dta") 

rm(list = ls(all.names = TRUE)) #will clear all objects including hidden objects.
gc()
quit()