library(tidyverse)
library(magrittr)
library(fs)
library(data.table)
## set the directory
setwd ("/Users/mathias/ROOT/EDU/UM/MA_MPP/MTH4911/MPP_Thesis")

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

## load in panel data

load("outputs/panel_unif.Rdata") # load data with mean deviations in units!
meandev <- NGA_panel[,147:254]
rm(NGA_panel)
load("outputs/panel_unif_level.Rdata") # load data with mean deviations in percentages
NGA_panel %<>% cbind(meandev)


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

### compute MPI dimenstion scores

NGA_panel$MPI_health <- NGA_panel$mpi_hunger
NGA_panel$MPI_educaction <- NGA_panel$mpi_educ_att*0.5 + NGA_panel$mpi_educ_sixyrs*0.5
NGA_panel$MPI_poverty <- NGA_panel$mpi_fuel*(1/6) + NGA_panel$mpi_electricity*(1/6) +
                         NGA_panel$mpi_sanitation*(1/6) + NGA_panel$mpi_water*(1/6) +
                         NGA_panel$mpi_building*(1/6) + NGA_panel$mpi_assets*(1/6)

### aggregate MPI

NGA_panel$MPI <- (rowSums(NGA_panel[,395:397], na.rm = T))
NGA_panel$MPI <- NGA_panel$MPI/3

ggplot(data = NGA_panel, aes(x=MPI)) +
  geom_histogram(alpha=1, bins = 15)

## non-negative child work!
NGA_panel %<>% mutate(
  childlabour = case_when(
    !is.na(childworkchores) & childworkchores > 80 ~ NA_real_,
    !is.na(childworkchores) & childworkchores ==0 ~ NA_real_,
    !is.na(childworkchores) & childworkchores <= 80 & childworkchores >0 ~ as.numeric(childworkchores),
    is.na(childworkchores) ~ NA_real_
  )
)

save(NGA_panel, file = "outputs/NGA_panel_final.Rda") #save and you're done!
rm(list = ls(all=T))
quit()
