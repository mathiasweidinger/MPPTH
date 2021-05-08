library(tidyverse)
library(magrittr)
library(fs)
library(data.table)
## set the directory
setwd ("/Users/mathias/ROOT/EDU/UM/MA_MPP/MTH4911/MPP_Thesis")

## import data for post-planting visit 2010 (t=1)
file_paths <- fs::dir_ls("inputs/lsms_data/NGA_2010_GHSP-W1_v03_M_CSV/PPW1/Household")
lst <- lapply(file_paths, read.csv)

roster_1 <- as.data.frame(lst[[2]]) %>% select("zone":"s1q3", "s1q4", "s1q8")
cover_1 <- as.data.frame(lst[[17]]) %>% select("zone":"ric","wt_wave1", "wt_combined")
educ_1 <- as.data.frame(lst[[3]]) %>% select("zone", "state",	"lga",	"sector",	"ea", "hhid", "indiv",
                                             "s2q3", "s2q4", "s2q6", "s2q7", "s2q9", "s2q10")
                                             #s2q9 and s2q10 is 2009-2010 school year
labor_1 <- as.data.frame(lst[[4]]) %>% select("zone", "state",	"lga", "sector", "ea", "hhid", "indiv",
                                              "s3q7", "s3q8","s3q14", "s3q18", "s3q21a","s3q21b",
                                              "s3q26", "s3q30", "s3q33a", "s3q33b","s3q37a", "s3q37b",
                                              "s3q38a", "s3q38b")
cons_1 <- as.data.frame(read.csv("inputs/lsms_data/NGA_2010_GHSP-W1_v03_M_CSV/cons_agg_wave1_visit1.csv",
                                     header = TRUE, sep = ",", dec = ".")) %>% select("zone":"hhweight")

## import data for post-harvest visit 2011 (t=2)
file_paths <- fs::dir_ls("inputs/lsms_data/NGA_2010_GHSP-W1_v03_M_CSV/PHW1/Household")
lst <- lapply(file_paths, read.csv)

roster_2 <- as.data.frame(lst[[17]]) %>%  select("zone":"s1q3", "s1q4", "s1q7")
cover_2 <- as.data.frame(lst[[29]]) %>% select("zone":"ric","wt_wave1", "wt_combined")
educ_2_new <- as.data.frame(lst[[18]]) %>% select("zone", "state",	"lga",	"sector",	"ea",	"hhid",
                                                  "indiv", "s2aq5", "s2aq6", "s2aq8", "s2aq9", "s2aq11",
                                                  "s2aq13", "s2aq14", "s2aq15")
                                                  # s2aq11 is 2009-2010, s2aq13:15 is 2010-2011
educ_2_old <- as.data.frame(lst[[19]]) %>% select("zone", "state",	"lga",	"sector",	"ea",	"hhid",
                                                  "indiv", "s2bq1", "s2bq2", "s2bq3")
                                                  # s2bq1:3 refer to 2010-2011
labor_2 <- as.data.frame(lst[[20]]) %>% select("zone", "state",	"lga",	"sector",	"ea",	"hhid",
                                               "indiv","s3aq4", "s3aq5","s3aq11", "s3aq15",
                                               "s3aq18a","s3aq18b", "s3aq23", "s3aq27", "s3aq30a",
                                               "s3aq30b","s3aq39a", "s3aq39b", "s3aq40a", "s3aq40b")
cons_2 <- as.data.frame(read.csv("inputs/lsms_data/NGA_2010_GHSP-W1_v03_M_CSV/cons_agg_wave1_visit2.csv",
                                 header = TRUE, sep = ",", dec = ".")) %>% select("zone":"hhweight")

## import data for post-planting visit 2012 (t=3)
file_paths <- fs::dir_ls("inputs/lsms_data/NGA_2012_GHSP-W2_v02_M_CSV/Post Planting Wave 2/Household")
lst <- lapply(file_paths, read.csv)

roster_3 <- as.data.frame(lst[[2]]) %>%  select("zone":"s1q3", "s1q6", "s1q8")
cover_3 <- as.data.frame(lst[[22]]) %>% select("zone":"wt_combined")
educ_3 <- as.data.frame(lst[[3]]) %>% select("zone", "state",	"lga",	"sector",	"ea",	"hhid", "indiv",
                                             "s2q4", "s2q5", "s2q7", "s2q8", "s2q10", "s2q11")
                                             #s2q10:11 is 2011-12
labor_3 <- as.data.frame(lst[[4]]) %>% select("zone", "state",	"lga",	"sector",	"ea",	"hhid",
                                              "indiv","s3aq7", "s3aq8","s3aq14", "s3aq18", "s3aq21a",
                                              "s3aq21b", "s3aq27", "s3aq31", "s3aq34a", "s3aq34b",
                                              "s3aq39a", "s3aq39b", "s3aq40a", "s3aq40b")
cons_3 <- as.data.frame(read.csv("inputs/lsms_data/NGA_2012_GHSP-W2_v02_M_CSV/cons_agg_wave2_visit1.csv",
                                 header = TRUE, sep = ",", dec = ".")) %>% select("zone":"hhweight")

## import data for post-harvest visit 2013 (t=4)
file_paths <- fs::dir_ls("inputs/lsms_data/NGA_2012_GHSP-W2_v02_M_CSV/Post Harvest Wave 2/Household")
lst <- lapply(file_paths, read.csv)

roster_4 <- as.data.frame(lst[[17]]) %>%  select("zone":"s1q3", "s1q4", "s1q7")
cover_4 <- as.data.frame(lst[[29]]) %>% select("zone":"wt_combined")
educ_4_new <- as.data.frame(lst[[18]]) %>% select("zone", "state",	"lga",	"sector",	"ea",	"hhid",
                                                  "indiv", "s2aq5", "s2aq6", "s2aq8", "s2aq9", "s2aq11",
                                                  "s2aq13", "s2aq14", "s2aq15")
                                                  # s2aq11 is 2011-2012, s2aq13:15 is 2012-2013.
educ_4_old <- as.data.frame(lst[[19]]) %>% select("zone", "state",	"lga",	"sector",	"ea",	"hhid",
                                                  "indiv","s2bq1a", "s2bq2","s2bq3")
labor_4 <- as.data.frame(lst[[20]]) %>% select("zone", "state",	"lga",	"sector",	"ea",	"hhid",
                                               "indiv","s3aq4", "s3aq5","s3aq11", "s3aq15",
                                               "s3aq18a1","s3aq18a2", "s3aq23", "s3aq27",
                                               "s3aq30a1", "s3aq30a2","s3aq39b1", "s3aq39b2",
                                               "s3aq40b1", "s3aq40b2")
cons_4 <- as.data.frame(read.csv("inputs/lsms_data/NGA_2012_GHSP-W2_v02_M_CSV/cons_agg_wave2_visit2.csv",
                                 header = TRUE, sep = ",", dec = ".")) %>% select("zone":"hhweight")

## import data for post-planting visit 2015 (t=5)
file_paths <- fs::dir_ls("inputs/lsms_data/NGA_2015_GHSP-W3_v02_M_CSV/P")
lst <- lapply(file_paths, read.csv)

roster_5 <- as.data.frame(lst[[14]]) %>%  select("zone":"s1q3","s1q6", "s1q8")
cover_5 <- as.data.frame(lst[[30]]) %>% select("zone":"wt_w1_w2_w3")
# MISSING: educ_5
labor_5 <- as.data.frame(lst[[15]]) %>% select("zone", "state",	"lga",	"sector",	"ea",	"hhid",
                                               "indiv","s3q7", "s3q8","s3q14", "s3q18", "s3q21a",
                                               "s3q21b", "s3q27", "s3q31", "s3q34a", "s3q34b",
                                               "s3q39_new", "s3q40_new")
cons_5 <- as.data.frame(read.csv("inputs/lsms_data/NGA_2015_GHSP-W3_v02_M_CSV/cons_agg_wave3_visit1.csv",
                                 header = TRUE, sep = ",", dec = ".")) %>% select("zone":"hhweight")

## import data for post-harvest visit 2016 (t=6)
file_paths <- fs::dir_ls("inputs/lsms_data/NGA_2015_GHSP-W3_v02_M_CSV/H")
lst <- lapply(file_paths, read.csv)

roster_6 <- as.data.frame(lst[[19]]) %>% select("zone":"s1q3", "s1q4", "s1q7")
cover_6 <- as.data.frame(lst[[46]]) %>% select("zone":"wt_w1_w2_w3")
educ_6 <- as.data.frame(lst[[20]]) %>% select("zone", "state",	"lga",	"sector",	"ea",	"hhid",
                                              "indiv", "s2aq5", "s2aq6", "s2aq8", "s2aq9", "s2aq13",
                                              "s2aq14", "s2aq15")
labor_6 <- as.data.frame(lst[[21]]) %>% select("zone", "state",	"lga",	"sector",	"ea",	"hhid",
                                               "indiv","s3q7", "s3q8","s3q14", "s3q18", "s3q21a",
                                               "s3q21b", "s3q27", "s3q31", "s3q34a", "s3q34b",
                                               "s3q39_new", "s3q40_new")
cons_6 <- as.data.frame(read.csv("inputs/lsms_data/NGA_2015_GHSP-W3_v02_M_CSV/cons_agg_wave3_visit2.csv",
                                 header = TRUE, sep = ",", dec = ".")) %>% select("zone":"hhweight")

# There are several flaws with wave 4 data:
#   - We have no geo-variables
#   - We have no consumption aggregates at all
#   - We lack weights for the panel application
#   - The response rate for most of the labour related questions is too low
#   - education is only available for t=8, but not for t=7
# 
# All of this would suggest leaving out wave four and restricting the analysis to t = {1, 2, 3, 4, 5, 6}

rm(lst) #remove list from memory
rm(file_paths) # remove file paths

# Add an ID to all of the partial data sets, name variables more intuitively, and fix minor issues
# (unify minutes and hours for hh_chores, etc.)

# t=1 (post-planting wave 1)
cover_1 <- cbind(t=1, cover_1)
roster_1 <- cbind(t=1, roster_1)
names(roster_1)[9:12] <- c("sex", "relat_hhh", "age", "marital_status")
educ_1 <- cbind(t=1, educ_1)
names(educ_1)[9:length(educ_1)] <- c("literate", "school_ever", "shoolstart_age", "highest_educ",
                                     "school_current", "school_current_whynot")
                                     # school_current* is 2009-2010.
labor_1 <- cbind(t=1, labor_1)
names(labor_1)[9:22] <-c("work_7d", "search_7d", "pri_sect", "pri_hrs_7d", "pri_wage", "pri_wage_int",
                         "sec_sect", "sec_hrs", "sec_wage", "sec_wage_int", "wood_hrs", "wood_min",
                         "water_hrs", "water_min")
# sum up total chore time in minutes
labor_1$wood_hrs <- labor_1$wood_hrs*60
labor_1$water_hrs <- labor_1$water_hrs*60
labor_1$wood <- rowSums(labor_1[,c("wood_hrs", "wood_min")], na.rm=TRUE) 
labor_1$water <- rowSums(labor_1[,c("water_hrs", "water_min")], na.rm=TRUE)
labor_1$chores <- rowSums(labor_1[,c("wood", "water")], na.rm=TRUE)
labor_1<-labor_1[-(19:22)] # delete temporary variables
cons_1 <- cbind(t=1, cons_1)

# t=2 (post-harvest wave 1)
cover_2 <- cbind(t=2, cover_2)
roster_2 <- cbind(t=2, roster_2)
names(roster_2)[9:12] <- c("sex", "relat_hhh", "age", "marital_status")
educ_2_new <- cbind(t=2, educ_2_new)
educ_2_new<-educ_2_new[!(is.na(educ_2_new$literate) & is.na(educ_2_new$school_ever) &
                           is.na(educ_2_new$shoolstart_age) & is.na(educ_2_new$highest_educ) &
                           is.na(educ_2_new$school_last) & is.na(educ_2_new$school_current) &
                           is.na(educ_2_new$school_current_whynot) &
                           is.na(educ_2_new$school_current_level)),]
names(educ_2_new)[9:length(educ_2_new)] <- c("literate", "school_ever", "shoolstart_age", "highest_educ",
                                             "school_last","school_current", "school_current_whynot",
                                             "school_current_level")
                                             # school_last is 2009-2010, school_current* is 2010-2011.
educ_2_old <- cbind(t=2, educ_2_old)
names(educ_2_old)[9:length(educ_2_old)] <- c("school_current", "school_current_whynot",
                                             "school_current_level") # all refer to 2010-2011.
labor_2 <- cbind(t=2, labor_2)
names(labor_2)[9:22] <-c("work_7d", "search_7d", "pri_sect", "pri_hrs_7d", "pri_wage", "pri_wage_int",
                         "sec_sect", "sec_hrs", "sec_wage", "sec_wage_int", "wood_hrs", "wood_min",
                         "water_hrs", "water_min")
# sum up total chore time in minutes
labor_2$wood_hrs <- labor_2$wood_hrs*60
labor_2$water_hrs <- labor_2$water_hrs*60
labor_2$wood <- rowSums(labor_2[,c("wood_hrs", "wood_min")], na.rm=TRUE)
labor_2$water <- rowSums(labor_2[,c("water_hrs", "water_min")], na.rm=TRUE)
labor_2$chores <- rowSums(labor_2[,c("wood", "water")], na.rm=TRUE)
labor_2<-labor_2[-(19:22)] # delete temporary variables
cons_2 <- cbind(t=2, cons_2)

# t=3 (post-planting wave 2)
cover_3 <- cbind(t=3, cover_3)
roster_3 <- cbind(t=3, roster_3)
names(roster_3)[9:12] <- c("sex", "relat_hhh", "age", "marital_status")
educ_3 <- cbind(t=3, educ_3)
names(educ_3)[9:length(educ_3)] <- c("literate", "school_ever", "schoolstart_age", "highest_educ",
                                     "school_current", "school_current_whynot")
                                     #school_current* is 2011-2012
labor_3 <- cbind(t=3, labor_3)
names(labor_3)[9:22] <-c("work_7d", "search_7d", "pri_sect", "pri_hrs_7d", "pri_wage", "pri_wage_int",
                         "sec_sect", "sec_hrs", "sec_wage", "sec_wage_int", "wood_hrs", "wood_min",
                         "water_hrs", "water_min")
# sum up total chore time in minutes
labor_3$wood_hrs <- labor_3$wood_hrs*60
labor_3$water_hrs <- labor_3$water_hrs*60
labor_3$wood <- rowSums(labor_3[,c("wood_hrs", "wood_min")], na.rm=TRUE) 
labor_3$water <- rowSums(labor_3[,c("water_hrs", "water_min")], na.rm=TRUE)
labor_3$chores <- rowSums(labor_3[,c("wood", "water")], na.rm=TRUE)
labor_3<-labor_3[-(19:22)] # delete temporary variables
cons_3 <- cbind(t=3, cons_3)

# t=4 (post-harvest wave 2)
cover_4 <- cbind(t=4, cover_4)
roster_4 <- cbind(t=4, roster_4)
names(roster_4)[9:12] <- c("sex", "relat_hhh", "age", "marital_status")
educ_4_new <- cbind(t=4, educ_4_new)
educ_4_new<-educ_4_new[!(is.na(educ_4_new$literate) & is.na(educ_4_new$school_ever) &
                           is.na(educ_4_new$shoolstart_age) & is.na(educ_4_new$highest_educ) &
                           is.na(educ_4_new$school_last) & is.na(educ_4_new$school_current) &
                           is.na(educ_4_new$school_current_whynot) &
                           is.na(educ_4_new$school_current_level)),]
names(educ_4_new)[9:length(educ_4_new)] <- c("literate", "school_ever", "shoolstart_age", "highest_educ",
                                             "school_last","school_current", "school_current_whynot",
                                             "school_current_level")
                                             # school_last is 2011-2012, school_current* is 2012-2013
educ_4_old <- cbind(t=4, educ_4_old)
names(educ_4_old)[9:length(educ_4_old)] <- c("school_current", "school_current_whynot",
                                             "school_current_level") # all refer to 2012-2013.
labor_4 <- cbind(t=4, labor_4)
names(labor_4)[9:22] <-c("work_7d", "search_7d", "pri_sect", "pri_hrs_7d", "pri_wage", "pri_wage_int",
                         "sec_sect", "sec_hrs", "sec_wage", "sec_wage_int", "wood_hrs", "wood_min",
                         "water_hrs", "water_min")
# sum up total chore time in minutes
labor_4$wood_hrs <- labor_4$wood_hrs*60
labor_4$water_hrs <- labor_4$water_hrs*60
labor_4$wood <- rowSums(labor_4[,c("wood_hrs", "wood_min")], na.rm=TRUE) 
labor_4$water <- rowSums(labor_4[,c("water_hrs", "water_min")], na.rm=TRUE)
labor_4$chores <- rowSums(labor_4[,c("wood", "water")], na.rm=TRUE)
labor_4<-labor_4[-(19:22)] # delete temporary variables
cons_4 <- cbind(t=4, cons_4)

# t=5 (post-planting wave 3)
cover_5 <- cbind(t=5, cover_5)
roster_5 <- cbind(t=5, roster_5)
roster_5 <- cbind(roster_5[1:2], rep(NA,32139), roster_5[3:11])
names(roster_5) <- c("t", "zone", "state", "lga", "sector", "ea", "hhid", "indiv", "sex", "relat_hhh",
                     "age", "marital_status")
# replace NA's with the state coded
# lga_codes are specific to the states's id codes
for (i in 1:37){
  roster_5$state[roster_5$lga < (i+1)*100 & is.na(roster_5$state)] <- i
} 
# MISSING: educ.dta_Pw3 <- cbind(t=5, educ.dta_Pw3)
# names(educ.dta_Pw3)[9:13] <- c("literate", "school_ever", "shoolstart_age", "highest_educ", "school_current")
labor_5 <- cbind(t=5, labor_5)
names(labor_5)[9:20] <-c("work_7d", "search_7d", "pri_sect", "pri_hrs_7d", "pri_wage", "pri_wage_int",
                         "sec_sect", "sec_hrs", "sec_wage", "sec_wage_int", "wood", "water")
# sum up total chore time in minutes
labor_5$chores <- rowSums(labor_5[,c("wood", "water")], na.rm=TRUE)
cons_5 <- cbind(t=5, cons_5)

# t=6 (post-harvest wave 3)
cover_6 <- cbind(t=6, cover_6)
roster_6 <- cbind(t=6, roster_6)
names(roster_6)[9:length(roster_6)] <- c("sex", "relat_hhh", "age", "marital_status")  
#roster_6 <- roster_6[-11]
educ_6 <- cbind(t=6, educ_6)
names(educ_6)[9:length(educ_6)] <- c("literate", "school_ever", "shoolstart_age", "highest_educ",
                                     "school_current", "school_current_whynot", "school_current_level")
                                     # school_current in 2015-2016.
labor_6 <- cbind(t=6, labor_6)
names(labor_6)[9:20] <-c("work_7d", "search_7d", "pri_sect", "pri_hrs_7d", "pri_wage", "pri_wage_int",
                         "sec_sect", "sec_hrs", "sec_wage", "sec_wage_int", "wood", "water")
# sum up total chore time in minutes
labor_6$chores <- rowSums(labor_6[,c("wood", "water")], na.rm=TRUE)
cons_6 <- cbind(t=6, cons_6)

# Education is mostly static in variables
# (e.g. a person who is literate at t=1 will not unlearn how to read or write in t=3)
# At t>1, education variables for *newly added* individuals are in section 2A, and updates
# on those already in the data in section 2B.

# First join the "constant" educ variables across years
educ_constant <- full_join(educ_1[1:12], educ_2_new[1:12])
educ_constant <- full_join(educ_constant, educ_3[1:12])
educ_constant <- full_join(educ_constant, educ_4_new[1:12])
educ_constant <- full_join(educ_constant, educ_6[1:12])

# Next, join the time-variable educ characteristics 
educ_variable <- full_join(educ_1[-(9:12)], educ_2_old)
educ_variable <- full_join(educ_variable, educ_2_new[-(9:12)])
educ_variable <- full_join(educ_variable, educ_3[-(9:12)])
educ_variable <- full_join(educ_variable, educ_4_new[-(9:12)])
educ_variable <- full_join(educ_variable, educ_4_old)
educ_variable <- full_join(educ_variable, educ_6[-(9:12)])

educ_panel <- left_join(educ_variable, educ_constant, by=c("t" = "t", "zone"= "zone", "state" = "state",
                                                           "lga" =	"lga", "sector" ="sector","ea" = "ea",
                                                           "hhid"= "hhid","indiv" = "indiv"),na.rm=TRUE)
# Now compile all other panels

roster_panel <- rbind(roster_1, roster_2, roster_3, roster_4, roster_5, roster_6)
cover_list <-list(cover_1, cover_2, cover_3, cover_4, cover_5, cover_6)
colnames <- c("t", "zone", "state", "lga", "sector", "ea", "ric", "wt_wave", "wt_combined")
cover_list <- lapply(cover_list, setNames, colnames)

cover_1 <- cover_list[[1]]
cover_2 <- cover_list[[2]]
cover_3 <- cover_list[[3]]
cover_4 <- cover_list[[4]]
cover_5 <- cover_list[[5]]
cover_6 <- cover_list[[6]]
                                                                 
cover_panel <- rbind(cover_1, cover_2, cover_3, cover_4, cover_5, cover_6)
cons_panel <- rbind(cons_1, cons_2, cons_3, cons_4, cons_5, cons_6)
labor_panel <- rbind(labor_1, labor_2, labor_3, labor_4, labor_5, labor_6)

NGA_panel <- merge(roster_panel, labor_panel, by = c("t", "zone", "state", "lga", "sector", "ea", "hhid", "indiv"))
NGA_panel <- merge(NGA_panel, educ_panel, by = c("t", "zone", "state", "lga", "sector", "ea", "hhid", "indiv"), all=T)

NGA_panel <- left_join(NGA_panel, cons_panel, by = c("t" = "t", "zone" = "zone", "state" = "state", "lga" = "lga",
                                                     "ea" = "ea", "hhid" = "hhid"))


    #COVER DOES NOT COMPILE CORRECTLY...jumps to 10 obs per person in a six year panel!

    # Leave out for not (i.e. no sampling weights from here, but hhweights from cons_agg!):
    #NGA_panel <- merge(NGA_panel, cover_panel, by = c("t" = "t", "zone" = "zone", "state" = "state",
                                              #        "lga" = "lga", "sector" = "sector", "ea" = "ea"))

# Lastly, import longitude and latitude for all four waves

# first add the wave variable to dataset

NGA_panel %<>% 
  mutate(wave = case_when(t == 1 | t == 2 ~ 1, # wave 1
                          t == 3 | t == 4 ~ 2, # wave 2
                          t == 5 | t == 6 ~ 3, # wave 3
                         ))


# import geo-information (for now limited to coordinates...but selection can/should be extended!)
geo_1 <- cbind(wave = 1, as.data.frame(read.csv("inputs/lsms_data/NGA_2010_GHSP-W1_v03_M_CSV/Geodata/nga_householdgeovariables_y1.csv")))
geo_1 %<>% rename(
  LON_DD_MOD = lon_dd_mod,
  LAT_DD_MOD = lat_dd_mod
)

geo_2 <- cbind(wave = 2, as.data.frame(read.csv("inputs/lsms_data/NGA_2012_GHSP-W2_v02_M_CSV/Geodata Wave 2/nga_householdgeovars_y2.csv")))

geo_3 <- cbind(wave = 3, as.data.frame(read.csv("inputs/lsms_data/NGA_2015_GHSP-W3_v02_M_CSV/nga_householdgeovars_y3.csv")))

str_sort(names(geo_1)) # varnames are different for different waves...need to adjust 
str_sort(names(geo_2)) # difference to t=1 is distY1Y2
str_sort(names(geo_3)) # difference to t=1 is distY1Y3, and...
# wavewise colnames, e.g. w1 = h2010_eviarea, h2010_evimax, h2010_grn, h2010_sen, h2010_tot, h2010_wetQ, h2010_wetQstart

geo_1 %<>% rename(h_eviarea = h2010_eviarea,
                  h_evimax = h2010_evimax,
                  h_grn = h2010_grn,
                  h_sen = h2010_sen,
                  h_tot = h2010_tot,
                  h_wetQ = h2010_wetQ,
                  h_wetQstart = h2010_wetQstart)
geo_1 <- cbind(dist_Y1=0, geo_1) # add variable to w1
geo_1 <- geo_1[,-11]

geo_2 %<>% rename(h_eviarea = h2012_eviarea,
                  h_evimax = h2012_evimax,
                  h_grn = h2012_grn,
                  h_sen = h2012_sen,
                  h_tot = h2012_tot,
                  h_wetQ = h2012_wetQ,
                  h_wetQstart = h2012_wetQstart,
                  dist_borderpost = dist_border2,
                  dist_popcenter = dist_popcenter2,
                  dist_road = dist_road2,
                  dist_Y1 = distY1Y2) # streamline naming
geo_2 <- geo_2[,-8]

geo_3 %<>% rename(h_eviarea = h2015_eviarea,
                  h_evimax = h2015_evimax,
                  h_grn = h2015_grn,
                  h_sen = h2015_sen,
                  h_tot = h2015_tot,
                  h_wetQ = h2015_wetQ,
                  h_wetQstart = h2015_wetQstart,
                  dist_borderpost = dist_border2,
                  dist_popcenter = dist_popcenter2,
                  dist_road = dist_road2,
                  dist_Y1 = distY1Y3) # streamline naming
geo_3 <- geo_3[,-20]

NGA_panel %<>% full_join(geo_1, by = c("wave", "zone", "state", "lga", "sector", "ea", "hhid"))

NGA_panel %<>% full_join(geo_2, by = c("wave", "zone", "state", "lga", "sector", "ea", "hhid"))
NGA_panel %<>% 
  mutate(LON_DD_MOD.x = case_when(wave == 2 & is.na(LON_DD_MOD.x) ~ LON_DD_MOD.y,
                                  wave != 2 ~ LON_DD_MOD.x
                                  ))
NGA_panel %<>% 
  mutate(LAT_DD_MOD.x = case_when(wave == 2 & is.na(LAT_DD_MOD.x) ~ LAT_DD_MOD.y,
                                  wave != 2 ~ LAT_DD_MOD.x
                                  ))
NGA_panel %<>% 
  mutate(af_bio_1.x = case_when(wave == 2 & is.na(af_bio_1.x) ~ af_bio_1.y,
                                wave != 2 ~ af_bio_1.x
                                ))
NGA_panel %<>% 
  mutate(af_bio_12.x = case_when(wave == 2 & is.na(af_bio_12.x) ~ af_bio_12.y,
                                 wave != 2 ~ af_bio_12.x
                                 ))
NGA_panel %<>% 
  mutate(af_bio_13.x = case_when(wave == 2 & is.na(af_bio_13.x) ~ af_bio_13.y,
                                 wave != 2 ~ af_bio_13.x
                                 ))
NGA_panel %<>% 
  mutate(af_bio_16.x = case_when(wave == 2 & is.na(af_bio_16.x) ~ af_bio_16.y,
                                 wave != 2 ~ af_bio_16.x
                                 ))
NGA_panel %<>% 
  mutate(af_bio_8.x = case_when(wave == 2 & is.na(af_bio_8.x) ~ af_bio_8.y,
                                wave != 2 ~ af_bio_8.x
                                ))
NGA_panel %<>% 
  mutate(afmnslp_pct.x = case_when(wave == 2 & is.na(afmnslp_pct.x) ~ afmnslp_pct.y,
                                   wave != 2 ~ afmnslp_pct.x
                                   ))
NGA_panel %<>% 
  mutate(anntot_avg.x = case_when(wave == 2 & is.na(anntot_avg.x) ~ anntot_avg.y,
                                  wave != 2 ~ anntot_avg.x
                                  ))
NGA_panel %<>% 
  mutate(dist_admctr.x = case_when(wave == 2 & is.na(dist_admctr.x) ~ dist_admctr.y,
                                   wave != 2 ~ dist_admctr.x
                                   ))
NGA_panel %<>% 
  mutate(dist_borderpost.x = case_when(wave == 2 & is.na(dist_borderpost.x) ~ dist_borderpost.y,
                                       wave != 2 ~ dist_borderpost.x
                                       ))
NGA_panel %<>% 
  mutate(dist_market.x = case_when(wave == 2 & is.na(dist_market.x) ~ dist_market.y,
                                   wave != 2 ~ dist_market.x
                                   ))
NGA_panel %<>% 
  mutate(dist_popcenter.x = case_when(wave == 2 & is.na(dist_popcenter.x) ~ dist_popcenter.y,
                                      wave != 2 ~ dist_popcenter.x
                                      ))
NGA_panel %<>% 
  mutate(dist_road.x = case_when(wave == 2 & is.na(dist_road.x) ~ dist_road.y,
                                 wave != 2 ~ dist_road.x
                                 ))
NGA_panel %<>% 
  mutate(dist_Y1.x = case_when(wave == 2 & is.na(dist_Y1.x) ~ dist_Y1.y,
                               wave != 2 ~ dist_Y1.x
                               ))
NGA_panel %<>% 
  mutate(eviarea_avg.x = case_when(wave == 2 & is.na(eviarea_avg.x) ~ eviarea_avg.y,
                                   wave != 2 ~ eviarea_avg.x
                                   ))
NGA_panel %<>% 
  mutate(evimax_avg.x = case_when(wave == 2 & is.na(evimax_avg.x) ~ evimax_avg.y,
                                  wave != 2 ~ evimax_avg.x
                                  ))
NGA_panel %<>% 
  mutate(fsrad3_agpct.x = case_when(wave == 2 & is.na(fsrad3_agpct.x) ~ fsrad3_agpct.y,
                                    wave != 2 ~ fsrad3_agpct.x
                                    ))
NGA_panel %<>% 
  mutate(fsrad3_lcmaj.x = case_when(wave == 2 & is.na(fsrad3_lcmaj.x) ~ fsrad3_lcmaj.y,
                                    wave != 2 ~ fsrad3_lcmaj.x
                                    ))
NGA_panel %<>% 
  mutate(grn_avg.x = case_when(wave == 2 & is.na(grn_avg.x) ~ grn_avg.y,
                               wave != 2 ~ grn_avg.x
                               ))
NGA_panel$h_eviarea.x <- as.numeric(NGA_panel$h_eviarea.x)
NGA_panel %<>% 
  mutate(h_eviarea.x = case_when(wave == 2 & is.na(h_eviarea.x) ~ h_eviarea.y,
                                 wave != 2 ~ h_eviarea.x
                                 ))
NGA_panel %<>% 
  mutate(h_evimax.x = case_when(wave == 2 & is.na(h_evimax.x) ~ h_evimax.y,
                                wave != 2 ~ h_evimax.x
                                ))
NGA_panel %<>% 
  mutate(h_grn.x = case_when(wave == 2 & is.na(h_grn.x) ~ h_grn.y,
                             wave != 2 ~ h_grn.x
                             ))
NGA_panel %<>% 
  mutate(h_sen.x = case_when(wave == 2 & is.na(h_sen.x) ~ h_sen.y,
                             wave != 2 ~ h_sen.x
                             ))
NGA_panel %<>% 
  mutate(h_tot.x = case_when(wave == 2 & is.na(h_tot.x) ~ h_tot.y,
                             wave != 2 ~ h_tot.x
                             ))
NGA_panel %<>% 
  mutate(h_wetQ.x = case_when(wave == 2 & is.na(h_wetQ.x) ~ h_wetQ.y,
                              wave != 2 ~ h_wetQ.x
                              ))
NGA_panel %<>% 
  mutate(h_wetQstart.x = case_when(wave == 2 & is.na(h_wetQstart.x) ~ h_wetQstart.y,
                                   wave != 2 ~ h_wetQstart.x
                                   ))
NGA_panel %<>% 
  mutate(sen_avg.x = case_when(wave == 2 & is.na(sen_avg.x) ~ sen_avg.y,
                               wave != 2 ~ sen_avg.x
                               ))
NGA_panel %<>% 
  mutate(sq2.x = case_when(wave == 2 & is.na(sq2.x) ~ sq2.y,
                           wave != 2 ~ sq2.x
                           ))
NGA_panel %<>% 
  mutate(sq1.x = case_when(wave == 2 & is.na(sq1.x) ~ sq1.y,
                           wave != 2 ~ sq1.x
                           ))
NGA_panel %<>% 
  mutate(sq3.x = case_when(wave == 2 & is.na(sq3.x) ~ sq3.y,
                           wave != 2 ~ sq3.x
                           ))
NGA_panel %<>% 
  mutate(sq4.x = case_when(wave == 2 & is.na(sq4.x) ~ sq4.y,
                           wave != 2 ~ sq4.x
  ))
NGA_panel %<>% 
  mutate(sq5.x = case_when(wave == 2 & is.na(sq5.x) ~ sq5.y,
                           wave != 2 ~ sq5.x
  ))
NGA_panel %<>% 
  mutate(sq6.x = case_when(wave == 2 & is.na(sq6.x) ~ sq6.y,
                           wave != 2 ~ sq6.x
  ))
NGA_panel %<>% 
  mutate(sq7.x = case_when(wave == 2 & is.na(sq7.x) ~ sq7.y,
                           wave != 2 ~ sq7.x
  ))
NGA_panel %<>% 
  mutate(srtm_nga.x = case_when(wave == 2 & is.na(srtm_nga.x) ~ srtm_nga.y,
                           wave != 2 ~ srtm_nga.x
  ))
NGA_panel %<>% 
  mutate(srtm_nga_5_15.x = case_when(wave == 2 & is.na(srtm_nga_5_15.x) ~ srtm_nga_5_15.y,
                                wave != 2 ~ srtm_nga_5_15.x
  ))
NGA_panel %<>% 
  mutate(ssa_aez09.x = case_when(wave == 2 & is.na(ssa_aez09.x) ~ ssa_aez09.y,
                                     wave != 2 ~ ssa_aez09.x
  ))
NGA_panel %<>% 
  mutate(twi_nga.x = case_when(wave == 2 & is.na(twi_nga.x) ~ twi_nga.y,
                                 wave != 2 ~ twi_nga.x
  ))
NGA_panel %<>% 
  mutate(wetQ_avg.x = case_when(wave == 2 & is.na(wetQ_avg.x) ~ wetQ_avg.y,
                               wave != 2 ~ wetQ_avg.x
  ))
NGA_panel %<>% 
  mutate(wetQ_avgstart.x = case_when(wave == 2 & is.na(wetQ_avgstart.x) ~ wetQ_avgstart.y,
                                wave != 2 ~ wetQ_avgstart.x
  ))

NGA_panel <- NGA_panel[,1:146]
NGA_panel %<>% 
  rename(dist_Y1 = dist_Y1.x,            
         LAT_DD_MOD = LAT_DD_MOD.x,
         LON_DD_MOD = LON_DD_MOD.x,
         dist_road = dist_road.x,
         dist_popcenter = dist_popcenter.x,
         dist_market = dist_market.x,
         dist_borderpost = dist_borderpost.x,
         dist_admctr = dist_admctr.x,
         af_bio_1 = af_bio_1.x,
         af_bio_8 = af_bio_8.x,
         af_bio_12 = af_bio_12.x,
         af_bio_13 = af_bio_13.x,
         af_bio_16 = af_bio_16.x,
         fsrad3_agpct = fsrad3_agpct.x,
         fsrad3_lcmaj = fsrad3_lcmaj.x,
         ssa_aez09 = ssa_aez09.x,
         afmnslp_pct = afmnslp_pct.x,
         srtm_nga = srtm_nga.x,
         twi_nga = twi_nga.x,
         srtm_nga_5_15 = srtm_nga_5_15.x,
         sq1 = sq1.x,
         sq2 = sq2.x,
         sq3 = sq3.x,
         sq4 = sq4.x,
         sq5 = sq5.x,
         sq6 = sq6.x,
         sq7 = sq7.x,
         anntot_avg = anntot_avg.x,
         wetQ_avg = wetQ_avg.x,
         wetQ_avgstart = wetQ_avgstart.x,
         h_tot = h_tot.x,
         h_wetQ = h_wetQ.x,
         h_wetQstart = h_wetQstart.x,
         eviarea_avg = eviarea_avg.x,
         evimax_avg = evimax_avg.x,
         grn_avg = grn_avg.x,
         sen_avg = sen_avg.x,
         h_eviarea = h_eviarea.x,
         h_evimax = h_evimax.x,
         h_grn = h_grn.x,
         h_sen = h_sen.x
         )    

NGA_panel %<>% full_join(geo_3, by = c("wave", "zone", "state", "lga", "sector", "ea", "hhid"))
NGA_panel %<>% 
  mutate(LON_DD_MOD.x = case_when(wave == 3 & is.na(LON_DD_MOD.x) ~ LON_DD_MOD.y,
                                  wave != 3 ~ LON_DD_MOD.x
  ))
NGA_panel %<>% 
  mutate(LAT_DD_MOD.x = case_when(wave == 3 & is.na(LAT_DD_MOD.x) ~ LAT_DD_MOD.y,
                                  wave != 3 ~ LAT_DD_MOD.x
  ))
NGA_panel %<>% 
  mutate(af_bio_1.x = case_when(wave == 3 & is.na(af_bio_1.x) ~ af_bio_1.y,
                                wave != 3 ~ af_bio_1.x
  ))
NGA_panel %<>% 
  mutate(af_bio_12.x = case_when(wave == 3 & is.na(af_bio_12.x) ~ af_bio_12.y,
                                 wave != 3 ~ af_bio_12.x
  ))
NGA_panel %<>% 
  mutate(af_bio_13.x = case_when(wave == 3 & is.na(af_bio_13.x) ~ af_bio_13.y,
                                 wave != 3 ~ af_bio_13.x
  ))
NGA_panel %<>% 
  mutate(af_bio_16.x = case_when(wave == 3 & is.na(af_bio_16.x) ~ af_bio_16.y,
                                 wave != 3 ~ af_bio_16.x
  ))
NGA_panel %<>% 
  mutate(af_bio_8.x = case_when(wave == 3 & is.na(af_bio_8.x) ~ af_bio_8.y,
                                wave != 3 ~ af_bio_8.x
  ))
NGA_panel %<>% 
  mutate(afmnslp_pct.x = case_when(wave == 3 & is.na(afmnslp_pct.x) ~ afmnslp_pct.y,
                                   wave != 3 ~ afmnslp_pct.x
  ))
NGA_panel %<>% 
  mutate(anntot_avg.x = case_when(wave == 3 & is.na(anntot_avg.x) ~ anntot_avg.y,
                                  wave != 3 ~ anntot_avg.x
  ))
NGA_panel %<>% 
  mutate(dist_admctr.x = case_when(wave == 3 & is.na(dist_admctr.x) ~ dist_admctr.y,
                                   wave != 3 ~ dist_admctr.x
  ))
NGA_panel %<>% 
  mutate(dist_borderpost.x = case_when(wave == 3 & is.na(dist_borderpost.x) ~ dist_borderpost.y,
                                       wave != 3 ~ dist_borderpost.x
  ))
NGA_panel %<>% 
  mutate(dist_market.x = case_when(wave == 3 & is.na(dist_market.x) ~ dist_market.y,
                                   wave != 3 ~ dist_market.x
  ))
NGA_panel %<>% 
  mutate(dist_popcenter.x = case_when(wave == 3 & is.na(dist_popcenter.x) ~ dist_popcenter.y,
                                      wave != 3 ~ dist_popcenter.x
  ))
NGA_panel %<>% 
  mutate(dist_road.x = case_when(wave == 3 & is.na(dist_road.x) ~ dist_road.y,
                                 wave != 3 ~ dist_road.x
  ))
NGA_panel$dist_Y1.x <-as.integer(NGA_panel$dist_Y1.x)
NGA_panel %<>% 
  mutate(dist_Y1.x = case_when(wave == 3 & is.na(dist_Y1.x) ~ dist_Y1.y,
                               wave != 3 ~ dist_Y1.x
  ))
NGA_panel %<>% 
  mutate(eviarea_avg.x = case_when(wave == 3 & is.na(eviarea_avg.x) ~ eviarea_avg.y,
                                   wave != 3 ~ eviarea_avg.x
  ))
NGA_panel %<>% 
  mutate(evimax_avg.x = case_when(wave == 3 & is.na(evimax_avg.x) ~ evimax_avg.y,
                                  wave != 3 ~ evimax_avg.x
  ))
NGA_panel %<>% 
  mutate(fsrad3_agpct.x = case_when(wave == 3 & is.na(fsrad3_agpct.x) ~ fsrad3_agpct.y,
                                    wave != 3 ~ fsrad3_agpct.x
  ))
NGA_panel %<>% 
  mutate(fsrad3_lcmaj.x = case_when(wave == 3 & is.na(fsrad3_lcmaj.x) ~ fsrad3_lcmaj.y,
                                    wave != 3 ~ fsrad3_lcmaj.x
  ))
NGA_panel %<>% 
  mutate(grn_avg.x = case_when(wave == 3 & is.na(grn_avg.x) ~ grn_avg.y,
                               wave != 3 ~ grn_avg.x
  ))
NGA_panel %<>% 
  mutate(h_eviarea.x = case_when(wave == 3 & is.na(h_eviarea.x) ~ h_eviarea.y,
                                 wave != 3 ~ h_eviarea.x
  ))
NGA_panel %<>% 
  mutate(h_evimax.x = case_when(wave == 3 & is.na(h_evimax.x) ~ h_evimax.y,
                                wave != 3 ~ h_evimax.x
  ))
NGA_panel %<>% 
  mutate(h_grn.x = case_when(wave == 3 & is.na(h_grn.x) ~ h_grn.y,
                             wave != 3 ~ h_grn.x
  ))
NGA_panel %<>% 
  mutate(h_sen.x = case_when(wave == 3 & is.na(h_sen.x) ~ h_sen.y,
                             wave != 3 ~ h_sen.x
  ))
NGA_panel %<>% 
  mutate(h_tot.x = case_when(wave == 3 & is.na(h_tot.x) ~ h_tot.y,
                             wave != 3 ~ h_tot.x
  ))
NGA_panel %<>% 
  mutate(h_wetQ.x = case_when(wave == 3 & is.na(h_wetQ.x) ~ h_wetQ.y,
                              wave != 3 ~ h_wetQ.x
  ))
NGA_panel %<>% 
  mutate(h_wetQstart.x = case_when(wave == 3 & is.na(h_wetQstart.x) ~ h_wetQstart.y,
                                   wave != 3 ~ h_wetQstart.x
  ))
NGA_panel %<>% 
  mutate(sen_avg.x = case_when(wave == 3 & is.na(sen_avg.x) ~ sen_avg.y,
                               wave != 3 ~ sen_avg.x
  ))
NGA_panel %<>% 
  mutate(sq2.x = case_when(wave == 3 & is.na(sq2.x) ~ sq2.y,
                           wave != 3 ~ sq2.x
  ))
NGA_panel %<>% 
  mutate(sq1.x = case_when(wave == 3 & is.na(sq1.x) ~ sq1.y,
                           wave != 3 ~ sq1.x
  ))
NGA_panel %<>% 
  mutate(sq3.x = case_when(wave == 3 & is.na(sq3.x) ~ sq3.y,
                           wave != 3 ~ sq3.x
  ))
NGA_panel %<>% 
  mutate(sq4.x = case_when(wave == 3 & is.na(sq4.x) ~ sq4.y,
                           wave != 3 ~ sq4.x
  ))
NGA_panel %<>% 
  mutate(sq5.x = case_when(wave == 3 & is.na(sq5.x) ~ sq5.y,
                           wave != 3 ~ sq5.x
  ))
NGA_panel %<>% 
  mutate(sq6.x = case_when(wave == 3 & is.na(sq6.x) ~ sq6.y,
                           wave != 3 ~ sq6.x
  ))
NGA_panel %<>% 
  mutate(sq7.x = case_when(wave == 3 & is.na(sq7.x) ~ sq7.y,
                           wave != 3 ~ sq7.x
  ))
NGA_panel %<>% 
  mutate(srtm_nga.x = case_when(wave == 3 & is.na(srtm_nga.x) ~ srtm_nga.y,
                                wave != 3 ~ srtm_nga.x
  ))
NGA_panel %<>% 
  mutate(srtm_nga_5_15.x = case_when(wave == 3 & is.na(srtm_nga_5_15.x) ~ srtm_nga_5_15.y,
                                     wave != 3 ~ srtm_nga_5_15.x
  ))
NGA_panel %<>% 
  mutate(ssa_aez09.x = case_when(wave == 3 & is.na(ssa_aez09.x) ~ ssa_aez09.y,
                                 wave != 3 ~ ssa_aez09.x
  ))
NGA_panel %<>% 
  mutate(twi_nga.x = case_when(wave == 3 & is.na(twi_nga.x) ~ twi_nga.y,
                               wave != 3 ~ twi_nga.x
  ))
NGA_panel %<>% 
  mutate(wetQ_avg.x = case_when(wave == 3 & is.na(wetQ_avg.x) ~ wetQ_avg.y,
                                wave != 3 ~ wetQ_avg.x
  ))
NGA_panel %<>% 
  mutate(wetQ_avgstart.x = case_when(wave == 3 & is.na(wetQ_avgstart.x) ~ wetQ_avgstart.y,
                                     wave != 3 ~ wetQ_avgstart.x
  ))
NGA_panel <- NGA_panel[,1:146]
NGA_panel %<>% 
  rename(dist_Y1 = dist_Y1.x,            
         LAT_DD_MOD = LAT_DD_MOD.x,
         LON_DD_MOD = LON_DD_MOD.x,
         dist_road = dist_road.x,
         dist_popcenter = dist_popcenter.x,
         dist_market = dist_market.x,
         dist_borderpost = dist_borderpost.x,
         dist_admctr = dist_admctr.x,
         af_bio_1 = af_bio_1.x,
         af_bio_8 = af_bio_8.x,
         af_bio_12 = af_bio_12.x,
         af_bio_13 = af_bio_13.x,
         af_bio_16 = af_bio_16.x,
         fsrad3_agpct = fsrad3_agpct.x,
         fsrad3_lcmaj = fsrad3_lcmaj.x,
         ssa_aez09 = ssa_aez09.x,
         afmnslp_pct = afmnslp_pct.x,
         srtm_nga = srtm_nga.x,
         twi_nga = twi_nga.x,
         srtm_nga_5_15 = srtm_nga_5_15.x,
         sq1 = sq1.x,
         sq2 = sq2.x,
         sq3 = sq3.x,
         sq4 = sq4.x,
         sq5 = sq5.x,
         sq6 = sq6.x,
         sq7 = sq7.x,
         anntot_avg = anntot_avg.x,
         wetQ_avg = wetQ_avg.x,
         wetQ_avgstart = wetQ_avgstart.x,
         h_tot = h_tot.x,
         h_wetQ = h_wetQ.x,
         h_wetQstart = h_wetQstart.x,
         eviarea_avg = eviarea_avg.x,
         evimax_avg = evimax_avg.x,
         grn_avg = grn_avg.x,
         sen_avg = sen_avg.x,
         h_eviarea = h_eviarea.x,
         h_evimax = h_evimax.x,
         h_grn = h_grn.x,
         h_sen = h_sen.x
  )    

NGA_panel <- NGA_panel[!is.na(NGA_panel$t), ] # delete added obs

sapply(NGA_panel[,106:107], function(x) sum(is.na(x))) # there are NAs in coordinates
table(NGA_panel$t, is.na(NGA_panel$LAT_DD_MOD)) # missing values are not systematically missing
NGA_panel <- NGA_panel[! is.na(NGA_panel$LAT_DD_MOD),]
NGA_panel <- NGA_panel[! is.na(NGA_panel$LON_DD_MOD),]

save(NGA_panel, file = "outputs/NGA_panel.Rda") #save and you're done!
rm(list = ls(all=T))
