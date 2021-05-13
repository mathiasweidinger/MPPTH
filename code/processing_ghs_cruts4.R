# load packages
library(tidyverse)
library(magrittr)
library(lfe)
library(data.table)

# set working directory
dir <- "C:/Users/mathias/ROOT/EDU/UM/MA_MPP/MTH4911/MPP_Thesis"
setwd(dir)

load("outputs/NGA_panel.Rda") # load cleaned household data

cru_lst <- as.list(fs::dir_ls("outputs/cru")) # fetch climate data names
lapply(cru_lst,load,.GlobalEnv) # load data into workspace
name_lst <-ls() # save names
rm(cru_lst) 
dfs <-  list(cld01,cld02,cld03,cld04,cld05,cld06,cld07,cld08,cld09,cld10,cld11,cld12,
             dtr01,dtr02,dtr03,dtr04,dtr05,dtr06,dtr07,dtr08,dtr09,dtr10,dtr11,dtr12,
             frs01,frs02,frs03,frs04,frs05,frs06,frs07,frs08,frs09,frs10,frs11,frs12,
             pet01,pet02,pet03,pet04,pet05,pet06,pet07,pet08,pet09,pet10,pet11,pet12,
             pre01,pre02,pre03,pre04,pre05,pre06,pre07,pre08,pre09,pre10,pre11,pre12,
             tmn01,tmn02,tmn03,tmn04,tmn05,tmn06,tmn07,tmn08,tmn09,tmn10,tmn11,tmn12,
             tmp01,tmp02,tmp03,tmp04,tmp05,tmp06,tmp07,tmp08,tmp09,tmp10,tmp11,tmp12,
             tmx01,tmx02,tmx03,tmx04,tmx05,tmx06,tmx07,tmx08,tmx09,tmx10,tmx11,tmx12,
             vap01,vap02,vap03,vap04,vap05,vap06,vap07,vap08,vap09,vap10,vap11,vap12,
             wet01,wet02,wet03,wet04,wet05,wet06,wet07,wet08,wet09,wet10,wet11,wet12)
dfs <- lapply(dfs, function(x){
  x <- cbind(x,rowMeans(x[1:10])) # 10 years averages 1999-2008,
  }
)

df_diffs <- lapply(dfs,function(x){
  as.data.frame(x <- cbind(
                x[,11]<-x[,11]-x[,23],x[,12]<-x[,12]-x[,23], # replace values with diffs from avg
                x[,13]<-x[,13]-x[,23],x[,14]<-x[,14]-x[,23],
                x[,15]<-x[,15]-x[,23],x[,16]<-x[,16]-x[,23],
                x[,17]<-x[,17]-x[,23],x[,18]<-x[,18]-x[,23],
                x[,19]<-x[,19]-x[,23],x[,20]<-x[,20]-x[,23],
                x[,21]<-x[,21]-x[,23],x[,22]<-x[,22]-x[,23],
                x[,23]))
  }
)

for (i in (1:120)){
  names(df_diffs[[i]])<- c(2009:2020,"rowMean9908") # attach names to variables
}
names_clim <- name_lst[c(-13,-14,-39)] # attach names to dataframes
names(df_diffs) <- names_clim # attach names to dataframes

all = ls()
rm(list= all[c(-13,-14,-43)]) #clear up workspace

####################################################################### tmx

cld <- NGA_panel$t # initiate df with time index from GHS
for(i in (1:12)){ # load in var
  cld <- cbind(cld,(df_diffs[[i]]))
}
dtr <- NGA_panel$t # initiate df with time index from GHS
for(i in (13:24)){ # load in var
  dtr <- cbind(dtr,(df_diffs[[i]]))
}
frs <- NGA_panel$t # initiate df with time index from GHS
for(i in (25:36)){ # load in var
  frs <- cbind(frs,(df_diffs[[i]]))
}
pet <- NGA_panel$t # initiate df with time index from GHS
for(i in (37:48)){ # load in var
  pet <- cbind(pet,(df_diffs[[i]]))
}
pre <- NGA_panel$t # initiate df with time index from GHS
for(i in (49:60)){ # load in var
  pre <- cbind(pre,(df_diffs[[i]]))
}
tmn <- NGA_panel$t # initiate df with time index from GHS
for(i in (61:72)){ # load in var
  tmn <- cbind(tmn,(df_diffs[[i]]))
}
tmp <- NGA_panel$t # initiate df with time index from GHS
for(i in (73:84)){ # load in var
  tmp <- cbind(tmp,(df_diffs[[i]]))
}
tmx <- NGA_panel$t # initiate df with time index from GHS
for(i in (85:96)){ # load in var
  tmx <- cbind(tmx,(df_diffs[[i]]))
}
vap <- NGA_panel$t # initiate df with time index from GHS
for(i in (97:108)){ # load in var
  vap <- cbind(vap,(df_diffs[[i]]))
}
wet <- NGA_panel$t # initiate df with time index from GHS
for(i in (109:120)){ # load in var
  wet <- cbind(wet,(df_diffs[[i]]))
}


generate <- function(var){
  
  names(var) <- c("t","012009","012010","012011","012012","012013","012014","012015","012016",
                  "012017","012018","012019","012020","01_avg",
                  "022009","022010","022011","022012","022013","022014","022015","022016",
                  "022017","022018","022019","022020","02_avg",
                  "032009","032010","032011","032012","032013","032014","032015","032016",
                  "032017","032018","032019","032020","03_avg",
                  "042009","042010","042011","042012","042013","042014","042015","042016",
                  "042017","042018","042019","042020","04_avg",
                  "052009","052010","052011","052012","052013","052014","052015","052016",
                  "052017","052018","052019","052020","05_avg",
                  "062009","062010","062011","062012","062013","062014","062015","062016",
                  "062017","062018","062019","062020","06_avg",
                  "072009","072010","072011","072012","072013","072014","072015","072016",
                  "072017","072018","072019","072020","07_avg",
                  "082009","082010","082011","082012","082013","082014","082015","082016",
                  "082017","082018","082019","082020","08_avg",
                  "092009","092010","092011","092012","092013","092014","092015","092016",
                  "092017","092018","092019","092020","09_avg",
                  "102009","102010","102011","102012","102013","102014","102015","102016",
                  "102017","102018","102019","102020","10_avg",
                  "112009","112010","112011","112012","112013","112014","112015","112016",
                  "112017","112018","112019","112020","11_avg",
                  "122009","122010","122011","122012","122013","122014","122015","122016",
                  "122017","122018","122019","122020","12_avg")
  var %<>%
    mutate(
      var_01 = case_when(
        t==1 ~ `012010`,  # t=1 -> 9:11 , 2010
        t==2 ~ `012011`,  # t=2 -> 2:4  , 2011
        t==3 ~ `012012`,  # t=3 -> 9:11 , 2012
        t==4 ~ `012013`,  # t=4 -> 2:4  , 2013
        t==5 ~ `012015`,  # t=5 -> 9:11 , 2015
        t==6 ~ `012016`   # t=6 -> 2:4  , 2016
      )
    )
  var %<>%
    mutate(
      var_02 = case_when(
        t==1 ~ `022010`,  # t=1 -> 9:11 , 2010
        t==2 ~ `022010`,  # t=2 -> 2:4  , 2011
        t==3 ~ `022012`,  # t=3 -> 9:11 , 2012
        t==4 ~ `022012`,  # t=4 -> 2:4  , 2013
        t==5 ~ `022015`,  # t=5 -> 9:11 , 2015
        t==6 ~ `022015`   # t=6 -> 2:4  , 2016
      )
    )
  var %<>%
    mutate(
      var_03 = case_when(
        t==1 ~ `032010`,  # t=1 -> 9:11 , 2010
        t==2 ~ `032010`,  # t=2 -> 2:4  , 2011
        t==3 ~ `032012`,  # t=3 -> 9:11 , 2012
        t==4 ~ `032012`,  # t=4 -> 2:4  , 2013
        t==5 ~ `032015`,  # t=5 -> 9:11 , 2015
        t==6 ~ `032015`   # t=6 -> 2:4  , 2016
      )
    )
  
  var %<>%
    mutate(
      var_04 = case_when(
        t==1 ~ `042010`,  # t=1 -> 9:11 , 2010
        t==2 ~ `042010`,  # t=2 -> 2:4  , 2011
        t==3 ~ `042012`,  # t=3 -> 9:11 , 2012
        t==4 ~ `042012`,  # t=4 -> 2:4  , 2013
        t==5 ~ `042015`,  # t=5 -> 9:11 , 2015
        t==6 ~ `042015`   # t=6 -> 2:4  , 2016
      )
    )
  
  var %<>%
    mutate(
      var_05 = case_when(
        t==1 ~ `052010`,  # t=1 -> 9:11 , 2010
        t==2 ~ `052010`,  # t=2 -> 2:4  , 2011
        t==3 ~ `052012`,  # t=3 -> 9:11 , 2012
        t==4 ~ `052012`,  # t=4 -> 2:4  , 2013
        t==5 ~ `052015`,  # t=5 -> 9:11 , 2015
        t==6 ~ `052015`   # t=6 -> 2:4  , 2016
      )
    )
  
  var %<>%
    mutate(
      var_06 = case_when(
        t==1 ~ `062010`,  # t=1 -> 9:11 , 2010
        t==2 ~ `062010`,  # t=2 -> 2:4  , 2011
        t==3 ~ `062012`,  # t=3 -> 9:11 , 2012
        t==4 ~ `062012`,  # t=4 -> 2:4  , 2013
        t==5 ~ `062015`,  # t=5 -> 9:11 , 2015
        t==6 ~ `062015`   # t=6 -> 2:4  , 2016
      )
    )
  
  var %<>%
    mutate(
      var_07 = case_when(
        t==1 ~ `072010`,  # t=1 -> 9:11 , 2010
        t==2 ~ `072010`,  # t=2 -> 2:4  , 2011
        t==3 ~ `072012`,  # t=3 -> 9:11 , 2012
        t==4 ~ `072012`,  # t=4 -> 2:4  , 2013
        t==5 ~ `072015`,  # t=5 -> 9:11 , 2015
        t==6 ~ `072015`   # t=6 -> 2:4  , 2016
      )
    )
  
  var %<>%
    mutate(
      var_08 = case_when(
        t==1 ~ `082010`,  # t=1 -> 9:11 , 2010
        t==2 ~ `082010`,  # t=2 -> 2:4  , 2011
        t==3 ~ `082012`,  # t=3 -> 9:11 , 2012
        t==4 ~ `082012`,  # t=4 -> 2:4  , 2013
        t==5 ~ `082015`,  # t=5 -> 9:11 , 2015
        t==6 ~ `082015`   # t=6 -> 2:4  , 2016
      )
    )
  
  var %<>%
    mutate(
      var_09 = case_when(
        t==1 ~ `092009`,  # t=1 -> 9:11 , 2010
        t==2 ~ `092010`,  # t=2 -> 2:4  , 2011
        t==3 ~ `092011`,  # t=3 -> 9:11 , 2012
        t==4 ~ `092012`,  # t=4 -> 2:4  , 2013
        t==5 ~ `092014`,  # t=5 -> 9:11 , 2015
        t==6 ~ `092015`   # t=6 -> 2:4  , 2016
      )
    )
  
  var %<>%
    mutate(
      var_10 = case_when(
        t==1 ~ `102009`,  # t=1 -> 9:11 , 2010
        t==2 ~ `102010`,  # t=2 -> 2:4  , 2011
        t==3 ~ `102011`,  # t=3 -> 9:11 , 2012
        t==4 ~ `102012`,  # t=4 -> 2:4  , 2013
        t==5 ~ `102014`,  # t=5 -> 9:11 , 2015
        t==6 ~ `102015`   # t=6 -> 2:4  , 2016
      )
    )
  var %<>%
    mutate(
      var_11 = case_when(
        t==1 ~ `112009`,  # t=1 -> 9:11 , 2010
        t==2 ~ `112010`,  # t=2 -> 2:4  , 2011
        t==3 ~ `112011`,  # t=3 -> 9:11 , 2012
        t==4 ~ `112012`,  # t=4 -> 2:4  , 2013
        t==5 ~ `112014`,  # t=5 -> 9:11 , 2015
        t==6 ~ `112015`   # t=6 -> 2:4  , 2016
      )
    )
  var %<>%
    mutate(
      var_12 = case_when(
        t==1 ~ `122009`,  # t=1 -> 9:11 , 2010
        t==2 ~ `122010`,  # t=2 -> 2:4  , 2011
        t==3 ~ `122011`,  # t=3 -> 9:11 , 2012
        t==4 ~ `122012`,  # t=4 -> 2:4  , 2013
        t==5 ~ `122014`,  # t=5 -> 9:11 , 2015
        t==6 ~ `122015`   # t=6 -> 2:4  , 2016
      )
    )
  var <- var[,158:169]
  return(var)
}

rm(frs) # no frost NGA

# generate output
cld <- generate(cld)
names(cld) <-c("cld01","cld02","cld03","cld04",
               "cld05","cld06","cld07","cld08",
               "cld09","cld10","cld11","cld12")
dtr <- generate(dtr)
names(dtr) <-c("dtr01","dtr02","dtr03","dtr04",
               "dtr05","dtr06","dtr07","dtr08",
               "dtr09","dtr10","dtr11","dtr12")
pet <- generate(pet)
names(pet) <-c("pet01","pet02","pet03","pet04",
               "pet05","pet06","pet07","pet08",
               "pet09","pet10","pet11","pet12")
pre <- generate(pre)
names(pre) <-c("pre01","pre02","pre03","pre04",
               "pre05","pre06","pre07","pre08",
               "pre09","pre10","pre11","pre12")
tmn <- generate(tmn)
names(tmn) <-c("tmn01","tmn02","tmn03","tmn04",
               "tmn05","tmn06","tmn07","tmn08",
               "tmn09","tmn10","tmn11","tmn12")
tmp <- generate(tmp)
names(tmp) <-c("tmp01","tmp02","tmp03","tmp04",
               "tmp05","tmp06","tmp07","tmp08",
               "tmp09","tmp10","tmp11","tmp12")
tmx <- generate(tmx)
names(tmx) <-c("tmx01","tmx02","tmx03","tmx04",
               "tmx05","tmx06","tmx07","tmx08",
               "tmx09","tmx10","tmx11","tmx12")
vap <- generate(vap)
names(vap) <-c("vap01","vap02","vap03","vap04",
               "vap05","vap06","vap07","vap08",
               "vap09","vap10","vap11","vap12")
wet <- generate(wet)
names(wet) <-c("wet01","wet02","wet03","wet04",
               "wet05","wet06","wet07","wet08",
               "wet09","wet10","wet11","wet12")

# merge with GHS data
NGA_panel %<>%
  cbind(cld) %>% 
  cbind(dtr) %>% 
  cbind(pet) %>% 
  cbind(pre) %>% 
  cbind(tmn) %>% 
  cbind(tmp) %>% 
  cbind(tmx) %>% 
  cbind(vap) %>% 
  cbind(wet)


# create DV Child Labour

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
NGA_

NGA_panel %<>%
  mutate(childworkchores = case_when(
    !is.na(childwork) & !is.na(childchores) ~ as.numeric(childwork+childchores),
    !is.na(childwork) & is.na(childchores) ~ childwork,
    is.na(childwork) & !is.na(childchores) ~ childchores,
    is.na(childwork) & is.na(childchores) ~ NA_real_
  ))

save(NGA_panel, file = "outputs/panel_unif.RData")
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()
quit()