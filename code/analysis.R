dir <- "C:/Users/mathias/ROOT/EDU/UM/MA_MPP/MTH4911/MPP_Thesis"
setwd(dir)

library(plm)
library(magrittr)
library(foreign)
library(tidyverse)
library(lmtest)
library(stargazer)
library(splines) # bsplines for spline estimation
library(survey) # svycontrast for prediction
library(lfe) # high dimensional FE 
library(scales) # transparency
library(data.table)
library(tictoc)
library(GGally)
library(PerformanceAnalytics)
library(cowplot)
library(lspline)
library(censReg)
library(AER)
library(caret)
library(pmdplyr)
library(panelr)
library(ggpubr)
#tic()

load("outputs/NGA_panel_final.Rda") # load final data

# distribution of IV

NGA_panel %>% 
  ggplot() +
  geom_histogram(aes(childworkchores), fill = "grey", color = "black") +
  theme_minimal() +
  xlab("weekly work hours") +
  ylab("count")


# summary statistics

require(reporttools)
data <- NGA_panel[NGA_panel$age<18,]
data$totcons <- data$totcons/1000
vars <- data[,c('childworkchores','hhsize','totcons','MPI','age')]
group <- data[,c('t')]

## display default statistics, only use a subset of observations, grouped analysis
tableContinuous(vars = vars, group = group, prec = 1, cap = "GHS Variables", lab = "tab: descr stat", na.rm=T, stats=c("n", "min", "q1", "median",
                                                                                                                       "mean", "q3", "max", "na"))

DF_long <- NGA_panel[,c(1:11, 36, 103, 147:254, 257:365, 383:384, 398)]# create subset relevant for analysis
DF_long <- cbind(DF_long$ID,DF_long[,-231])
names(DF_long)[1] <- "ID"
DF_long <- panel_data(DF_long, id=ID, wave = t) # declare a panel dataset
DF_wide <- widen_panel(DF_long, separator = "___") # transform to wide panel !!!THIS TAKES A WHILE!!!
DF_wide <- DF_wide[,order(colnames(DF_wide))] # order variables alphabetically

omit_cols <- c("age___1","age___2","age___3","age___4","age___5","age___6",
               "childworkchores___1","childworkchores___2","childworkchores___3",
               "childworkchores___4","childworkchores___5","childworkchores___6",
               "ea___1","ea___2","ea___3","ea___4","ea___5","ea___6","hhid",
               "hhsize___1","hhsize___2","hhsize___3","hhsize___4","hhsize___5","hhsize___6","ID","indiv",
               "lga___1","lga___2","lga___3","lga___4","lga___5","lga___6",
               "MPI___1","MPI___2","MPI___3","MPI___4","MPI___5","MPI___6",
               "relat_hhh___1","relat_hhh___2","relat_hhh___3","relat_hhh___4","relat_hhh___5","relat_hhh___6",
               "sector___1","sector___2","sector___3","sector___4","sector___5","sector___6",
               "sex___1","sex___2","sex___3","sex___4","sex___5","sex___6",
               "state___1","state___2","state___3","state___4","state___5","state___6", 
               "totcons___1","totcons___2","totcons___3","totcons___4","totcons___5","totcons___6",
               "visit___1","visit___2","visit___3","visit___4","visit___5","visit___6",
               "zone___1","zone___2","zone___3","zone___4","zone___5","zone___6"     
               )
DF_wide %>% select(omit_cols) -> DF_wide_nonclim
DF_wide %>% select(-one_of(omit_cols)) -> DF_wide_clim
DF_wide_nonclim <- DF_wide_nonclim[,-c(7:12,13:19,27:33,40:45,52:63,70:81)] # take out unnecessary vars


IVs <- cbind(DF_wide_nonclim, DF_wide_clim)

DVs <- NGA_panel[,c("t","ID","childworkchores")]

# add all lags/leads to all observations in all t=1,...,6
DF_tobits <- left_join(DVs, IVs, by = "ID")

### Jeff Wooldridge's heckit extension for unbalanced panel data

# 1. Estimate 6 huge tobits

DF_tob1 <- DF_tobits[DF_tobits$t==1,]
DF_tob1 %<>% mutate(selector = case_when(
  !is.na(childworkchores) & childworkchores > 0 ~ 1,
  childworkchores == 0 ~ 0,
  is.na(childworkchores) ~ NA_real_
))
DF_tob2 <- DF_tobits[DF_tobits$t==2,]
DF_tob3 <- DF_tobits[DF_tobits$t==3,]
DF_tob4 <- DF_tobits[DF_tobits$t==4,]
DF_tob5 <- DF_tobits[DF_tobits$t==5,]
DF_tob6 <- DF_tobits[DF_tobits$t==6,]
debugonce(censReg::censReg)
probit_1 <- glm( selector          ~    age___1+         age___2+         age___3+         age___4+         age___5+      
                                        age___6+         hhsize___1+      hhsize___2+      hhsize___3+      hhsize___4+      hhsize___5+      hhsize___6+      MPI___1+      
                                        MPI___2+         MPI___3+         MPI___4+         MPI___5+         MPI___6+         sector___1+      sector___2+      sector___3+   
                                        sector___4+      sector___5+      sector___6+      totcons___1+     totcons___2+     totcons___3+     totcons___4+     totcons___5+  
                                        totcons___6+     pre01___1+       pre01___2+       pre01___3+       pre01___4+       pre01___5+       pre01___6+       pre01_md___1+
                                        pre01_md___2+    pre01_md___3+    pre01_md___4+    pre01_md___5+    pre01_md___6+    pre02___1+       pre02___2+       pre02___3+      
                                        pre02___4+       pre02___5+       pre02___6+       pre02_md___1+    pre02_md___2+    pre02_md___3+    pre02_md___4+    pre02_md___5+   
                                        pre02_md___6+    pre03___1+       pre03___2+       pre03___3+       pre03___4+       pre03___5+       pre03___6+       pre03_md___1+   
                                        pre03_md___2+    pre03_md___3+    pre03_md___4+    pre03_md___5+    pre03_md___6+    pre04___1+       pre04___2+       pre04___3+      
                                        pre04___4+       pre04___5+       pre04___6+       pre04_md___1+    pre04_md___2+    pre04_md___3+    pre04_md___4+    pre04_md___5+   
                                        pre04_md___6+    pre05___1+       pre05___2+       pre05___3+       pre05___4+       pre05___5+       pre05___6+       pre05_md___1+   
                                        pre05_md___2+    pre05_md___3+    pre05_md___4+    pre05_md___5+    pre05_md___6+    pre06___1+       pre06___2+       pre06___3+      
                                        pre06___4+       pre06___5+       pre06___6+       pre06_md___1+    pre06_md___2+    pre06_md___3+    pre06_md___4+    pre06_md___5+   
                                        pre06_md___6+    pre07___1+       pre07___2+       pre07___3+       pre07___4+       pre07___5+       pre07___6+       pre07_md___1+   
                                        pre07_md___2+    pre07_md___3+    pre07_md___4+    pre07_md___5+    pre07_md___6+    pre08___1+       pre08___2+       pre08___3+      
                                        pre08___4+       pre08___5+       pre08___6+       pre08_md___1+    pre08_md___2+    pre08_md___3+    pre08_md___4+    pre08_md___5+   
                                        pre08_md___6+    pre09___1+       pre09___2+       pre09___3+       pre09___4+       pre09___5+       pre09___6+       pre09_md___1+   
                                        pre09_md___2+    pre09_md___3+    pre09_md___4+    pre09_md___5+    pre09_md___6+    pre10___1+       pre10___2+       pre10___3+      
                                        pre10___4+       pre10___5+       pre10___6+       pre10_md___1+    pre10_md___2+    pre10_md___3+    pre10_md___4+    pre10_md___5+   
                                        pre10_md___6+    pre11___1+       pre11___2+       pre11___3+       pre11___4+       pre11___5+       pre11___6+       pre11_md___1+   
                                        pre11_md___2+    pre11_md___3+    pre11_md___4+    pre11_md___5+    pre11_md___6+    pre12___1+       pre12___2+       pre12___3+      
                                        pre12___4+       pre12___5+       pre12___6+       pre12_md___1+    pre12_md___2+    pre12_md___3+    pre12_md___4+    pre12_md___5+   
                                        pre12_md___6+    tmn01___1+       tmn01___2+       tmn01___3+       tmn01___4+       tmn01___5+       tmn01___6+       tmn01_md___1+   
                                        tmn01_md___2+    tmn01_md___3+    tmn01_md___4+    tmn01_md___5+    tmn01_md___6+    tmn02___1+       tmn02___2+       tmn02___3+      
                                        tmn02___4+       tmn02___5+       tmn02___6+       tmn02_md___1+    tmn02_md___2+    tmn02_md___3+    tmn02_md___4+    tmn02_md___5+   
                                        tmn02_md___6+    tmn03___1+       tmn03___2+       tmn03___3+       tmn03___4+       tmn03___5+       tmn03___6+       tmn03_md___1+
                                        tmn03_md___2+    tmn03_md___3+    tmn03_md___4+    tmn03_md___5+    tmn03_md___6+    tmn04___1+       tmn04___2+       tmn04___3+      
                                        tmn04___4+       tmn04___5+       tmn04___6+       tmn04_md___1+    tmn04_md___2+    tmn04_md___3+    tmn04_md___4+    tmn04_md___5+   
                                        tmn04_md___6+    tmn05___1+       tmn05___2+       tmn05___3+       tmn05___4+       tmn05___5+       tmn05___6+       tmn05_md___1+   
                                        tmn05_md___2+    tmn05_md___3+    tmn05_md___4+    tmn05_md___5+    tmn05_md___6+    tmn06___1+       tmn06___2+       tmn06___3+      
                                        tmn06___4+       tmn06___5+       tmn06___6+       tmn06_md___1+    tmn06_md___2+    tmn06_md___3+    tmn06_md___4+    tmn06_md___5+   
                                        tmn06_md___6+    tmn07___1+       tmn07___2+       tmn07___3+       tmn07___4+       tmn07___5+       tmn07___6+       tmn07_md___1+   
                                        tmn07_md___2+    tmn07_md___3+    tmn07_md___4+    tmn07_md___5+    tmn07_md___6+    tmn08___1+       tmn08___2+       tmn08___3+      
                                        tmn08___4+       tmn08___5+       tmn08___6+       tmn08_md___1+    tmn08_md___2+    tmn08_md___3+    tmn08_md___4+    tmn08_md___5+   
                                        tmn08_md___6+    tmn09___1+       tmn09___2+       tmn09___3+       tmn09___4+       tmn09___5+       tmn09___6+       tmn09_md___1+   
                                        tmn09_md___2+    tmn09_md___3+    tmn09_md___4+    tmn09_md___5+    tmn09_md___6+    tmn10___1+       tmn10___2+       tmn10___3+      
                                        tmn10___4+       tmn10___5+       tmn10___6+       tmn10_md___1+    tmn10_md___2+    tmn10_md___3+    tmn10_md___4+    tmn10_md___5+   
                                        tmn10_md___6+    tmn11___1+       tmn11___2+       tmn11___3+       tmn11___4+       tmn11___5+       tmn11___6+       tmn11_md___1+   
                                        tmn11_md___2+    tmn11_md___3+    tmn11_md___4+    tmn11_md___5+    tmn11_md___6+    tmn12___1+       tmn12___2+       tmn12___3+      
                                        tmn12___4+       tmn12___5+       tmn12___6+       tmn12_md___1+    tmn12_md___2+    tmn12_md___3+    tmn12_md___4+    tmn12_md___5+   
                                        tmn12_md___6+    tmp01___1+       tmp01___2+       tmp01___3+       tmp01___4+       tmp01___5+       tmp01___6+       tmp01_md___1+   
                                        tmp01_md___2+    tmp01_md___3+    tmp01_md___4+    tmp01_md___5+    tmp01_md___6+    tmp02___1+       tmp02___2+       tmp02___3+      
                                        tmp02___4+       tmp02___5+       tmp02___6+       tmp02_md___1+    tmp02_md___2+    tmp02_md___3+    tmp02_md___4+    tmp02_md___5+   
                                        tmp02_md___6+    tmp03___1+       tmp03___2+       tmp03___3+       tmp03___4+       tmp03___5+       tmp03___6+       tmp03_md___1+   
                                        tmp03_md___2+    tmp03_md___3+    tmp03_md___4+    tmp03_md___5+    tmp03_md___6+    tmp04___1+       tmp04___2+       tmp04___3+      
                                        tmp04___4+       tmp04___5+       tmp04___6+       tmp04_md___1+    tmp04_md___2+    tmp04_md___3+    tmp04_md___4+    tmp04_md___5+   
                                        tmp04_md___6+    tmp05___1+       tmp05___2+       tmp05___3+       tmp05___4+       tmp05___5+       tmp05___6+       tmp05_md___1+   
                                        tmp05_md___2+    tmp05_md___3+    tmp05_md___4+    tmp05_md___5+    tmp05_md___6+    tmp06___1+       tmp06___2+       tmp06___3+
                                        tmp06___4+       tmp06___5+       tmp06___6+       tmp06_md___1+    tmp06_md___2+    tmp06_md___3+    tmp06_md___4+    tmp06_md___5+   
                                        tmp06_md___6+    tmp07___1+       tmp07___2+       tmp07___3+       tmp07___4+       tmp07___5+       tmp07___6+       tmp07_md___1+   
                                        tmp07_md___2+    tmp07_md___3+    tmp07_md___4+    tmp07_md___5+    tmp07_md___6+    tmp08___1+       tmp08___2+       tmp08___3+      
                                        tmp08___4+       tmp08___5+       tmp08___6+       tmp08_md___1+    tmp08_md___2+    tmp08_md___3+    tmp08_md___4+    tmp08_md___5+   
                                        tmp08_md___6+    tmp09___1+       tmp09___2+       tmp09___3+       tmp09___4+       tmp09___5+       tmp09___6+       tmp09_md___1+   
                                        tmp09_md___2+    tmp09_md___3+    tmp09_md___4+    tmp09_md___5+    tmp09_md___6+    tmp10___1+       tmp10___2+       tmp10___3+      
                                        tmp10___4+       tmp10___5+       tmp10___6+       tmp10_md___1+    tmp10_md___2+    tmp10_md___3+    tmp10_md___4+    tmp10_md___5+   
                                        tmp10_md___6+    tmp11___1+       tmp11___2+       tmp11___3+       tmp11___4+       tmp11___5+       tmp11___6+       tmp11_md___1+   
                                        tmp11_md___2+    tmp11_md___3+    tmp11_md___4+    tmp11_md___5+    tmp11_md___6+    tmp12___1+       tmp12___2+       tmp12___3+      
                                        tmp12___4+       tmp12___5+       tmp12___6+       tmp12_md___1+    tmp12_md___2+    tmp12_md___3+    tmp12_md___4+    tmp12_md___5+   
                                        tmp12_md___6+    tmx01___1+       tmx01___2+       tmx01___3+       tmx01___4+       tmx01___5+       tmx01___6+       tmx01_md___1+   
                                        tmx01_md___2+    tmx01_md___3+    tmx01_md___4+    tmx01_md___5+    tmx01_md___6+    tmx02___1+       tmx02___2+       tmx02___3+      
                                        tmx02___4+       tmx02___5+       tmx02___6+       tmx02_md___1+    tmx02_md___2+    tmx02_md___3+    tmx02_md___4+    tmx02_md___5+   
                                        tmx02_md___6+    tmx03___1+       tmx03___2+       tmx03___3+       tmx03___4+       tmx03___5+       tmx03___6+       tmx03_md___1+   
                                        tmx03_md___2+    tmx03_md___3+    tmx03_md___4+    tmx03_md___5+    tmx03_md___6+    tmx04___1+       tmx04___2+       tmx04___3+      
                                        tmx04___4+       tmx04___5+       tmx04___6+       tmx04_md___1+    tmx04_md___2+    tmx04_md___3+    tmx04_md___4+    tmx04_md___5+   
                                        tmx04_md___6+    tmx05___1+       tmx05___2+       tmx05___3+       tmx05___4+       tmx05___5+       tmx05___6+       tmx05_md___1+   
                                        tmx05_md___2+    tmx05_md___3+    tmx05_md___4+    tmx05_md___5+    tmx05_md___6+    tmx06___1+       tmx06___2+       tmx06___3+      
                                        tmx06___4+       tmx06___5+       tmx06___6+       tmx06_md___1+    tmx06_md___2+    tmx06_md___3+    tmx06_md___4+    tmx06_md___5+   
                                        tmx06_md___6+    tmx07___1+       tmx07___2+       tmx07___3+       tmx07___4+       tmx07___5+       tmx07___6+       tmx07_md___1+   
                                        tmx07_md___2+    tmx07_md___3+    tmx07_md___4+    tmx07_md___5+    tmx07_md___6+    tmx08___1+       tmx08___2+       tmx08___3+      
                                        tmx08___4+       tmx08___5+       tmx08___6+       tmx08_md___1+    tmx08_md___2+    tmx08_md___3+    tmx08_md___4+    tmx08_md___5+  
                                        tmx08_md___6+    tmx09___1+       tmx09___2+       tmx09___3+       tmx09___4+       tmx09___5+       tmx09___6+       tmx09_md___1+   
                                        tmx09_md___2+    tmx09_md___3+    tmx09_md___4+    tmx09_md___5+    tmx09_md___6+    tmx10___1+       tmx10___2+       tmx10___3+      
                                        tmx10___4+       tmx10___5+       tmx10___6+       tmx10_md___1+    tmx10_md___2+    tmx10_md___3+    tmx10_md___4+    tmx10_md___5+   
                                        tmx10_md___6+    tmx11___1+       tmx11___2+       tmx11___3+       tmx11___4+       tmx11___5+       tmx11___6+       tmx11_md___1+   
                                        tmx11_md___2+    tmx11_md___3+    tmx11_md___4+    tmx11_md___5+    tmx11_md___6+    tmx12___1+       tmx12___2+       tmx12___3+      
                                        tmx12___4+       tmx12___5+       tmx12___6+       tmx12_md___1+    tmx12_md___2+    tmx12_md___3+    tmx12_md___4+    tmx12_md___5+   
                                        tmx12_md___6, family = binomial(link = "probit"), data = DF_tob1)

summary(probit_1)
stargazer(tobit_1, type = "text")




# SPLINES FOR LEVELS:

knots <- quantile(NGA_panel$cld01, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld01); degree = 3; intercept = FALSE
cld01_spl <- ns(NGA_panel$cld01, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$cld02, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld02); degree = 3; intercept = FALSE
cld02_spl <- ns(NGA_panel$cld02, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$cld03, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld03); degree = 3; intercept = FALSE
cld03_spl <- ns(NGA_panel$cld03, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$cld04, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld04); degree = 3; intercept = FALSE
cld04_spl <- ns(NGA_panel$cld04, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$cld05, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld05); degree = 3; intercept = FALSE
cld05_spl <- ns(NGA_panel$cld05, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$cld06, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld06); degree = 3; intercept = FALSE
cld06_spl <- ns(NGA_panel$cld06, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$cld07, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld07); degree = 3; intercept = FALSE
cld07_spl <- ns(NGA_panel$cld07, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$cld08, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld08); degree = 3; intercept = FALSE
cld08_spl <- ns(NGA_panel$cld08, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$cld09, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld09); degree = 3; intercept = FALSE
cld09_spl <- ns(NGA_panel$cld09, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$cld10, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld10); degree = 3; intercept = FALSE
cld10_spl <- ns(NGA_panel$cld10, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$cld11, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld11); degree = 3; intercept = FALSE
cld11_spl <- ns(NGA_panel$cld11, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$cld12, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld12); degree = 3; intercept = FALSE
cld12_spl <- ns(NGA_panel$cld12, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pet01, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet01); degree = 3; intercept = FALSE
pet01_spl <- ns(NGA_panel$pet01, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pet02, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet02); degree = 3; intercept = FALSE
pet02_spl <- ns(NGA_panel$pet02, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pet03, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet03); degree = 3; intercept = FALSE
pet03_spl <- ns(NGA_panel$pet03, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pet04, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet04); degree = 3; intercept = FALSE
pet04_spl <- ns(NGA_panel$pet04, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pet05, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet05); degree = 3; intercept = FALSE
pet05_spl <- ns(NGA_panel$pet05, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pet06, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet06); degree = 3; intercept = FALSE
pet06_spl <- ns(NGA_panel$pet06, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pet07, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet07); degree = 3; intercept = FALSE
pet07_spl <- ns(NGA_panel$pet07, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pet08, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet08); degree = 3; intercept = FALSE
pet08_spl <- ns(NGA_panel$pet08, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pet09, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet09); degree = 3; intercept = FALSE
pet09_spl <- ns(NGA_panel$pet09, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pet10, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet10); degree = 3; intercept = FALSE
pet10_spl <- ns(NGA_panel$pet10, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pet11, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet11); degree = 3; intercept = FALSE
pet11_spl <- ns(NGA_panel$pet11, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pet12, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet12); degree = 3; intercept = FALSE
pet12_spl <- ns(NGA_panel$pet12, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pre01, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre01); degree = 3; intercept = FALSE
pre01_spl <- ns(NGA_panel$pre01, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pre02, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre02); degree = 3; intercept = FALSE
pre02_spl <- ns(NGA_panel$pre02, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pre03, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre03); degree = 3; intercept = FALSE
pre03_spl <- ns(NGA_panel$pre03, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pre04, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre04); degree = 3; intercept = FALSE
pre04_spl <- ns(NGA_panel$pre04, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pre05, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre05); degree = 3; intercept = FALSE
pre05_spl <- ns(NGA_panel$pre05, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pre06, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre06); degree = 3; intercept = FALSE
pre06_spl <- ns(NGA_panel$pre06, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pre07, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre07); degree = 3; intercept = FALSE
pre07_spl <- ns(NGA_panel$pre07, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pre08, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre08); degree = 3; intercept = FALSE
pre08_spl <- ns(NGA_panel$pre08, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pre09, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre09); degree = 3; intercept = FALSE
pre09_spl <- ns(NGA_panel$pre09, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pre10, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre10); degree = 3; intercept = FALSE
pre10_spl <- ns(NGA_panel$pre10, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pre11, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre11); degree = 3; intercept = FALSE
pre11_spl <- ns(NGA_panel$pre11, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pre12, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre12); degree = 3; intercept = FALSE
pre12_spl <- ns(NGA_panel$pre12, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmn01, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn01); degree = 3; intercept = FALSE
tmn01_spl <- ns(NGA_panel$tmn01, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmn02, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn02); degree = 3; intercept = FALSE
tmn02_spl <- ns(NGA_panel$tmn02, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmn03, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn03); degree = 3; intercept = FALSE
tmn03_spl <- ns(NGA_panel$tmn03, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmn04, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn04); degree = 3; intercept = FALSE
tmn04_spl <- ns(NGA_panel$tmn04, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmn05, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn05); degree = 3; intercept = FALSE
tmn05_spl <- ns(NGA_panel$tmn05, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmn06, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn06); degree = 3; intercept = FALSE
tmn06_spl <- ns(NGA_panel$tmn06, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmn07, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn07); degree = 3; intercept = FALSE
tmn07_spl <- ns(NGA_panel$tmn07, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmn08, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn08); degree = 3; intercept = FALSE
tmn08_spl <- ns(NGA_panel$tmn08, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmn09, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn09); degree = 3; intercept = FALSE
tmn09_spl <- ns(NGA_panel$tmn09, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmn10, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn10); degree = 3; intercept = FALSE
tmn10_spl <- ns(NGA_panel$tmn10, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmn11, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn11); degree = 3; intercept = FALSE
tmn11_spl <- ns(NGA_panel$tmn11, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmn12, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn12); degree = 3; intercept = FALSE
tmn12_spl <- ns(NGA_panel$tmn12, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmp01, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp01); degree = 3; intercept = FALSE
tmp01_spl <- ns(NGA_panel$tmp01, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmp02, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp02); degree = 3; intercept = FALSE
tmp02_spl <- ns(NGA_panel$tmp02, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmp03, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp03); degree = 3; intercept = FALSE
tmp03_spl <- ns(NGA_panel$tmp03, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmp04, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp04); degree = 3; intercept = FALSE
tmp04_spl <- ns(NGA_panel$tmp04, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmp05, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp05); degree = 3; intercept = FALSE
tmp05_spl <- ns(NGA_panel$tmp05, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmp06, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp06); degree = 3; intercept = FALSE
tmp06_spl <- ns(NGA_panel$tmp06, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmp07, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp07); degree = 3; intercept = FALSE
tmp07_spl <- ns(NGA_panel$tmp07, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmp08, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp08); degree = 3; intercept = FALSE
tmp08_spl <- ns(NGA_panel$tmp08, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmp09, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp09); degree = 3; intercept = FALSE
tmp09_spl <- ns(NGA_panel$tmp09, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmp10, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp10); degree = 3; intercept = FALSE
tmp10_spl <- ns(NGA_panel$tmp10, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmp11, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp11); degree = 3; intercept = FALSE
tmp11_spl <- ns(NGA_panel$tmp11, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmp12, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp12); degree = 3; intercept = FALSE
tmp12_spl <- ns(NGA_panel$tmp12, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmx01, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx01); degree = 3; intercept = FALSE
tmx01_spl <- ns(NGA_panel$tmx01, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmx02, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx02); degree = 3; intercept = FALSE
tmx02_spl <- ns(NGA_panel$tmx02, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmx03, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx03); degree = 3; intercept = FALSE
tmx03_spl <- ns(NGA_panel$tmx03, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmx04, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx04); degree = 3; intercept = FALSE
tmx04_spl <- ns(NGA_panel$tmx04, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmx05, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx05); degree = 3; intercept = FALSE
tmx05_spl <- ns(NGA_panel$tmx05, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmx06, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx06); degree = 3; intercept = FALSE
tmx06_spl <- ns(NGA_panel$tmx06, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmx07, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx07); degree = 3; intercept = FALSE
tmx07_spl <- ns(NGA_panel$tmx07, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmx08, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx08); degree = 3; intercept = FALSE
tmx08_spl <- ns(NGA_panel$tmx08, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmx09, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx09); degree = 3; intercept = FALSE
tmx09_spl <- ns(NGA_panel$tmx09, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmx10, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx10); degree = 3; intercept = FALSE
tmx10_spl <- ns(NGA_panel$tmx10, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmx11, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx11); degree = 3; intercept = FALSE
tmx11_spl <- ns(NGA_panel$tmx11, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmx12, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx12); degree = 3; intercept = FALSE
tmx12_spl <- ns(NGA_panel$tmx12, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$vap01, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap01); degree = 3; intercept = FALSE
vap01_spl <- ns(NGA_panel$vap01, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$vap02, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap02); degree = 3; intercept = FALSE
vap02_spl <- ns(NGA_panel$vap02, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$vap03, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap03); degree = 3; intercept = FALSE
vap03_spl <- ns(NGA_panel$vap03, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$vap04, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap04); degree = 3; intercept = FALSE
vap04_spl <- ns(NGA_panel$vap04, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$vap05, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap05); degree = 3; intercept = FALSE
vap05_spl <- ns(NGA_panel$vap05, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$vap06, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap06); degree = 3; intercept = FALSE
vap06_spl <- ns(NGA_panel$vap06, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$vap07, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap07); degree = 3; intercept = FALSE
vap07_spl <- ns(NGA_panel$vap07, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$vap08, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap08); degree = 3; intercept = FALSE
vap08_spl <- ns(NGA_panel$vap08, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$vap09, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap09); degree = 3; intercept = FALSE
vap09_spl <- ns(NGA_panel$vap09, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$vap10, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap10); degree = 3; intercept = FALSE
vap10_spl <- ns(NGA_panel$vap10, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$vap11, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap11); degree = 3; intercept = FALSE
vap11_spl <- ns(NGA_panel$vap11, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$vap12, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap12); degree = 3; intercept = FALSE
vap12_spl <- ns(NGA_panel$vap12, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$wet01, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet01); degree = 3; intercept = FALSE
wet01_spl <- ns(NGA_panel$wet01, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$wet02, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet02); degree = 3; intercept = FALSE
wet02_spl <- ns(NGA_panel$wet02, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$wet03, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet03); degree = 3; intercept = FALSE
wet03_spl <- ns(NGA_panel$wet03, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$wet04, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet04); degree = 3; intercept = FALSE
wet04_spl <- ns(NGA_panel$wet04, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$wet05, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet05); degree = 3; intercept = FALSE
wet05_spl <- ns(NGA_panel$wet05, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$wet06, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet06); degree = 3; intercept = FALSE
wet06_spl <- ns(NGA_panel$wet06, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$wet07, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet07); degree = 3; intercept = FALSE
wet07_spl <- ns(NGA_panel$wet07, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$wet08, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet08); degree = 3; intercept = FALSE
wet08_spl <- ns(NGA_panel$wet08, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$wet09, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet09); degree = 3; intercept = FALSE
wet09_spl <- ns(NGA_panel$wet09, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$wet10, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet10); degree = 3; intercept = FALSE
wet10_spl <- ns(NGA_panel$wet10, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$wet11, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet11); degree = 3; intercept = FALSE
wet11_spl <- ns(NGA_panel$wet11, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$wet12, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet12); degree = 3; intercept = FALSE
wet12_spl <- ns(NGA_panel$wet12, knots = knots, Boundary.knots = boundary)

# SPLINES FOR MEAN DEVIATIONS:

knots <- quantile(NGA_panel$cld01_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld01_md); degree = 3; intercept = FALSE
cld01_md_spl <- ns(NGA_panel$cld01_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$cld02_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld02_md); degree = 3; intercept = FALSE
cld02_md_spl <- ns(NGA_panel$cld02_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$cld03_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld03_md); degree = 3; intercept = FALSE
cld03_md_spl <- ns(NGA_panel$cld03_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$cld04_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld04_md); degree = 3; intercept = FALSE
cld04_md_spl <- ns(NGA_panel$cld04_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$cld05_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld05_md); degree = 3; intercept = FALSE
cld05_md_spl <- ns(NGA_panel$cld05_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$cld06_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld06_md); degree = 3; intercept = FALSE
cld06_md_spl <- ns(NGA_panel$cld06_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$cld07_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld07_md); degree = 3; intercept = FALSE
cld07_md_spl <- ns(NGA_panel$cld07_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$cld08_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld08_md); degree = 3; intercept = FALSE
cld08_md_spl <- ns(NGA_panel$cld08_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$cld09_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld09_md); degree = 3; intercept = FALSE
cld09_md_spl <- ns(NGA_panel$cld09_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$cld10_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld10_md); degree = 3; intercept = FALSE
cld10_md_spl <- ns(NGA_panel$cld10_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$cld11_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld11_md); degree = 3; intercept = FALSE
cld11_md_spl <- ns(NGA_panel$cld11_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$cld12_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld12_md); degree = 3; intercept = FALSE
cld12_md_spl <- ns(NGA_panel$cld12_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pet01_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet01_md); degree = 3; intercept = FALSE
pet01_md_spl <- ns(NGA_panel$pet01_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pet02_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet02_md); degree = 3; intercept = FALSE
pet02_md_spl <- ns(NGA_panel$pet02_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pet03_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet03_md); degree = 3; intercept = FALSE
pet03_md_spl <- ns(NGA_panel$pet03_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pet04_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet04_md); degree = 3; intercept = FALSE
pet04_md_spl <- ns(NGA_panel$pet04_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pet05_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet05_md); degree = 3; intercept = FALSE
pet05_md_spl <- ns(NGA_panel$pet05_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pet06_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet06_md); degree = 3; intercept = FALSE
pet06_md_spl <- ns(NGA_panel$pet06_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pet07_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet07_md); degree = 3; intercept = FALSE
pet07_md_spl <- ns(NGA_panel$pet07_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pet08_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet08_md); degree = 3; intercept = FALSE
pet08_md_spl <- ns(NGA_panel$pet08_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pet09_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet09_md); degree = 3; intercept = FALSE
pet09_md_spl <- ns(NGA_panel$pet09_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pet10_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet10_md); degree = 3; intercept = FALSE
pet10_md_spl <- ns(NGA_panel$pet10_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pet11_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet11_md); degree = 3; intercept = FALSE
pet11_md_spl <- ns(NGA_panel$pet11_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pet12_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet12_md); degree = 3; intercept = FALSE
pet12_md_spl <- ns(NGA_panel$pet12_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pre01_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre01_md); degree = 3; intercept = FALSE
pre01_md_spl <- ns(NGA_panel$pre01_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pre02_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre02_md); degree = 3; intercept = FALSE
pre02_md_spl <- ns(NGA_panel$pre02_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pre03_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre03_md); degree = 3; intercept = FALSE
pre03_md_spl <- ns(NGA_panel$pre03_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pre04_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre04_md); degree = 3; intercept = FALSE
pre04_md_spl <- ns(NGA_panel$pre04_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pre05_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre05_md); degree = 3; intercept = FALSE
pre05_md_spl <- ns(NGA_panel$pre05_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pre06_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre06_md); degree = 3; intercept = FALSE
pre06_md_spl <- ns(NGA_panel$pre06_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pre07_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre07_md); degree = 3; intercept = FALSE
pre07_md_spl <- ns(NGA_panel$pre07_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pre08_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre08_md); degree = 3; intercept = FALSE
pre08_md_spl <- ns(NGA_panel$pre08_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pre09_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre09_md); degree = 3; intercept = FALSE
pre09_md_spl <- ns(NGA_panel$pre09_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pre10_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre10_md); degree = 3; intercept = FALSE
pre10_md_spl <- ns(NGA_panel$pre10_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pre11_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre11_md); degree = 3; intercept = FALSE
pre11_md_spl <- ns(NGA_panel$pre11_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$pre12_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre12_md); degree = 3; intercept = FALSE
pre12_md_spl <- ns(NGA_panel$pre12_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmn01_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn01_md); degree = 3; intercept = FALSE
tmn01_md_spl <- ns(NGA_panel$tmn01_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmn02_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn02_md); degree = 3; intercept = FALSE
tmn02_md_spl <- ns(NGA_panel$tmn02_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmn03_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn03_md); degree = 3; intercept = FALSE
tmn03_md_spl <- ns(NGA_panel$tmn03_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmn04_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn04_md); degree = 3; intercept = FALSE
tmn04_md_spl <- ns(NGA_panel$tmn04_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmn05_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn05_md); degree = 3; intercept = FALSE
tmn05_md_spl <- ns(NGA_panel$tmn05_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmn06_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn06_md); degree = 3; intercept = FALSE
tmn06_md_spl <- ns(NGA_panel$tmn06_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmn07_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn07_md); degree = 3; intercept = FALSE
tmn07_md_spl <- ns(NGA_panel$tmn07_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmn08_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn08_md); degree = 3; intercept = FALSE
tmn08_md_spl <- ns(NGA_panel$tmn08_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmn09_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn09_md); degree = 3; intercept = FALSE
tmn09_md_spl <- ns(NGA_panel$tmn09_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmn10_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn10_md); degree = 3; intercept = FALSE
tmn10_md_spl <- ns(NGA_panel$tmn10_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmn11_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn11_md); degree = 3; intercept = FALSE
tmn11_md_spl <- ns(NGA_panel$tmn11_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmn12_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn12_md); degree = 3; intercept = FALSE
tmn12_md_spl <- ns(NGA_panel$tmn12_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmp01_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp01_md); degree = 3; intercept = FALSE
tmp01_md_spl <- ns(NGA_panel$tmp01_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmp02_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp02_md); degree = 3; intercept = FALSE
tmp02_md_spl <- ns(NGA_panel$tmp02_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmp03_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp03_md); degree = 3; intercept = FALSE
tmp03_md_spl <- ns(NGA_panel$tmp03_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmp04_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp04_md); degree = 3; intercept = FALSE
tmp04_md_spl <- ns(NGA_panel$tmp04_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmp05_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp05_md); degree = 3; intercept = FALSE
tmp05_md_spl <- ns(NGA_panel$tmp05_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmp06_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp06_md); degree = 3; intercept = FALSE
tmp06_md_spl <- ns(NGA_panel$tmp06_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmp07_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp07_md); degree = 3; intercept = FALSE
tmp07_md_spl <- ns(NGA_panel$tmp07_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmp08_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp08_md); degree = 3; intercept = FALSE
tmp08_md_spl <- ns(NGA_panel$tmp08_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmp09_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp09_md); degree = 3; intercept = FALSE
tmp09_md_spl <- ns(NGA_panel$tmp09_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmp10_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp10_md); degree = 3; intercept = FALSE
tmp10_md_spl <- ns(NGA_panel$tmp10_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmp11_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp11_md); degree = 3; intercept = FALSE
tmp11_md_spl <- ns(NGA_panel$tmp11_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmp12_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp12_md); degree = 3; intercept = FALSE
tmp12_md_spl <- ns(NGA_panel$tmp12_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmx01_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx01_md); degree = 3; intercept = FALSE
tmx01_md_spl <- ns(NGA_panel$tmx01_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmx02_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx02_md); degree = 3; intercept = FALSE
tmx02_md_spl <- ns(NGA_panel$tmx02_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmx03_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx03_md); degree = 3; intercept = FALSE
tmx03_md_spl <- ns(NGA_panel$tmx03_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmx04_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx04_md); degree = 3; intercept = FALSE
tmx04_md_spl <- ns(NGA_panel$tmx04_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmx05_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx05_md); degree = 3; intercept = FALSE
tmx05_md_spl <- ns(NGA_panel$tmx05_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmx06_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx06_md); degree = 3; intercept = FALSE
tmx06_md_spl <- ns(NGA_panel$tmx06_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmx07_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx07_md); degree = 3; intercept = FALSE
tmx07_md_spl <- ns(NGA_panel$tmx07_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmx08_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx08_md); degree = 3; intercept = FALSE
tmx08_md_spl <- ns(NGA_panel$tmx08_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmx09_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx09_md); degree = 3; intercept = FALSE
tmx09_md_spl <- ns(NGA_panel$tmx09_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmx10_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx10_md); degree = 3; intercept = FALSE
tmx10_md_spl <- ns(NGA_panel$tmx10_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmx11_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx11_md); degree = 3; intercept = FALSE
tmx11_md_spl <- ns(NGA_panel$tmx11_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$tmx12_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx12_md); degree = 3; intercept = FALSE
tmx12_md_spl <- ns(NGA_panel$tmx12_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$vap01_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap01_md); degree = 3; intercept = FALSE
vap01_md_spl <- ns(NGA_panel$vap01_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$vap02_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap02_md); degree = 3; intercept = FALSE
vap02_md_spl <- ns(NGA_panel$vap02_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$vap03_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap03_md); degree = 3; intercept = FALSE
vap03_md_spl <- ns(NGA_panel$vap03_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$vap04_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap04_md); degree = 3; intercept = FALSE
vap04_md_spl <- ns(NGA_panel$vap04_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$vap05_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap05_md); degree = 3; intercept = FALSE
vap05_md_spl <- ns(NGA_panel$vap05_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$vap06_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap06_md); degree = 3; intercept = FALSE
vap06_md_spl <- ns(NGA_panel$vap06_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$vap07_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap07_md); degree = 3; intercept = FALSE
vap07_md_spl <- ns(NGA_panel$vap07_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$vap08_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap08_md); degree = 3; intercept = FALSE
vap08_md_spl <- ns(NGA_panel$vap08_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$vap09_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap09_md); degree = 3; intercept = FALSE
vap09_md_spl <- ns(NGA_panel$vap09_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$vap10_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap10_md); degree = 3; intercept = FALSE
vap10_md_spl <- ns(NGA_panel$vap10_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$vap11_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap11_md); degree = 3; intercept = FALSE
vap11_md_spl <- ns(NGA_panel$vap11_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$vap12_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap12_md); degree = 3; intercept = FALSE
vap12_md_spl <- ns(NGA_panel$vap12_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$wet01_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet01_md); degree = 3; intercept = FALSE
wet01_md_spl <- ns(NGA_panel$wet01_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$wet02_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet02_md); degree = 3; intercept = FALSE
wet02_md_spl <- ns(NGA_panel$wet02_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$wet03_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet03_md); degree = 3; intercept = FALSE
wet03_md_spl <- ns(NGA_panel$wet03_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$wet04_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet04_md); degree = 3; intercept = FALSE
wet04_md_spl <- ns(NGA_panel$wet04_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$wet05_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet05_md); degree = 3; intercept = FALSE
wet05_md_spl <- ns(NGA_panel$wet05_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$wet06_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet06_md); degree = 3; intercept = FALSE
wet06_md_spl <- ns(NGA_panel$wet06_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$wet07_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet07_md); degree = 3; intercept = FALSE
wet07_md_spl <- ns(NGA_panel$wet07_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$wet08_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet08_md); degree = 3; intercept = FALSE
wet08_md_spl <- ns(NGA_panel$wet08_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$wet09_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet09_md); degree = 3; intercept = FALSE
wet09_md_spl <- ns(NGA_panel$wet09_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$wet10_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet10_md); degree = 3; intercept = FALSE
wet10_md_spl <- ns(NGA_panel$wet10_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$wet11_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet11_md); degree = 3; intercept = FALSE
wet11_md_spl <- ns(NGA_panel$wet11_md, knots = knots, Boundary.knots = boundary)

knots <- quantile(NGA_panel$wet12_md, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet12_md); degree = 3; intercept = FALSE
wet12_md_spl <- ns(NGA_panel$wet12_md, knots = knots, Boundary.knots = boundary)












ggplot(data=NGA_panel, aes(x=childwork, color = t)) + 
  geom_histogram(alpha = 0.8, bins=37, fill = "#cc9a0e") +
  xlim(-1, 70) +
  xlab("Child labour weekly hours (excl. chores)") +
  theme_minimal()

ggplot(data=NGA_panel, aes(x=childworkchores)) + 
  geom_histogram(alpha = 0.8, bins = 37, fill = "#cc9a0e") +
  xlim(-1,70) +
  xlab("Child labour weekly hours (incl. chores)") +
  theme_minimal()

ggplot(data=NGA_panel, aes(x=childlabour)) + 
  geom_histogram(alpha = 0.8, bins = 100, fill = "#cc9a0e") +
  xlim(-1,70) +
  theme_minimal()

ggplot(data=NGA_panel, aes(x=childwork)) + 
  geom_histogram(alpha = 0.8, bins = 22, fill = "#cc9a0e") +
  xlim(-1,70) +
  theme_minimal()

summary(tobit_1)

vars <- NGA_panel[,c(1:11, 36, 103, 147:254, 257:365, 398)]

cor(vars,use = "complete.obs")


est1 <- tobit(childworkchores ~ pre01 + tmn01 + tmp01 + tmx01 +
                         pre02 + tmn02 + tmp02 + tmx02 +
                         pre03 + tmn03 + tmp03 + tmx03 +
                         pre04 + tmn04 + tmp04 + tmx04 +
                         pre05 + tmn05 + tmp05 + tmx05 +
                         pre06 + tmn06 + tmp06 + tmx06 +
                         pre07 + tmn07 + tmp07 + tmx07 +
                         pre08 + tmn08 + tmp08 + tmx08 +
                         pre09 + tmn09 + tmp09 + tmx09 +
                         pre10 + tmn10 + tmp10 + tmx10 +
                         pre11 + tmn11 + tmp11 + tmx11 +
                         pre12 + tmn12 + tmp12 + tmx12 +
                         pre01_md + tmn01_md + tmp01_md + tmx01_md +
                         pre02_md + tmn02_md + tmp02_md + tmx02_md +
                         pre03_md + tmn03_md + tmp03_md + tmx03_md +
                         pre04_md + tmn04_md + tmp04_md + tmx04_md +
                         pre05_md + tmn05_md + tmp05_md + tmx05_md +
                         pre06_md + tmn06_md + tmp06_md + tmx06_md +
                         pre07_md + tmn07_md + tmp07_md + tmx07_md +
                         pre08_md + tmn08_md + tmp08_md + tmx08_md +
                         pre09_md + tmn09_md + tmp09_md + tmx09_md +
                         pre10_md + tmn10_md + tmp10_md + tmx10_md +
                         pre11_md + tmn11_md + tmp11_md + tmx11_md +
                         pre12_md + tmn12_md + tmp12_md + tmx12_md +
                         sex + age + sector + totcons + hhsize + MPI,
                         left=0, right=Inf,data = NGA_panel[NGA_panel$t==1,], robust = T)

est2 <- tobit(childworkchores ~ pre01 + tmn01 + tmp01 + tmx01 +
                pre02 + tmn02 + tmp02 + tmx02 +
                pre03 + tmn03 + tmp03 + tmx03 +
                pre04 + tmn04 + tmp04 + tmx04 +
                pre05 + tmn05 + tmp05 + tmx05 +
                pre06 + tmn06 + tmp06 + tmx06 +
                pre07 + tmn07 + tmp07 + tmx07 +
                pre08 + tmn08 + tmp08 + tmx08 +
                pre09 + tmn09 + tmp09 + tmx09 +
                pre10 + tmn10 + tmp10 + tmx10 +
                pre11 + tmn11 + tmp11 + tmx11 +
                pre12 + tmn12 + tmp12 + tmx12 +
                pre01_md + tmn01_md + tmp01_md + tmx01_md +
                pre02_md + tmn02_md + tmp02_md + tmx02_md +
                pre03_md + tmn03_md + tmp03_md + tmx03_md +
                pre04_md + tmn04_md + tmp04_md + tmx04_md +
                pre05_md + tmn05_md + tmp05_md + tmx05_md +
                pre06_md + tmn06_md + tmp06_md + tmx06_md +
                pre07_md + tmn07_md + tmp07_md + tmx07_md +
                pre08_md + tmn08_md + tmp08_md + tmx08_md +
                pre09_md + tmn09_md + tmp09_md + tmx09_md +
                pre10_md + tmn10_md + tmp10_md + tmx10_md +
                pre11_md + tmn11_md + tmp11_md + tmx11_md +
                pre12_md + tmn12_md + tmp12_md + tmx12_md +
                sex + age + sector + totcons + hhsize + MPI,
              left=0, right=Inf,data = NGA_panel[NGA_panel$t==2,], robust = T)

est3 <- tobit(childworkchores ~ pre01 + tmn01 + tmp01 + tmx01 +
                pre02 + tmn02 + tmp02 + tmx02 +
                pre03 + tmn03 + tmp03 + tmx03 +
                pre04 + tmn04 + tmp04 + tmx04 +
                pre05 + tmn05 + tmp05 + tmx05 +
                pre06 + tmn06 + tmp06 + tmx06 +
                pre07 + tmn07 + tmp07 + tmx07 +
                pre08 + tmn08 + tmp08 + tmx08 +
                pre09 + tmn09 + tmp09 + tmx09 +
                pre10 + tmn10 + tmp10 + tmx10 +
                pre11 + tmn11 + tmp11 + tmx11 +
                pre12 + tmn12 + tmp12 + tmx12 +
                pre01_md + tmn01_md + tmp01_md + tmx01_md +
                pre02_md + tmn02_md + tmp02_md + tmx02_md +
                pre03_md + tmn03_md + tmp03_md + tmx03_md +
                pre04_md + tmn04_md + tmp04_md + tmx04_md +
                pre05_md + tmn05_md + tmp05_md + tmx05_md +
                pre06_md + tmn06_md + tmp06_md + tmx06_md +
                pre07_md + tmn07_md + tmp07_md + tmx07_md +
                pre08_md + tmn08_md + tmp08_md + tmx08_md +
                pre09_md + tmn09_md + tmp09_md + tmx09_md +
                pre10_md + tmn10_md + tmp10_md + tmx10_md +
                pre11_md + tmn11_md + tmp11_md + tmx11_md +
                pre12_md + tmn12_md + tmp12_md + tmx12_md +
                sex + age + sector + totcons + hhsize + MPI,
              left=0, right=Inf,data = NGA_panel[NGA_panel$t==3,], robust = T)

est4 <- tobit(childworkchores ~ pre01 + tmn01 + tmp01 + tmx01 +
                pre02 + tmn02 + tmp02 + tmx02 +
                pre03 + tmn03 + tmp03 + tmx03 +
                pre04 + tmn04 + tmp04 + tmx04 +
                pre05 + tmn05 + tmp05 + tmx05 +
                pre06 + tmn06 + tmp06 + tmx06 +
                pre07 + tmn07 + tmp07 + tmx07 +
                pre08 + tmn08 + tmp08 + tmx08 +
                pre09 + tmn09 + tmp09 + tmx09 +
                pre10 + tmn10 + tmp10 + tmx10 +
                pre11 + tmn11 + tmp11 + tmx11 +
                pre12 + tmn12 + tmp12 + tmx12 +
                pre01_md + tmn01_md + tmp01_md + tmx01_md +
                pre02_md + tmn02_md + tmp02_md + tmx02_md +
                pre03_md + tmn03_md + tmp03_md + tmx03_md +
                pre04_md + tmn04_md + tmp04_md + tmx04_md +
                pre05_md + tmn05_md + tmp05_md + tmx05_md +
                pre06_md + tmn06_md + tmp06_md + tmx06_md +
                pre07_md + tmn07_md + tmp07_md + tmx07_md +
                pre08_md + tmn08_md + tmp08_md + tmx08_md +
                pre09_md + tmn09_md + tmp09_md + tmx09_md +
                pre10_md + tmn10_md + tmp10_md + tmx10_md +
                pre11_md + tmn11_md + tmp11_md + tmx11_md +
                pre12_md + tmn12_md + tmp12_md + tmx12_md +
                sex + age + sector + totcons + hhsize + MPI,
              left=0, right=Inf,data = NGA_panel[NGA_panel$t==4,], robust = T)

est5 <- tobit(childworkchores ~ pre01 + tmn01 + tmp01 + tmx01 +
                pre02 + tmn02 + tmp02 + tmx02 +
                pre03 + tmn03 + tmp03 + tmx03 +
                pre04 + tmn04 + tmp04 + tmx04 +
                pre05 + tmn05 + tmp05 + tmx05 +
                pre06 + tmn06 + tmp06 + tmx06 +
                pre07 + tmn07 + tmp07 + tmx07 +
                pre08 + tmn08 + tmp08 + tmx08 +
                pre09 + tmn09 + tmp09 + tmx09 +
                pre10 + tmn10 + tmp10 + tmx10 +
                pre11 + tmn11 + tmp11 + tmx11 +
                pre12 + tmn12 + tmp12 + tmx12 +
                pre01_md + tmn01_md + tmp01_md + tmx01_md +
                pre02_md + tmn02_md + tmp02_md + tmx02_md +
                pre03_md + tmn03_md + tmp03_md + tmx03_md +
                pre04_md + tmn04_md + tmp04_md + tmx04_md +
                pre05_md + tmn05_md + tmp05_md + tmx05_md +
                pre06_md + tmn06_md + tmp06_md + tmx06_md +
                pre07_md + tmn07_md + tmp07_md + tmx07_md +
                pre08_md + tmn08_md + tmp08_md + tmx08_md +
                pre09_md + tmn09_md + tmp09_md + tmx09_md +
                pre10_md + tmn10_md + tmp10_md + tmx10_md +
                pre11_md + tmn11_md + tmp11_md + tmx11_md +
                pre12_md + tmn12_md + tmp12_md + tmx12_md +
                sex + age + sector + totcons + hhsize + MPI,
              left=0, right=Inf,data = NGA_panel[NGA_panel$t==5,], robust = T)

est6 <- tobit(childworkchores ~ pre01 + tmn01 + tmp01 + tmx01 +
                pre02 + tmn02 + tmp02 + tmx02 +
                pre03 + tmn03 + tmp03 + tmx03 +
                pre04 + tmn04 + tmp04 + tmx04 +
                pre05 + tmn05 + tmp05 + tmx05 +
                pre06 + tmn06 + tmp06 + tmx06 +
                pre07 + tmn07 + tmp07 + tmx07 +
                pre08 + tmn08 + tmp08 + tmx08 +
                pre09 + tmn09 + tmp09 + tmx09 +
                pre10 + tmn10 + tmp10 + tmx10 +
                pre11 + tmn11 + tmp11 + tmx11 +
                pre12 + tmn12 + tmp12 + tmx12 +
                pre01_md + tmn01_md + tmp01_md + tmx01_md +
                pre02_md + tmn02_md + tmp02_md + tmx02_md +
                pre03_md + tmn03_md + tmp03_md + tmx03_md +
                pre04_md + tmn04_md + tmp04_md + tmx04_md +
                pre05_md + tmn05_md + tmp05_md + tmx05_md +
                pre06_md + tmn06_md + tmp06_md + tmx06_md +
                pre07_md + tmn07_md + tmp07_md + tmx07_md +
                pre08_md + tmn08_md + tmp08_md + tmx08_md +
                pre09_md + tmn09_md + tmp09_md + tmx09_md +
                pre10_md + tmn10_md + tmp10_md + tmx10_md +
                pre11_md + tmn11_md + tmp11_md + tmx11_md +
                pre12_md + tmn12_md + tmp12_md + tmx12_md +
                sex + age + sector + totcons + hhsize + MPI,
              left=0, right=Inf,data = NGA_panel[NGA_panel$t==6,], robust = T)

summary(est1)
summary(est2)
summary(est3)
summary(est4)
summary(est5)
summary(est6)











# Residual Diagnostics
plot(density(resid(est2))) #A density plot
qqnorm(resid(est2)) # A quantile normal plot - good for checking normality
qqline(resid(est2))






stargazer(est1,est2, est3,est4,est5,est6, type = "html", out = "test.html")

(cbind(AIC(est1,est2),BIC(est1,est2)))
