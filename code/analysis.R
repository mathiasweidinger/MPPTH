dir <- "C:/Users/mathias/ROOT/EDU/UM/MA_MPP/MTH4911/MPP_Thesis"
setwd(dir)
load("outputs/panel_unif.Rdata")
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

tic()

cor(NGA_panel$childworkchores, NGA_panel$tmp08, use = "complete.obs")

NGA_panel$ID <-paste(as.character(NGA_panel$hhid),as.character(NGA_panel$indiv))
NGA_panel$zone <- factor(NGA_panel$zone, levels = c(1:6), labels = c("north central", "north east", "north west", "south east", "south south", "south west"))
NGA_panel$sector <- factor(NGA_panel$sector, levels = c(1,2), labels = c("urban", "rural"))
NGA_panel$sex <- factor(NGA_panel$sex, levels = c(1,2), labels = c("male", "female"))
NGA_panel %<>%
  mutate(visit = case_when(t==1 | t==3 | t==5 ~ 1,
                           t==2 | t==4 | t==6 ~ 2,
  ))


attach(NGA_panel)
est1 <- plm(childworkchores ~  cld01 + pet01 + pre01 + tmn01 + tmp01 + tmx01 + vap01 + wet01 +
                               cld02 + pet02 + pre02 + tmn02 + tmp02 + tmx02 + vap02 + wet02 +
                               cld03 + pet03 + pre03 + tmn03 + tmp03 + tmx03 + vap03 + wet03 +
                               cld04 + pet04 + pre04 + tmn04 + tmp04 + tmx04 + vap04 + wet04 +
                               cld05 + pet05 + pre05 + tmn05 + tmp05 + tmx05 + vap05 + wet05 +
                               cld06 + pet06 + pre06 + tmn06 + tmp06 + tmx06 + vap06 + wet06 +
                               cld07 + pet07 + pre07 + tmn07 + tmp07 + tmx07 + vap07 + wet07 +
                               cld08 + pet08 + pre08 + tmn08 + tmp08 + tmx08 + vap08 + wet08 +
                               cld09 + pet09 + pre09 + tmn09 + tmp09 + tmx09 + vap09 + wet09 +
                               cld10 + pet10 + pre10 + tmn10 + tmp10 + tmx10 + vap10 + wet10 +
                               cld11 + pet11 + pre11 + tmn11 + tmp11 + tmx11 + vap11 + wet11 +
                               cld12 + pet12 + pre12 + tmn12 + tmp12 + tmx12 + vap12 + wet12 +
                               sex + age + sector + totcons,
                               data = NGA_panel, index = c("ID", "t"), model = "within")

est2 <- felm(childworkchores ~ cld01 + pet01 + pre01 + tmn01 + tmp01 + tmx01 + vap01 + wet01 +
                               cld02 + pet02 + pre02 + tmn02 + tmp02 + tmx02 + vap02 + wet02 +
                               cld03 + pet03 + pre03 + tmn03 + tmp03 + tmx03 + vap03 + wet03 +
                               cld04 + pet04 + pre04 + tmn04 + tmp04 + tmx04 + vap04 + wet04 +
                               cld05 + pet05 + pre05 + tmn05 + tmp05 + tmx05 + vap05 + wet05 +
                               cld06 + pet06 + pre06 + tmn06 + tmp06 + tmx06 + vap06 + wet06 +
                               cld07 + pet07 + pre07 + tmn07 + tmp07 + tmx07 + vap07 + wet07 +
                               cld08 + pet08 + pre08 + tmn08 + tmp08 + tmx08 + vap08 + wet08 +
                               cld09 + pet09 + pre09 + tmn09 + tmp09 + tmx09 + vap09 + wet09 +
                               cld10 + pet10 + pre10 + tmn10 + tmp10 + tmx10 + vap10 + wet10 +
                               cld11 + pet11 + pre11 + tmn11 + tmp11 + tmx11 + vap11 + wet11 +
                               cld12 + pet12 + pre12 + tmn12 + tmp12 + tmx12 + vap12 + wet12 +
                               sex + age + sector + totcons |
                               ID + lga + zone + visit, data = NGA_panel)

knots <- quantile(NGA_panel$cld01, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld01); degree = 3; intercept = FALSE
cld01_spl <- ns(NGA_panel$cld01, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$cld02, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld02); degree = 3; intercept = FALSE
cld02_spl <- ns(NGA_panel$cld02, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$cld03, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld03); degree = 3; intercept = FALSE
cld03_spl <- ns(NGA_panel$cld03, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$cld04, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld04); degree = 3; intercept = FALSE
cld04_spl <- ns(NGA_panel$cld04, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$cld05, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld05); degree = 3; intercept = FALSE
cld05_spl <- ns(NGA_panel$cld05, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$cld06, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld06); degree = 3; intercept = FALSE
cld06_spl <- ns(NGA_panel$cld06, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$cld07, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld07); degree = 3; intercept = FALSE
cld07_spl <- ns(NGA_panel$cld07, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$cld08, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld08); degree = 3; intercept = FALSE
cld08_spl <- ns(NGA_panel$cld08, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$cld09, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld09); degree = 3; intercept = FALSE
cld09_spl <- ns(NGA_panel$cld09, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$cld10, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld10); degree = 3; intercept = FALSE
cld10_spl <- ns(NGA_panel$cld10, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$cld11, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld11); degree = 3; intercept = FALSE
cld11_spl <- ns(NGA_panel$cld11, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$cld12, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$cld12); degree = 3; intercept = FALSE
cld12_spl <- ns(NGA_panel$cld12, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$pet01, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet01); degree = 3; intercept = FALSE
pet01_spl <- ns(NGA_panel$pet01, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$pet02, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet02); degree = 3; intercept = FALSE
pet02_spl <- ns(NGA_panel$pet02, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$pet03, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet03); degree = 3; intercept = FALSE
pet03_spl <- ns(NGA_panel$pet03, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$pet04, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet04); degree = 3; intercept = FALSE
pet04_spl <- ns(NGA_panel$pet04, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$pet05, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet05); degree = 3; intercept = FALSE
pet05_spl <- ns(NGA_panel$pet05, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$pet06, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet06); degree = 3; intercept = FALSE
pet06_spl <- ns(NGA_panel$pet06, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$pet07, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet07); degree = 3; intercept = FALSE
pet07_spl <- ns(NGA_panel$pet07, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$pet08, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet08); degree = 3; intercept = FALSE
pet08_spl <- ns(NGA_panel$pet08, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$pet09, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet09); degree = 3; intercept = FALSE
pet09_spl <- ns(NGA_panel$pet09, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$pet10, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet10); degree = 3; intercept = FALSE
pet10_spl <- ns(NGA_panel$pet10, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$pet11, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet11); degree = 3; intercept = FALSE
pet11_spl <- ns(NGA_panel$pet11, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$pet12, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pet12); degree = 3; intercept = FALSE
pet12_spl <- ns(NGA_panel$pet12, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$pre01, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre01); degree = 3; intercept = FALSE
pre01_spl <- ns(NGA_panel$pre01, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$pre02, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre02); degree = 3; intercept = FALSE
pre02_spl <- ns(NGA_panel$pre02, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$pre03, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre03); degree = 3; intercept = FALSE
pre03_spl <- ns(NGA_panel$pre03, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$pre04, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre04); degree = 3; intercept = FALSE
pre04_spl <- ns(NGA_panel$pre04, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$pre05, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre05); degree = 3; intercept = FALSE
pre05_spl <- ns(NGA_panel$pre05, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$pre06, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre06); degree = 3; intercept = FALSE
pre06_spl <- ns(NGA_panel$pre06, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$pre07, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre07); degree = 3; intercept = FALSE
pre07_spl <- ns(NGA_panel$pre07, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$pre08, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre08); degree = 3; intercept = FALSE
pre08_spl <- ns(NGA_panel$pre08, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$pre09, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre09); degree = 3; intercept = FALSE
pre09_spl <- ns(NGA_panel$pre09, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$pre10, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre10); degree = 3; intercept = FALSE
pre10_spl <- ns(NGA_panel$pre10, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$pre11, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre11); degree = 3; intercept = FALSE
pre11_spl <- ns(NGA_panel$pre11, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$pre12, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$pre12); degree = 3; intercept = FALSE
pre12_spl <- ns(NGA_panel$pre12, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmn01, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn01); degree = 3; intercept = FALSE
tmn01_spl <- ns(NGA_panel$tmn01, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmn02, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn02); degree = 3; intercept = FALSE
tmn02_spl <- ns(NGA_panel$tmn02, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmn03, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn03); degree = 3; intercept = FALSE
tmn03_spl <- ns(NGA_panel$tmn03, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmn04, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn04); degree = 3; intercept = FALSE
tmn04_spl <- ns(NGA_panel$tmn04, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmn05, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn05); degree = 3; intercept = FALSE
tmn05_spl <- ns(NGA_panel$tmn05, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmn06, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn06); degree = 3; intercept = FALSE
tmn06_spl <- ns(NGA_panel$tmn06, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmn07, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn07); degree = 3; intercept = FALSE
tmn07_spl <- ns(NGA_panel$tmn07, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmn08, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn08); degree = 3; intercept = FALSE
tmn08_spl <- ns(NGA_panel$tmn08, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmn09, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn09); degree = 3; intercept = FALSE
tmn09_spl <- ns(NGA_panel$tmn09, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmn10, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn10); degree = 3; intercept = FALSE
tmn10_spl <- ns(NGA_panel$tmn10, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmn11, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn11); degree = 3; intercept = FALSE
tmn11_spl <- ns(NGA_panel$tmn11, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmn12, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmn12); degree = 3; intercept = FALSE
tmn12_spl <- ns(NGA_panel$tmn12, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmp01, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp01); degree = 3; intercept = FALSE
tmp01_spl <- ns(NGA_panel$tmp01, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmp02, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp02); degree = 3; intercept = FALSE
tmp02_spl <- ns(NGA_panel$tmp02, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmp03, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp03); degree = 3; intercept = FALSE
tmp03_spl <- ns(NGA_panel$tmp03, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmp04, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp04); degree = 3; intercept = FALSE
tmp04_spl <- ns(NGA_panel$tmp04, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmp05, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp05); degree = 3; intercept = FALSE
tmp05_spl <- ns(NGA_panel$tmp05, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmp06, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp06); degree = 3; intercept = FALSE
tmp06_spl <- ns(NGA_panel$tmp06, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmp07, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp07); degree = 3; intercept = FALSE
tmp07_spl <- ns(NGA_panel$tmp07, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmp08, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp08); degree = 3; intercept = FALSE
tmp08_spl <- ns(NGA_panel$tmp08, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmp09, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp09); degree = 3; intercept = FALSE
tmp09_spl <- ns(NGA_panel$tmp09, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmp10, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp10); degree = 3; intercept = FALSE
tmp10_spl <- ns(NGA_panel$tmp10, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmp11, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp11); degree = 3; intercept = FALSE
tmp11_spl <- ns(NGA_panel$tmp11, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmp12, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmp12); degree = 3; intercept = FALSE
tmp12_spl <- ns(NGA_panel$tmp12, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmx01, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx01); degree = 3; intercept = FALSE
tmx01_spl <- ns(NGA_panel$tmx01, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmx02, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx02); degree = 3; intercept = FALSE
tmx02_spl <- ns(NGA_panel$tmx02, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmx03, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx03); degree = 3; intercept = FALSE
tmx03_spl <- ns(NGA_panel$tmx03, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmx04, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx04); degree = 3; intercept = FALSE
tmx04_spl <- ns(NGA_panel$tmx04, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmx05, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx05); degree = 3; intercept = FALSE
tmx05_spl <- ns(NGA_panel$tmx05, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmx06, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx06); degree = 3; intercept = FALSE
tmx06_spl <- ns(NGA_panel$tmx06, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmx07, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx07); degree = 3; intercept = FALSE
tmx07_spl <- ns(NGA_panel$tmx07, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmx08, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx08); degree = 3; intercept = FALSE
tmx08_spl <- ns(NGA_panel$tmx08, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmx09, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx09); degree = 3; intercept = FALSE
tmx09_spl <- ns(NGA_panel$tmx09, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmx10, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx10); degree = 3; intercept = FALSE
tmx10_spl <- ns(NGA_panel$tmx10, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmx11, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx11); degree = 3; intercept = FALSE
tmx11_spl <- ns(NGA_panel$tmx11, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$tmx12, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$tmx12); degree = 3; intercept = FALSE
tmx12_spl <- ns(NGA_panel$tmx12, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$vap01, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap01); degree = 3; intercept = FALSE
vap01_spl <- ns(NGA_panel$vap01, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$vap02, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap02); degree = 3; intercept = FALSE
vap02_spl <- ns(NGA_panel$vap02, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$vap03, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap03); degree = 3; intercept = FALSE
vap03_spl <- ns(NGA_panel$vap03, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$vap04, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap04); degree = 3; intercept = FALSE
vap04_spl <- ns(NGA_panel$vap04, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$vap05, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap05); degree = 3; intercept = FALSE
vap05_spl <- ns(NGA_panel$vap05, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$vap06, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap06); degree = 3; intercept = FALSE
vap06_spl <- ns(NGA_panel$vap06, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$vap07, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap07); degree = 3; intercept = FALSE
vap07_spl <- ns(NGA_panel$vap07, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$vap08, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap08); degree = 3; intercept = FALSE
vap08_spl <- ns(NGA_panel$vap08, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$vap09, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap09); degree = 3; intercept = FALSE
vap09_spl <- ns(NGA_panel$vap09, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$vap10, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap10); degree = 3; intercept = FALSE
vap10_spl <- ns(NGA_panel$vap10, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$vap11, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap11); degree = 3; intercept = FALSE
vap11_spl <- ns(NGA_panel$vap11, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$vap12, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$vap12); degree = 3; intercept = FALSE
vap12_spl <- ns(NGA_panel$vap12, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$wet01, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet01); degree = 3; intercept = FALSE
wet01_spl <- ns(NGA_panel$wet01, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$wet02, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet02); degree = 3; intercept = FALSE
wet02_spl <- ns(NGA_panel$wet02, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$wet03, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet03); degree = 3; intercept = FALSE
wet03_spl <- ns(NGA_panel$wet03, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$wet04, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet04); degree = 3; intercept = FALSE
wet04_spl <- ns(NGA_panel$wet04, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$wet05, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet05); degree = 3; intercept = FALSE
wet05_spl <- ns(NGA_panel$wet05, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$wet06, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet06); degree = 3; intercept = FALSE
wet06_spl <- ns(NGA_panel$wet06, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$wet07, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet07); degree = 3; intercept = FALSE
wet07_spl <- ns(NGA_panel$wet07, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$wet08, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet08); degree = 3; intercept = FALSE
wet08_spl <- ns(NGA_panel$wet08, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$wet09, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet09); degree = 3; intercept = FALSE
wet09_spl <- ns(NGA_panel$wet09, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$wet10, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet10); degree = 3; intercept = FALSE
wet10_spl <- ns(NGA_panel$wet10, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$wet11, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet11); degree = 3; intercept = FALSE
wet11_spl <- ns(NGA_panel$wet11, knots = knots, Boundary.knots = boundary, intercept=intercept)

knots <- quantile(NGA_panel$wet12, probs = c(0.25, 0.5, 0.75))
boundary <- range(NGA_panel$wet12); degree = 3; intercept = FALSE
wet12_spl <- ns(NGA_panel$wet12, knots = knots, Boundary.knots = boundary, intercept=intercept)

est3 <- felm(childworkchores ~  cld01_spl + pet01_spl + pre01_spl + tmn01_spl + tmp01_spl + tmx01_spl + vap01_spl + wet01_spl +
                                cld02_spl + pet02_spl + pre02_spl + tmn02_spl + tmp02_spl + tmx02_spl + vap02_spl + wet02_spl +
                                cld03_spl + pet03_spl + pre03_spl + tmn03_spl + tmp03_spl + tmx03_spl + vap03_spl + wet03_spl +
                                cld04_spl + pet04_spl + pre04_spl + tmn04_spl + tmp04_spl + tmx04_spl + vap04_spl + wet04_spl +
                                cld05_spl + pet05_spl + pre05_spl + tmn05_spl + tmp05_spl + tmx05_spl + vap05_spl + wet05_spl +
                                cld06_spl + pet06_spl + pre06_spl + tmn06_spl + tmp06_spl + tmx06_spl + vap06_spl + wet06_spl +
                                cld07_spl + pet07_spl + pre07_spl + tmn07_spl + tmp07_spl + tmx07_spl + vap07_spl + wet07_spl +
                                cld08_spl + pet08_spl + pre08_spl + tmn08_spl + tmp08_spl + tmx08_spl + vap08_spl + wet08_spl +
                                cld09_spl + pet09_spl + pre09_spl + tmn09_spl + tmp09_spl + tmx09_spl + vap09_spl + wet09_spl +
                                cld10_spl + pet10_spl + pre10_spl + tmn10_spl + tmp10_spl + tmx10_spl + vap10_spl + wet10_spl +
                                cld11_spl + pet11_spl + pre11_spl + tmn11_spl + tmp11_spl + tmx11_spl + vap11_spl + wet11_spl +
                                cld12_spl + pet12_spl + pre12_spl + tmn12_spl + tmp12_spl + tmx12_spl + vap12_spl + wet12_spl +
                                sex + age + sector + totcons |
                                ID + lga + zone + visit, data = NGA_panel)


stargazer(est1,est2,est3, type = "html", out = "outputs/est.html")

est4 <- plm(childworkchores ~  pre01 + tmn01 + tmp01 + tmx01+
              pre02 + tmn02 + tmp02 + tmx02+
              pre03 + tmn03 + tmp03 + tmx03+
              pre04 + tmn04 + tmp04 + tmx04+
              pre05 + tmn05 + tmp05 + tmx05+
              pre06 + tmn06 + tmp06 + tmx06+
              pre07 + tmn07 + tmp07 + tmx07+
              pre08 + tmn08 + tmp08 + tmx08+
              pre09 + tmn09 + tmp09 + tmx09+
              pre10 + tmn10 + tmp10 + tmx10+
              pre11 + tmn11 + tmp11 + tmx11+
              pre12 + tmn12 + tmp12 + tmx12+
              sex + age + sector + totcons,
              data = NGA_panel, index = c("ID", "t"),model = "between")

est5 <- felm(childworkchores ~ pre01 + tmn01 + tmp01 + tmx01 +
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
                               sex + age + sector + totcons |
                               ID + lga + zone + visit, data = NGA_panel)


est6 <- felm(childworkchores ~  pre01_spl + tmn01_spl + tmp01_spl + tmx01_spl +
                                pre02_spl + tmn02_spl + tmp02_spl + tmx02_spl +
                                pre03_spl + tmn03_spl + tmp03_spl + tmx03_spl +
                                pre04_spl + tmn04_spl + tmp04_spl + tmx04_spl +
                                pre05_spl + tmn05_spl + tmp05_spl + tmx05_spl +
                                pre06_spl + tmn06_spl + tmp06_spl + tmx06_spl +
                                pre07_spl + tmn07_spl + tmp07_spl + tmx07_spl +
                                pre08_spl + tmn08_spl + tmp08_spl + tmx08_spl +
                                pre09_spl + tmn09_spl + tmp09_spl + tmx09_spl +
                                pre10_spl + tmn10_spl + tmp10_spl + tmx10_spl +
                                pre11_spl + tmn11_spl + tmp11_spl + tmx11_spl +
                                pre12_spl + tmn12_spl + tmp12_spl + tmx12_spl +
                                sex + age + sector + totcons -1|
                                ID + lga + zone + visit, data = NGA_panel)

stargazer(est4,est5,est6, type = "html", out = "outputs/lim_est.html")

toc()

ggplot(data = NGA_panel, aes(x = childworkchores)) +
  geom_histogram(bins = 25, alpha = 0.3, fill = "blue", col= "blue") +
  theme_minimal()
