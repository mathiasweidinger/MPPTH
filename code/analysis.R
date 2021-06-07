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
#tic()

load("outputs/NGA_panel_final.Rda") # load final data

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

### Jeff Wooldridge's heckit extension for unbalanced panel data...


# Step 1: For each t, estimate the equation h_it=max(0,x_i \delta_t + v_it)
#         by standard Tobit, where not x_i = (1, x_i1, x_i2,..., x_iT) and
#         \delta_t = (\delta_t0, \delta_t1',...,\delta_tT')'
#         
#         For s_it = 1, define \hat{v}_it = h_it - x_i \hat{delta_t}.


# Step 2: Estimate the equation y_it = x_it\beta + \rho v_it + error_it by
#         pooled OLS using those observations for which s_it=1, where



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























est1 <- plm(childwork ~ pre01 + tmn01 + tmp01 + tmx01 +
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
                         sex + age + sector + totcons + hhsize,
                         data = NGA_panel,
                         index =c("ID", "t"),
                         model = "within")

est2<- felm(childwork ~ pre01_spl + tmn01_spl + tmp01_spl + tmx01_spl +
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
                        pre01_md_spl + tmn01_md_spl + tmp01_md_spl + tmx01_md_spl +
                        pre02_md_spl + tmn02_md_spl + tmp02_md_spl + tmx02_md_spl +
                        pre03_md_spl + tmn03_md_spl + tmp03_md_spl + tmx03_md_spl +
                        pre04_md_spl + tmn04_md_spl + tmp04_md_spl + tmx04_md_spl +
                        pre05_md_spl + tmn05_md_spl + tmp05_md_spl + tmx05_md_spl +
                        pre06_md_spl + tmn06_md_spl + tmp06_md_spl + tmx06_md_spl +
                        pre07_md_spl + tmn07_md_spl + tmp07_md_spl + tmx07_md_spl +
                        pre08_md_spl + tmn08_md_spl + tmp08_md_spl + tmx08_md_spl +
                        pre09_md_spl + tmn09_md_spl + tmp09_md_spl + tmx09_md_spl +
                        pre10_md_spl + tmn10_md_spl + tmp10_md_spl + tmx10_md_spl +
                        pre11_md_spl + tmn11_md_spl + tmp11_md_spl + tmx11_md_spl +
                        pre12_md_spl + tmn12_md_spl + tmp12_md_spl + tmx12_md_spl +
                        sex + age + sector + totcons + hhsize|
                        ea + zone + visit, data = NGA_panel)

# Residual Diagnostics
plot(density(resid(est2))) #A density plot
qqnorm(resid(est2)) # A quantile normal plot - good for checking normality
qqline(resid(est2))



stargazer(est1, est2, type = "html", out = "outputs/test.html")

(cbind(AIC(est1,est2),BIC(est1,est2)))
