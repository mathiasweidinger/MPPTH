dir <- "C:/Users/mathias/ROOT/EDU/UM/MA_MPP/MTH4911/MPP_Thesis"
setwd(dir)
load("outputs/panel_unif_perc.Rdata")
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

NGA_panel$ID <-paste(as.character(NGA_panel$hhid),as.character(NGA_panel$indiv))
NGA_panel$zone <- factor(NGA_panel$zone, levels = c(1:6), labels = c("north central", "north east", "north west", "south east", "south south", "south west"))
NGA_panel$sector <- factor(NGA_panel$sector, levels = c(1,2), labels = c("urban", "rural"))
NGA_panel$sex <- factor(NGA_panel$sex, levels = c(1,2), labels = c("male", "female"))
NGA_panel %<>%
  mutate(visit = case_when(t==1 | t==3 | t==5 ~ 1,
                           t==2 | t==4 | t==6 ~ 2,
  ))

NGA_panel %<>% mutate(
  childlabour = case_when(
    !is.na(childworkchores) & childworkchores > 80 ~ NA_real_,
    !is.na(childworkchores) & childworkchores ==0 ~ NA_real_,
    !is.na(childworkchores) & childworkchores <= 80 & childworkchores >0 ~ as.numeric(childworkchores),
    is.na(childworkchores) ~ NA_real_
  )
) 


attach(NGA_panel)

#------plot cld -----
dta_cld01 <- tibble(cbind(month = 1, NGA_panel[,c(1,  260, 257, 147)]))
dta_cld02 <- tibble(cbind(month = 2, NGA_panel[,c(1,  260, 257, 148)]))
dta_cld03 <- tibble(cbind(month = 3, NGA_panel[,c(1,  260, 257, 149)]))
dta_cld04 <- tibble(cbind(month = 4, NGA_panel[,c(1,  260, 257, 150)]))
dta_cld05 <- tibble(cbind(month = 5, NGA_panel[,c(1,  260, 257, 151)]))
dta_cld06 <- tibble(cbind(month = 6, NGA_panel[,c(1,  260, 257, 152)]))
dta_cld07 <- tibble(cbind(month = 7, NGA_panel[,c(1,  260, 257, 153)]))
dta_cld08 <- tibble(cbind(month = 8, NGA_panel[,c(1,  260, 257, 154)]))
dta_cld09 <- tibble(cbind(month = 9, NGA_panel[,c(1,  260, 257, 155)]))
dta_cld10 <- tibble(cbind(month = 10, NGA_panel[,c(1, 260, 257, 156)]))
dta_cld11 <- tibble(cbind(month = 11, NGA_panel[,c(1, 260, 257, 157)]))
dta_cld12 <- tibble(cbind(month = 12, NGA_panel[,c(1, 260, 257, 158)]))

names(dta_cld01)[5] <- "cld"
names(dta_cld02)[5] <- "cld"
names(dta_cld03)[5] <- "cld"
names(dta_cld04)[5] <- "cld"
names(dta_cld05)[5] <- "cld"
names(dta_cld06)[5] <- "cld"
names(dta_cld07)[5] <- "cld"
names(dta_cld08)[5] <- "cld"
names(dta_cld09)[5] <- "cld"
names(dta_cld10)[5] <- "cld"
names(dta_cld11)[5] <- "cld"
names(dta_cld12)[5] <- "cld"

dta_cld <- rbind(dta_cld01, dta_cld02, dta_cld03, dta_cld04,
                 dta_cld05, dta_cld06, dta_cld07, dta_cld08,
                 dta_cld09, dta_cld10, dta_cld11, dta_cld12)
dta_cld$month <- factor(dta_cld$month, levels = 1:12, labels = c(
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
))

p_cld <- ggplot(data=dta_cld, aes(x=cld, y= childlabour)) +
  xlim(-50,50) + # exclude outliers
  geom_smooth(color="#1c92d6", method = lm, se = TRUE) +
  geom_smooth(color="#3b8f1d", se = TRUE) +
  geom_smooth(method = lm, formula = y ~ splines::ns(
    x, knots = quantile(x,  probs = c(0.25, 0.5, 0.75)),
    Boundary.knots = range(x)),se = TRUE, color = "#cc9a0e") +
  geom_smooth(method=lm, formula = y ~ lspline::lspline(x, c(-5,-2,0,2,5))) +
  coord_cartesian(xlim = c(-50,50),
                  ylim = c(0,40)) +
  geom_rug(sides = "b", color ="red") +
  xlab("CLD mean deviation (%)") + 
  ylab("non-zero child labour (hrs/week)") +
  theme_minimal()

ggsave("outputs/pplots/cld_year.png", plot = p_cld)

# p_cld <- p_cld + facet_wrap(vars(month), ncol=3) +
#   ggtitle("Linear and B-Spline fits of cloud coverage")
# ggsave("outputs/pplots/cld_months.png", plot = p)



#------plot dtr -----
dta_dtr01 <- tibble(cbind(month = 1, NGA_panel[,c(1,  260, 257, 159)]))
dta_dtr02 <- tibble(cbind(month = 2, NGA_panel[,c(1,  260, 257, 160)]))
dta_dtr03 <- tibble(cbind(month = 3, NGA_panel[,c(1,  260, 257, 161)]))
dta_dtr04 <- tibble(cbind(month = 4, NGA_panel[,c(1,  260, 257, 162)]))
dta_dtr05 <- tibble(cbind(month = 5, NGA_panel[,c(1,  260, 257, 163)]))
dta_dtr06 <- tibble(cbind(month = 6, NGA_panel[,c(1,  260, 257, 164)]))
dta_dtr07 <- tibble(cbind(month = 7, NGA_panel[,c(1,  260, 257, 165)]))
dta_dtr08 <- tibble(cbind(month = 8, NGA_panel[,c(1,  260, 257, 166)]))
dta_dtr09 <- tibble(cbind(month = 9, NGA_panel[,c(1,  260, 257, 167)]))
dta_dtr10 <- tibble(cbind(month = 10, NGA_panel[,c(1, 260, 257, 168)]))
dta_dtr11 <- tibble(cbind(month = 11, NGA_panel[,c(1, 260, 257, 169)]))
dta_dtr12 <- tibble(cbind(month = 12, NGA_panel[,c(1, 260, 257, 170)]))

names(dta_dtr01)[5] <- "dtr"
names(dta_dtr02)[5] <- "dtr"
names(dta_dtr03)[5] <- "dtr"
names(dta_dtr04)[5] <- "dtr"
names(dta_dtr05)[5] <- "dtr"
names(dta_dtr06)[5] <- "dtr"
names(dta_dtr07)[5] <- "dtr"
names(dta_dtr08)[5] <- "dtr"
names(dta_dtr09)[5] <- "dtr"
names(dta_dtr10)[5] <- "dtr"
names(dta_dtr11)[5] <- "dtr"
names(dta_dtr12)[5] <- "dtr"

dta_dtr <- rbind(dta_dtr01, dta_dtr02, dta_dtr03, dta_dtr04,
                 dta_dtr05, dta_dtr06, dta_dtr07, dta_dtr08,
                 dta_dtr09, dta_dtr10, dta_dtr11, dta_dtr12)
dta_dtr$month <- factor(dta_dtr$month, levels = 1:12, labels = c(
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
))

p_dtr <- ggplot(data=dta_dtr, aes(x=dtr, y= childlabour)) +
  geom_smooth(color="#1c92d6", method = lm, se = TRUE) +
  geom_smooth(color="#3b8f1d", se = TRUE) +
  geom_smooth(method = lm, formula = y ~ splines::ns(
    x, knots = quantile(x,  probs = c(0.25, 0.5, 0.75)),
    Boundary.knots = range(x)),se = TRUE, color = "#cc9a0e") +
  geom_smooth(method=lm, formula = y ~ lspline::lspline(x, c(-5,-2,0,2,5))) +
  coord_cartesian(xlim = c(min(dta_dtr$dtr),max(dta_dtr$dtr)),
                  ylim = c(0,40)) +
  geom_rug(sides = "b", color ="red") +
  xlab("DTR mean deviation (%)") + 
  ylab("non-zero child labour (hrs/week)") +
  theme_minimal()

ggsave("outputs/pplots/dtr_year.png", plot = p_dtr)


#------plot pet -----
dta_pet01 <- tibble(cbind(month = 1, NGA_panel[,c(1,  260, 257, 171)]))
dta_pet02 <- tibble(cbind(month = 2, NGA_panel[,c(1,  260, 257, 172)]))
dta_pet03 <- tibble(cbind(month = 3, NGA_panel[,c(1,  260, 257, 173)]))
dta_pet04 <- tibble(cbind(month = 4, NGA_panel[,c(1,  260, 257, 174)]))
dta_pet05 <- tibble(cbind(month = 5, NGA_panel[,c(1,  260, 257, 175)]))
dta_pet06 <- tibble(cbind(month = 6, NGA_panel[,c(1,  260, 257, 176)]))
dta_pet07 <- tibble(cbind(month = 7, NGA_panel[,c(1,  260, 257, 177)]))
dta_pet08 <- tibble(cbind(month = 8, NGA_panel[,c(1,  260, 257, 178)]))
dta_pet09 <- tibble(cbind(month = 9, NGA_panel[,c(1,  260, 257, 179)]))
dta_pet10 <- tibble(cbind(month = 10, NGA_panel[,c(1, 260, 257, 180)]))
dta_pet11 <- tibble(cbind(month = 11, NGA_panel[,c(1, 260, 257, 181)]))
dta_pet12 <- tibble(cbind(month = 12, NGA_panel[,c(1, 260, 257, 182)]))

names(dta_pet01)[5] <- "pet"
names(dta_pet02)[5] <- "pet"
names(dta_pet03)[5] <- "pet"
names(dta_pet04)[5] <- "pet"
names(dta_pet05)[5] <- "pet"
names(dta_pet06)[5] <- "pet"
names(dta_pet07)[5] <- "pet"
names(dta_pet08)[5] <- "pet"
names(dta_pet09)[5] <- "pet"
names(dta_pet10)[5] <- "pet"
names(dta_pet11)[5] <- "pet"
names(dta_pet12)[5] <- "pet"

dta_pet <- rbind(dta_pet01, dta_pet02, dta_pet03, dta_pet04,
                 dta_pet05, dta_pet06, dta_pet07, dta_pet08,
                 dta_pet09, dta_pet10, dta_pet11, dta_pet12)
dta_pet$month <- factor(dta_pet$month, levels = 1:12, labels = c(
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
))

p_pet <- ggplot(data=dta_pet, aes(x=pet, y= childlabour)) +
  geom_smooth(color="#1c92d6", method = lm, se = TRUE) +
  geom_smooth(color="#3b8f1d", se = TRUE) +
  geom_smooth(method = lm, formula = y ~ splines::ns(
    x, knots = quantile(x,  probs = c(0.25, 0.5, 0.75)),
    Boundary.knots = range(x)),se = TRUE, color = "#cc9a0e") +
  geom_smooth(method=lm, formula = y ~ lspline::lspline(x, c(-5,-2,0,2,5))) +
  coord_cartesian(xlim = c(min(dta_pet$pet),max(dta_pet$pet)),
                  ylim = c(0,40)) +
  geom_rug(sides = "b", color ="red") +
  xlab("PET mean deviation (%)") + 
  ylab("non-zero child labour (hrs/week)") +
  theme_minimal()

ggsave("outputs/pplots/pet_year.png", plot = p_pet)


#------plot pre -----
dta_pre01 <- tibble(cbind(month = 1, NGA_panel[,c(1,  260, 257, 183)]))
dta_pre02 <- tibble(cbind(month = 2, NGA_panel[,c(1,  260, 257, 184)]))
dta_pre03 <- tibble(cbind(month = 3, NGA_panel[,c(1,  260, 257, 185)]))
dta_pre04 <- tibble(cbind(month = 4, NGA_panel[,c(1,  260, 257, 186)]))
dta_pre05 <- tibble(cbind(month = 5, NGA_panel[,c(1,  260, 257, 187)]))
dta_pre06 <- tibble(cbind(month = 6, NGA_panel[,c(1,  260, 257, 188)]))
dta_pre07 <- tibble(cbind(month = 7, NGA_panel[,c(1,  260, 257, 189)]))
dta_pre08 <- tibble(cbind(month = 8, NGA_panel[,c(1,  260, 257, 190)]))
dta_pre09 <- tibble(cbind(month = 9, NGA_panel[,c(1,  260, 257, 191)]))
dta_pre10 <- tibble(cbind(month = 10, NGA_panel[,c(1, 260, 257, 192)]))
dta_pre11 <- tibble(cbind(month = 11, NGA_panel[,c(1, 260, 257, 193)]))
dta_pre12 <- tibble(cbind(month = 12, NGA_panel[,c(1, 260, 257, 194)]))

names(dta_pre01)[5] <- "pre"
names(dta_pre02)[5] <- "pre"
names(dta_pre03)[5] <- "pre"
names(dta_pre04)[5] <- "pre"
names(dta_pre05)[5] <- "pre"
names(dta_pre06)[5] <- "pre"
names(dta_pre07)[5] <- "pre"
names(dta_pre08)[5] <- "pre"
names(dta_pre09)[5] <- "pre"
names(dta_pre10)[5] <- "pre"
names(dta_pre11)[5] <- "pre"
names(dta_pre12)[5] <- "pre"

dta_pre <- rbind(dta_pre01, dta_pre02, dta_pre03, dta_pre04,
                 dta_pre05, dta_pre06, dta_pre07, dta_pre08,
                 dta_pre09, dta_pre10, dta_pre11, dta_pre12)
dta_pre$month <- factor(dta_pre$month, levels = 1:12, labels = c(
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
))

p_pre <- ggplot(data=dta_pre, aes(x=pre, y= childlabour)) +
  xlim(-100,100) + # exclude outliers
  geom_smooth(color="#1c92d6", method = lm, se = TRUE) +
  geom_smooth(color="#3b8f1d", se = TRUE) +
  geom_smooth(method = lm, formula = y ~ splines::ns(
    x, knots = quantile(x,  probs = c(0.25, 0.5, 0.75)),
    Boundary.knots = range(x)),se = TRUE, color = "#cc9a0e") +
  geom_smooth(method=lm, formula = y ~ lspline::lspline(x, c(-5,-2,0,2,5))) +
  coord_cartesian(xlim = c(-100,100),
                  ylim = c(0,40)) +
  geom_rug(sides = "b", color ="red") +
  xlab("PRE mean deviation (%)") + 
  ylab("non-zero child labour (hrs/week)") +
  theme_minimal()

ggsave("outputs/pplots/pre_year.png", plot = p_pre)


#------plot tmn -----
dta_tmn01 <- tibble(cbind(month = 1, NGA_panel[,c(1,  260, 257, 195)]))
dta_tmn02 <- tibble(cbind(month = 2, NGA_panel[,c(1,  260, 257, 196)]))
dta_tmn03 <- tibble(cbind(month = 3, NGA_panel[,c(1,  260, 257, 197)]))
dta_tmn04 <- tibble(cbind(month = 4, NGA_panel[,c(1,  260, 257, 198)]))
dta_tmn05 <- tibble(cbind(month = 5, NGA_panel[,c(1,  260, 257, 199)]))
dta_tmn06 <- tibble(cbind(month = 6, NGA_panel[,c(1,  260, 257, 200)]))
dta_tmn07 <- tibble(cbind(month = 7, NGA_panel[,c(1,  260, 257, 201)]))
dta_tmn08 <- tibble(cbind(month = 8, NGA_panel[,c(1,  260, 257, 202)]))
dta_tmn09 <- tibble(cbind(month = 9, NGA_panel[,c(1,  260, 257, 203)]))
dta_tmn10 <- tibble(cbind(month = 10, NGA_panel[,c(1, 260, 257, 204)]))
dta_tmn11 <- tibble(cbind(month = 11, NGA_panel[,c(1, 260, 257, 205)]))
dta_tmn12 <- tibble(cbind(month = 12, NGA_panel[,c(1, 260, 257, 206)]))

names(dta_tmn01)[5] <- "tmn"
names(dta_tmn02)[5] <- "tmn"
names(dta_tmn03)[5] <- "tmn"
names(dta_tmn04)[5] <- "tmn"
names(dta_tmn05)[5] <- "tmn"
names(dta_tmn06)[5] <- "tmn"
names(dta_tmn07)[5] <- "tmn"
names(dta_tmn08)[5] <- "tmn"
names(dta_tmn09)[5] <- "tmn"
names(dta_tmn10)[5] <- "tmn"
names(dta_tmn11)[5] <- "tmn"
names(dta_tmn12)[5] <- "tmn"

dta_tmn <- rbind(dta_tmn01, dta_tmn02, dta_tmn03, dta_tmn04,
                 dta_tmn05, dta_tmn06, dta_tmn07, dta_tmn08,
                 dta_tmn09, dta_tmn10, dta_tmn11, dta_tmn12)
dta_tmn$month <- factor(dta_tmn$month, levels = 1:12, labels = c(
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
))

p_tmn <- ggplot(data=dta_tmn, aes(x=tmn, y= childlabour)) +
  geom_smooth(color="#1c92d6", method = lm, se = TRUE) +
  geom_smooth(color="#3b8f1d", se = TRUE) +
  geom_smooth(method = lm, formula = y ~ splines::ns(
    x, knots = quantile(x,  probs = c(0.25, 0.5, 0.75)),
    Boundary.knots = range(x)),se = TRUE, color = "#cc9a0e") +
  geom_smooth(method=lm, formula = y ~ lspline::lspline(x, c(-5,-2,0,2,5))) +
  coord_cartesian(xlim = c(min(dta_tmn$tmn),max(dta_tmn$tmn)),
                  ylim = c(0,40)) +
  geom_rug(sides = "b", color ="red") +
  xlab("TMN mean deviation (%)") + 
  ylab("non-zero child labour (hrs/week)") +
  theme_minimal()

ggsave("outputs/pplots/tmn.png", plot = p_tmn)


#------plot tmp -----
dta_tmp01 <- tibble(cbind(month = 1, NGA_panel[,c(1,  260, 257, 207)]))
dta_tmp02 <- tibble(cbind(month = 2, NGA_panel[,c(1,  260, 257, 208)]))
dta_tmp03 <- tibble(cbind(month = 3, NGA_panel[,c(1,  260, 257, 209)]))
dta_tmp04 <- tibble(cbind(month = 4, NGA_panel[,c(1,  260, 257, 210)]))
dta_tmp05 <- tibble(cbind(month = 5, NGA_panel[,c(1,  260, 257, 211)]))
dta_tmp06 <- tibble(cbind(month = 6, NGA_panel[,c(1,  260, 257, 212)]))
dta_tmp07 <- tibble(cbind(month = 7, NGA_panel[,c(1,  260, 257, 213)]))
dta_tmp08 <- tibble(cbind(month = 8, NGA_panel[,c(1,  260, 257, 214)]))
dta_tmp09 <- tibble(cbind(month = 9, NGA_panel[,c(1,  260, 257, 215)]))
dta_tmp10 <- tibble(cbind(month = 10, NGA_panel[,c(1, 260, 257, 216)]))
dta_tmp11 <- tibble(cbind(month = 11, NGA_panel[,c(1, 260, 257, 217)]))
dta_tmp12 <- tibble(cbind(month = 12, NGA_panel[,c(1, 260, 257, 218)]))

names(dta_tmp01)[5] <- "tmp"
names(dta_tmp02)[5] <- "tmp"
names(dta_tmp03)[5] <- "tmp"
names(dta_tmp04)[5] <- "tmp"
names(dta_tmp05)[5] <- "tmp"
names(dta_tmp06)[5] <- "tmp"
names(dta_tmp07)[5] <- "tmp"
names(dta_tmp08)[5] <- "tmp"
names(dta_tmp09)[5] <- "tmp"
names(dta_tmp10)[5] <- "tmp"
names(dta_tmp11)[5] <- "tmp"
names(dta_tmp12)[5] <- "tmp"

dta_tmp <- rbind(dta_tmp01, dta_tmp02, dta_tmp03, dta_tmp04,
                 dta_tmp05, dta_tmp06, dta_tmp07, dta_tmp08,
                 dta_tmp09, dta_tmp10, dta_tmp11, dta_tmp12)
dta_tmp$month <- factor(dta_tmp$month, levels = 1:12, labels = c(
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
))

p_tmp <- ggplot(data=dta_tmp, aes(x=tmp, y= childlabour)) +
  geom_smooth(color="#1c92d6", method = lm, se = TRUE) +
  geom_smooth(color="#3b8f1d", se = TRUE) +
  geom_smooth(method = lm, formula = y ~ splines::ns(
    x, knots = quantile(x,  probs = c(0.25, 0.5, 0.75)),
    Boundary.knots = range(x)),se = TRUE, color = "#cc9a0e") +
  geom_smooth(method=lm, formula = y ~ lspline::lspline(x, c(-5,-2,0,2,5))) +
  coord_cartesian(xlim = c(min(dta_tmp$tmp),max(dta_tmp$tmp)),
                  ylim = c(0,40)) +
  geom_rug(sides = "b", color ="red") +
  xlab("TMP mean deviation (%)") + 
  ylab("non-zero child labour (hrs/week)") +
  theme_minimal()

ggsave("outputs/pplots/tmp.png", plot = p_tmp)


#------plot tmx -----
dta_tmx01 <- tibble(cbind(month = 1, NGA_panel[,c(1,  260, 257, 219)]))
dta_tmx02 <- tibble(cbind(month = 2, NGA_panel[,c(1,  260, 257, 220)]))
dta_tmx03 <- tibble(cbind(month = 3, NGA_panel[,c(1,  260, 257, 221)]))
dta_tmx04 <- tibble(cbind(month = 4, NGA_panel[,c(1,  260, 257, 222)]))
dta_tmx05 <- tibble(cbind(month = 5, NGA_panel[,c(1,  260, 257, 223)]))
dta_tmx06 <- tibble(cbind(month = 6, NGA_panel[,c(1,  260, 257, 224)]))
dta_tmx07 <- tibble(cbind(month = 7, NGA_panel[,c(1,  260, 257, 225)]))
dta_tmx08 <- tibble(cbind(month = 8, NGA_panel[,c(1,  260, 257, 226)]))
dta_tmx09 <- tibble(cbind(month = 9, NGA_panel[,c(1,  260, 257, 227)]))
dta_tmx10 <- tibble(cbind(month = 10, NGA_panel[,c(1, 260, 257, 228)]))
dta_tmx11 <- tibble(cbind(month = 11, NGA_panel[,c(1, 260, 257, 229)]))
dta_tmx12 <- tibble(cbind(month = 12, NGA_panel[,c(1, 260, 257, 230)]))

names(dta_tmx01)[5] <- "tmx"
names(dta_tmx02)[5] <- "tmx"
names(dta_tmx03)[5] <- "tmx"
names(dta_tmx04)[5] <- "tmx"
names(dta_tmx05)[5] <- "tmx"
names(dta_tmx06)[5] <- "tmx"
names(dta_tmx07)[5] <- "tmx"
names(dta_tmx08)[5] <- "tmx"
names(dta_tmx09)[5] <- "tmx"
names(dta_tmx10)[5] <- "tmx"
names(dta_tmx11)[5] <- "tmx"
names(dta_tmx12)[5] <- "tmx"

dta_tmx <- rbind(dta_tmx01, dta_tmx02, dta_tmx03, dta_tmx04,
                 dta_tmx05, dta_tmx06, dta_tmx07, dta_tmx08,
                 dta_tmx09, dta_tmx10, dta_tmx11, dta_tmx12)
dta_tmx$month <- factor(dta_tmx$month, levels = 1:12, labels = c(
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
))

p_tmx <- ggplot(data=dta_tmx, aes(x=tmx, y= childlabour)) +
  geom_smooth(color="#1c92d6", method = lm, se = TRUE) +
  geom_smooth(color="#3b8f1d", se = TRUE) +
  geom_smooth(method = lm, formula = y ~ splines::ns(
    x, knots = quantile(x,  probs = c(0.25, 0.5, 0.75)),
    Boundary.knots = range(x)),se = TRUE, color = "#cc9a0e") +
  geom_smooth(method=lm, formula = y ~ lspline::lspline(x, c(-5,-2,0,2,5))) +
  coord_cartesian(xlim = c(min(dta_tmx$tmx),max(dta_tmx$tmx)),
                  ylim = c(0,40)) +
  geom_rug(sides = "b", color ="red") +
  xlab("TMX mean deviation (%)") + 
  ylab("non-zero child labour (hrs/week)") +
  theme_minimal()

ggsave("outputs/pplots/tmx.png", plot = p_tmx)


#------plot vap -----
dta_vap01 <- tibble(cbind(month = 1, NGA_panel[,c(1,  260, 257, 231)]))
dta_vap02 <- tibble(cbind(month = 2, NGA_panel[,c(1,  260, 257, 232)]))
dta_vap03 <- tibble(cbind(month = 3, NGA_panel[,c(1,  260, 257, 233)]))
dta_vap04 <- tibble(cbind(month = 4, NGA_panel[,c(1,  260, 257, 234)]))
dta_vap05 <- tibble(cbind(month = 5, NGA_panel[,c(1,  260, 257, 235)]))
dta_vap06 <- tibble(cbind(month = 6, NGA_panel[,c(1,  260, 257, 236)]))
dta_vap07 <- tibble(cbind(month = 7, NGA_panel[,c(1,  260, 257, 237)]))
dta_vap08 <- tibble(cbind(month = 8, NGA_panel[,c(1,  260, 257, 238)]))
dta_vap09 <- tibble(cbind(month = 9, NGA_panel[,c(1,  260, 257, 239)]))
dta_vap10 <- tibble(cbind(month = 10, NGA_panel[,c(1, 260, 257, 240)]))
dta_vap11 <- tibble(cbind(month = 11, NGA_panel[,c(1, 260, 257, 241)]))
dta_vap12 <- tibble(cbind(month = 12, NGA_panel[,c(1, 260, 257, 242)]))

names(dta_vap01)[5] <- "vap"
names(dta_vap02)[5] <- "vap"
names(dta_vap03)[5] <- "vap"
names(dta_vap04)[5] <- "vap"
names(dta_vap05)[5] <- "vap"
names(dta_vap06)[5] <- "vap"
names(dta_vap07)[5] <- "vap"
names(dta_vap08)[5] <- "vap"
names(dta_vap09)[5] <- "vap"
names(dta_vap10)[5] <- "vap"
names(dta_vap11)[5] <- "vap"
names(dta_vap12)[5] <- "vap"

dta_vap <- rbind(dta_vap01, dta_vap02, dta_vap03, dta_vap04,
                 dta_vap05, dta_vap06, dta_vap07, dta_vap08,
                 dta_vap09, dta_vap10, dta_vap11, dta_vap12)
dta_vap$month <- factor(dta_vap$month, levels = 1:12, labels = c(
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
))

p_vap <- ggplot(data=dta_vap, aes(x=vap, y= childlabour)) +
  xlim(-25,25) + # exclude outliers
  geom_smooth(color="#1c92d6", method = lm, se = TRUE) +
  geom_smooth(color="#3b8f1d", se = TRUE) +
  geom_smooth(method = lm, formula = y ~ splines::ns(
    x, knots = quantile(x,  probs = c(0.25, 0.5, 0.75)),
    Boundary.knots = range(x)),se = TRUE, color = "#cc9a0e") +
  geom_smooth(method=lm, formula = y ~ lspline::lspline(x, c(-5,-2,0,2,5))) +
  coord_cartesian(xlim = c(-25,25),
                  ylim = c(0,40)) +
  geom_rug(sides = "b", color ="red") +
  xlab("VAP mean deviation (%)") + 
  ylab("non-zero child labour (hrs/week)") +
  theme_minimal()

ggsave("outputs/pplots/vap.png", plot = p_vap)

#------plot wet -----
dta_wet01 <- tibble(cbind(month = 1, NGA_panel[,c(1,  260, 257, 243)]))
dta_wet02 <- tibble(cbind(month = 2, NGA_panel[,c(1,  260, 257, 244)]))
dta_wet03 <- tibble(cbind(month = 3, NGA_panel[,c(1,  260, 257, 245)]))
dta_wet04 <- tibble(cbind(month = 4, NGA_panel[,c(1,  260, 257, 246)]))
dta_wet05 <- tibble(cbind(month = 5, NGA_panel[,c(1,  260, 257, 247)]))
dta_wet06 <- tibble(cbind(month = 6, NGA_panel[,c(1,  260, 257, 248)]))
dta_wet07 <- tibble(cbind(month = 7, NGA_panel[,c(1,  260, 257, 249)]))
dta_wet08 <- tibble(cbind(month = 8, NGA_panel[,c(1,  260, 257, 250)]))
dta_wet09 <- tibble(cbind(month = 9, NGA_panel[,c(1,  260, 257, 251)]))
dta_wet10 <- tibble(cbind(month = 10, NGA_panel[,c(1, 260, 257, 252)]))
dta_wet11 <- tibble(cbind(month = 11, NGA_panel[,c(1, 260, 257, 253)]))
dta_wet12 <- tibble(cbind(month = 12, NGA_panel[,c(1, 260, 257, 154)]))

names(dta_wet01)[5] <- "wet"
names(dta_wet02)[5] <- "wet"
names(dta_wet03)[5] <- "wet"
names(dta_wet04)[5] <- "wet"
names(dta_wet05)[5] <- "wet"
names(dta_wet06)[5] <- "wet"
names(dta_wet07)[5] <- "wet"
names(dta_wet08)[5] <- "wet"
names(dta_wet09)[5] <- "wet"
names(dta_wet10)[5] <- "wet"
names(dta_wet11)[5] <- "wet"
names(dta_wet12)[5] <- "wet"

dta_wet <- rbind(dta_wet01, dta_wet02, dta_wet03, dta_wet04,
                 dta_wet05, dta_wet06, dta_wet07, dta_wet08,
                 dta_wet09, dta_wet10, dta_wet11, dta_wet12)
dta_wet$month <- factor(dta_wet$month, levels = 1:12, labels = c(
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
))

p_wet <- ggplot(data=dta_wet, aes(x=wet, y= childlabour)) +
  xlim(-100,100) + # exclude outliers
  geom_smooth(color="#1c92d6", method = lm, se = TRUE) +
  geom_smooth(color="#3b8f1d", se = TRUE) +
  geom_smooth(method = lm, formula = y ~ splines::ns(
    x, knots = quantile(x,  probs = c(0.25, 0.5, 0.75)),
    Boundary.knots = range(x)),se = TRUE, color = "#cc9a0e") +
  geom_smooth(method=lm, formula = y ~ lspline::lspline(x, c(-5,-2,0,2,5))) +
  coord_cartesian(xlim = c(min(dta_wet$wet),max(dta_wet$wet)),
                  ylim = c(0,40)) +
  geom_rug(sides = "b", color ="red") +
  xlab("WET mean deviation (%)") + 
  ylab("non-zero child labour (hrs/week)") +
  theme_minimal()

ggsave("outputs/pplots/wet.png", plot = p_wet)
