# load packages
library(tidyverse)
library(magrittr)
library(lfe)
library(data.table)
library(climate)
# set working directory
dir <- "C:/Users/mathias/ROOT/EDU/UM/MA_MPP/MTH4911/MPP_Thesis"
setwd(dir)

# load cleaned household data

load("outputs/NGA_panel.Rda")

# create dependent variable: child labour

table(work_7d)
summary(pri_hrs_7d) # only a third of observations are non-NA. 
summary(sec_hrs)

NGA_panel %<>% tibble() %>% 
  mutate(
    daily_work_hrs = case_when(
      !is.na(pri_hrs_7d) & !is.na(sec_hrs) ~ as.numeric(sum(pri_hrs_7d,sec_hrs)/7),
      is.na(pri_hrs_7d) & !is.na(sec_hrs) ~ as.numeric(sec_hrs/7),
      !is.na(pri_hrs_7d) & is.na(sec_hrs) ~ as.numeric(pri_hrs_7d/7),
      is.na(pri_hrs_7d) & is.na(sec_hrs) ~ NA_real_
      )
    )
NGA_panel %<>% mutate(
  daily_work_hrs = case_when(
    daily_work_hrs < 19 ~ daily_work_hrs,
    daily_work_hrs >= 18 ~ NA_real_,
    )
  )
attach(NGA_panel)
summary(daily_work_hrs)
summary(pri_hrs_7d/7) # They are virtually identical distributions.

NGA_panel %<>% mutate(
  daily_chores =
    case_when(
      !is.na(water) & !is.na(wood)~as.numeric(water+wood)/60,
      !is.na(water) & is.na(wood)~as.numeric(wood)/60,
      is.na(water) & !is.na(wood)~as.numeric(water)/60,
      is.na(water) & is.na(wood)~NA_real_
      )
  )
NGA_panel %<>% mutate(
  daily_chores =
    case_when(daily_chores > 10 ~ NA_real_,
              daily_chores <=10 ~ daily_chores,
    )
)

summary(NGA_panel$daily_chores)

NGA_panel %<>% mutate(
  child_work =
    case_when(
      age < 18 & !is.na(daily_work_hrs) ~ daily_work_hrs,
      age >=18 ~ NA_real_,
      is.na(daily_work_hrs) ~ NA_real_
    )
)
NGA_panel %<>% mutate(
  child_work_chores =
    case_when(
      age < 18 & !is.na(daily_work_hrs) & !is.na(daily_chores) ~ as.numeric(daily_work_hrs+daily_chores),
      age < 18 & !is.na(daily_work_hrs) & is.na(daily_chores) ~ as.numeric(daily_work_hrs),
      age < 18 & is.na(daily_work_hrs) & !is.na(daily_chores) ~ as.numeric(daily_chores),
      age < 18 & is.na(daily_work_hrs) & is.na(daily_chores) ~ NA_real_,
      age >=18 ~ NA_real_,
    )
)

summary(child_work)
summary(child_work_chores)

NGA_panel %<>% mutate(
  pri_sect_bin = case_when(pri_sect == 1 ~ 1,
                           pri_sect != 1 ~ 0
    
  )
)
NGA_panel$pri_sect_bin <- factor(NGA_panel$pri_sect_bin, labels = c("agriculture", "other"))

NGA_panel$work_7d <- factor(NGA_panel$work_7d, labels = c("yes", "no")) 
NGA_panel$sex <- factor(NGA_panel$sex, labels =c("male", "female"))
NGA_panel$rururb <- factor(NGA_panel$rururb, labels = c("rural", "urban"))

NGA_panel %<>%
  mutate(below_avg_cons = case_when(
    totcons <= quantile(totcons, .1, na.rm = T) ~ 1,
    totcons > quantile(totcons, .1, na.rm = T) ~ 0
  ))

NGA_panel %<>%
  mutate(child_worker = case_when(
    !is.na(child_work_chores) & child_work_chores >0 ~ 1,
    !is.na(child_work_chores) & child_work_chores == 0 ~ 0,
    is.na(child_work_chores) ~ NA_real_
    ))




attach(NGA_panel)
child_worker <- factor(child_worker, labels =c("No", "Yes"))

ggplot(data=NGA_panel[child_worker==1 & !is.na(rururb), ], aes(x=age, y=child_work_chores, colour=rururb, group = rururb))+
  geom_smooth(method = "lm") +
  geom_smooth(method = "gam", alpha = 0.3) +
  xlim(5,18) +
  theme_minimal() +
  xlab("age in years") +
  ylab("average daily work hours")


