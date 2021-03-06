library(haven)
library(dplyr)
library(data.table)
library(sjstats)
library(lmtest)
library(readr)
library(ggplot2)

library(rstan)
library(rstantools)
library(brms)
library(haven)

CultureData <- read_dta("https://github.com/zwentt/national-culture-paper/raw/main/Data_Analysis/CultureData.dta")


RI.brm <- brm(agility2 ~ firmsizestd + network2xcj + gmci2016c + I(firmsizestd*firmsizestd) + (1 | countryx), data = CultureData)



reg.RI.brm <- brm(agility2 ~ firmsizestd + gmci2016c + I(firmsizestd*firmsizestd) + (1 | countryx), data = subset(CultureData, network2x == 0))



multi.RI.brm <- brm(agility2 ~ firmsizestd + gmci2016c + I(firmsizestd*firmsizestd) + (1 | countryx), data = subset(CultureData, network2x == 1))
