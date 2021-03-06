#install.packages(c("StanHeaders", "rstan"), type = "source")
library(rstan)
library(rstantools)
library(brms)
library(haven)
library(StanHeaders)

CultureData <- read_dta("https://github.com/zwentt/national-culture-paper/raw/main/Data_Analysis/CultureData.dta")


guaiv.cross.brm <- brm(outcome ~ agility2cj + guaiv_c + firmsizestd + network2xcj + gmci2016c + I(firmsizestd*firmsizestd) + guaiv_c*agility2cj +  (guaiv_c | countryx), data = CultureData)

gfuov.cross.brm <- brm(outcome ~ agility2cj + gfuov_c + firmsizestd + network2xcj + gmci2016c + I(firmsizestd*firmsizestd) + gfuov_c*agility2cj +  (gfuov_c | countryx), data = CultureData)

gpdiv.cross.brm <- brm(outcome ~ agility2cj + gpdiv_c + firmsizestd + network2xcj + gmci2016c + I(firmsizestd*firmsizestd) + gpdiv_c*agility2cj +  (gpdiv_c | countryx), data = CultureData)

ginscolv.cross.brm <- brm(outcome ~ agility2cj + ginscolv_c + firmsizestd + network2xcj + gmci2016c + ginscolv_c*agility2cj +  (ginscolv_c | countryx), data = CultureData)

ghumv.cross.brm <- brm(outcome ~ agility2cj + ghumv_c + firmsizestd + network2xcj + gmci2016c + I(firmsizestd*firmsizestd) + ghumv_c*agility2cj +  (ghumv_c | countryx), data = CultureData)

gperv.cross.brm <- brm(outcome ~ agility2cj + gperv_c + firmsizestd + network2xcj + gmci2016c + I(firmsizestd*firmsizestd) + gperv_c*agility2cj +  (gperv_c | countryx), data = CultureData)

gigrcolv.cross.brm <- brm(outcome ~ agility2cj + gigrcolv_c + firmsizestd + network2xcj + gmci2016c + I(firmsizestd*firmsizestd) + gigrcolv_c*agility2cj +  (gigrcolv_c | countryx), data = CultureData)

ggndv.cross.brm <- brm(outcome ~ agility2cj + ggndv_c + firmsizestd + network2xcj + gmci2016c + I(firmsizestd*firmsizestd) + ggndv_c*agility2cj +  (ggndv_c | countryx), data = CultureData)

gassv.cross.brm <- brm(outcome ~ agility2cj + gassv_c + firmsizestd + network2xcj + gmci2016c + I(firmsizestd*firmsizestd) + gassv_c*agility2cj +  (gassv_c | countryx), data = subset(CultureData, network2x == 0))





agility.ginscolv.cross.brm <- brm(sensing2 ~ ginscolv_c + firmsizestd + network2xcj + gmci2016c + ginscolv_c*network2xcj +  (ginscolv_c | countryx), data = CultureData)

agility.gpdiv.cross.brm <- brm(outcome ~ gpdiv_c + agilitycj + firmsizestd + network2xcj + gmci2016c + agility2cj * gpdiv_c + (gpdiv_c | countryx), data = subset(CultureData, network2x == 0))


fullModel <- brms::brm(outcome ~ agilitycj * (guaiv_c + gfuov_c + gpdiv_c + ginscolv_c + ghumv_c + gperv_c + gigrcolv_c + ggndv_c + gassv_c) + 
                         firmsizestd + gmci2013 +
                         (guaiv_c + gfuov_c + gpdiv_c + ginscolv_c + ghumv_c + gperv_c + gigrcolv_c + ggndv_c + gassv_c | countryx ), 
                       data = subset(cultureData, network2x == 0))



