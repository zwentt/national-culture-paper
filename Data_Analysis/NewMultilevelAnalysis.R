library(lmerTest)
library(haven)
library(ggplot2)
library(stargazer)
library(blme)

library(rstan)
library(rstantools)
library(brms)
library(haven)
library(StanHeaders)
options(scipen=10)

CultureData <- read_dta("/Users/zwen/Desktop/OneDrive - University of Toledo/Desktop/@ National Culture/national-culture-paper/Data_Analysis/CultureData.dta")

CultureData <- read_dta("D:/OneDrive - University of Toledo/Desktop/@ National Culture/national-culture-paper/Data_Analysis/CultureData.dta")


regionalCultureData <- subset(CultureData, network2x == 0)
globalCultureData <- subset(CultureData, network2x == 1)



outcome.null <- outcomecj ~  (1 | countryx)
stan.outcome.null <- brm(outcome.null, data = CultureData, core = 6, chains = 4)
stan.outcome.null

outcome.ri <- outcomecj ~ gmci2013c + y2013std + network2xcj  + firmsizestd + agilitycj + (guaiv_c) + (1 | countryx)
stan.outcome.ri <- brm(outcome.ri, data = CultureData, core = 6, chains = 4)
stan.outcome.ri

#gfuov_c + gpdiv_c + ginscolv_c + ghumv_c + gperv_c + gigrcolv_c + ggndv_c + gassv_c
outcome.rirs <- outcome2cj ~ gmci2013c + y2013std + network2xcj  + firmsizestd + agility2cj + (gfuop_c) + (1 + agilitycj | countryx)
stan.outcome.rirs <- brm(outcome.rirs, data = CultureData, core = 6, chains = 4)
stan.outcome.rirs

outcome.cross <- outcomecj ~ 1 + firmsizestd + network2xcj*agilitycj * (gfuov_c + gpdiv_c + ginscolv_c + ghumv_c + gperv_c + gigrcolv_c + ggndv_c + gassv_c) + (1 + agilitycj * network2xcj | countryx)
stan.outcome.cross <- brm(outcome.cross, data = CultureData, core = 6, chains = 4, 
                          #prior = prior(normal(0, 1), coef = "guaiv_c")
            )
stan.outcome.cross
summary(lm(outcomecj ~ 1 + firmsizestd + network2xcj*agilitycj * (guaiv_c), data = CultureData))


# Agility Models
agility.cross <- agilitycj ~ 1 + firmsizestd + (gfuop_c) + (1  | countryx)
stan.agility.cross <- brm(agility.cross, data = CultureData, core = 6, chains = 4)
stan.agility.cross



currentModel.fit <- lmer(outcome.cross, data = CultureData, REML = FALSE, 
                         control=lmerControl(optimizer="Nelder_Mead", optCtrl=list(maxfun = 1e+05)), verbose = TRUE)
