library(lmerTest)
library(haven)
library(ggplot2)
library(stargazer)
library(blme)
library(xtable)
library(rstan)
library(rstantools)
library(brms)
library(haven)
library(StanHeaders)
library(BayesPostEst)
options(scipen=10)

CultureData <- read_dta("/Users/zwen/Documents/GitHub/national-culture-paper/Data_Analysis/cultureDataFinal.dta")



CultureData <- read_dta("D:/Documents/GitHub/national-culture-paper/Data_Analysis/cultureDataFinal.dta")




regionData <- subset(CultureData, network2x == 0)
globalData <- subset(CultureData, network2x == 1)


#gfuov_c + gpdiv_c + ginscolv_c + ghumv_c + gperv_c + gigrcolv_c + ggndv_c + gassv_c
agility.null <- agility ~ 1 + (1 | countryx)
outcome.null <- outcome ~ 1 + (1 | countryx)

fit.agility.null <- brm(agility.null, data = CultureData, refresh = 0, core = 7)
fit.outcome.null <- brm(outcome.null, data = CultureData, refresh = 0, core = 7)

fit.agility.null
fit.outcome.null





fit.agility.null <- blmer(agility.null, data = CultureData)
fit.outcome.null <- blmer(outcome.null, data = CultureData)
summary(fit.agility.null)
summary(fit.outcome.null)


fit.agility.null <- brm(agility.null, data = CultureData, core = 6, chains = 4, refresh = 0)
fit.agility.null





agility.cross <- agilitycj ~ 1 + mfr2013lnstd + network2xcj : (ginscolv_c) + firmsizestd  + (1 + network2xcj | countryx)
fit.agility.cross <- brm(agility.cross, data = CultureData, core = 6, chains = 4, iter = 3000, control = list(adapt_delta = 0.95))
fit.agility.cross


# Effectiveness Models 







stan.outcome.null <- brm(outcome.null, data = CultureData, core = 6, chains = 4)
stan.outcome.null

outcome.ri <- outcomecj ~   mfr2013lnstd + network2xcj  + firmsizestd + agilitycj + (guaiv_c) + (1 | countryx)
stan.outcome.ri <- brm(outcome.ri, data = CultureData, core = 6, chains = 4)
stan.outcome.ri

#gfuov_c + gpdiv_c + ginscolv_c + ghumv_c + gperv_c + gigrcolv_c + ggndv_c + gassv_c
outcome.rirs <- outcome ~  1 + mfr2013lnstd + network2xcj  + firmsizestd + agilitycj + (gfuov_c) + (1 + agilitycj | countryx)
stan.outcome.rirs <- brm(outcome.rirs, data = CultureData, core = 6, chains = 4)
stan.outcome.rirs

start.time <- Sys.time()
outcome.cross <- outcome2 ~ 1 + mfr2013lnstd + firmsizestd + I(firmsizestd*firmsizestd) + network2xcj * agility2cj * (ginscolv_c) + (1 + agilitycj * network2xcj | countryx)
stan.outcome.cross <- brm(outcome.cross, data = CultureData, core = 6, chains = 4, 
                          family = gaussian(), refresh = 0
                          #prior = prior(normal(0, 1), coef = "guaiv_c")
            )

end.time <- Sys.time()
time.taken <- end.time - start.time
#Windows 10 Intel 10th Generation Time difference of (1) 1.252521 mins
# (2) Time difference of 59.66793 secs
# Exploded the R
# (3) Time difference of 1.002167 mins
# (4) Time difference of 1.146176 mins
# (5) Time difference of 56.23147 secs



time.taken

stan.outcome.cross

#Leave one out cross validation
loo(stan.outcome.cross)
loo_compare(loo(stan.outcome.rirs), loo(stan.outcome.cross))



summary(lm(outcomecj ~ 1 + firmsizestd + network2xcj*agilitycj * (guaiv_c), data = CultureData))


# Agility Models
agility.cross <- agilitycj ~ 1 + firmsizestd + network2xcj*(gfuov_c) + (1  | countryx)
stan.agility.cross <- brm(agility.cross, data = CultureData, core = 6, chains = 4)
stan.agility.cross



currentModel.fit <- blmer(outcome.rirs, data = CultureData, REML = FALSE, 
                         control=lmerControl(optimizer="Nelder_Mead", optCtrl=list(maxfun = 1e+05)), verbose = TRUE)
summary(currentModel.fit)

