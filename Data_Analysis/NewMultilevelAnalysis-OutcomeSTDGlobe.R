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
library(dplyr)

#Preamble settings
options(scipen=10)
#options(buildtools.check = function(action) TRUE )
theme_set(theme_bw())

#1/6. Outcome2 Models 
rm(brm.outcome.fit.names)
brm.outcome.fit.names <- "outcome2.control.brm.fit"

for (v in glo_v) {
  brm.outcome.fit.names <- c(brm.outcome.fit.names, paste("outcome2.", v, ".brm.fit", sep = ""))
}

#Fitting Control Variable Only Model
outcome2.control.brm.fit <- brm(outcome2 ~ mfr2013.z + firmsize.adj.cj + strategycj + (1 | countryx), data = CultureData, family = "student")

brm.outcome2.model <- outcome2 ~ mfr2013.z + firmsize.adj.cj + strategycj + network4xcj + agility2cj + agility2cj * network4xcj * cul  + (1 + network4xcj + agility2cj | countryx)


#model estimations 
for (i in 1:9) {
  CultureData$cul <- as.numeric(unlist(CultureData[, c(glo_vz[i])]))
  assign(brm.outcome.fit.names[i+1], brm(brm.outcome2.model, data = CultureData, core = 8, chains = 4, iter = 3000, family = "student", refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(20)
}


#Model Output using mcmcReg - 5% significance
mcmcReg(list(outcome2.guaiv.brm.fit,
             outcome2.gfuov.brm.fit,
             outcome2.gpdiv.brm.fit,
             outcome2.ginscolv.brm.fit,
             outcome2.ghumv.brm.fit,
             outcome2.gperv.brm.fit,
             outcome2.gigrcolv.brm.fit,
             outcome2.ggndv.brm.fit,
             outcome2.gassv.brm.fit),
        pars = c("sd", "cor", "sigma", "b"),
        ci = 0.95, format = "latex", caption = "Outcome2 Models (T) - 4 Level Network",
        file = paste(filepath, "LatexTables/", "outcome2ModelFinalT4", sep=""), 
        custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)

#Model Output using mcmcReg - 10% significance
mcmcReg(list(outcome2.guaiv.brm.fit,
             outcome2.gfuov.brm.fit,
             outcome2.gpdiv.brm.fit,
             outcome2.ginscolv.brm.fit,
             outcome2.ghumv.brm.fit,
             outcome2.gperv.brm.fit,
             outcome2.gigrcolv.brm.fit,
             outcome2.ggndv.brm.fit,
             outcome2.gassv.brm.fit),
        pars = c("b", "sd", "cor", "sigma"),
        ci = 0.90, format = "latex", caption = "Outcome2 Models (T) alpha = .10 - 4 Level Network",
        file = paste(filepath, "LatexTables/", "outcome2ModelFinalT410", sep=""), 
        custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)



#2/6. Outcome2 Models Regional
rm(brm.outcome.fit.names)
brm.outcome.fit.names <- "outcome2.control.brm.fit.regional"

for (v in glo_v) {
  brm.outcome.fit.names <- c(brm.outcome.fit.names, paste("outcome2.", v, ".brm.fit.regional", sep = ""))
}

#Fitting Control Variable Only Model
outcome2.control.brm.fit.regional <- brm(outcome2 ~ mfr2013.z + firmsize.adj.cj + strategycj + (1 | countryx), data = regionalData, family = "student")


brm.outcome2.model.regional <- outcome2 ~ mfr2013.z + firmsize.adj.cj + strategycj + agility2cj + agility2cj * cul  + (1 + agility2cj | countryx)


brm.outcome2.model <- outcome2 ~ mfr2013.z + firmsize.adj.cj + agility2cj + strategycj + 
                      cul * agility2cj + (1 + agility2cj | countryx)


#model estimations 
for (i in 1:9) {
  regionalData$cul <- as.numeric(unlist(regionalData[, c(glo_vz[i])]))
  assign(brm.outcome.fit.names[i+1], brm(brm.outcome2.model.regional, data = regionalData, core = 8, chains = 4, iter = 3000, family = "student", refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(20)
}


#Model Output using mcmcReg
mcmcReg(list(outcome2.guaiv.brm.fit.regional,
             outcome2.gfuov.brm.fit.regional,
             outcome2.gpdiv.brm.fit.regional,
             outcome2.ginscolv.brm.fit.regional,
             outcome2.ghumv.brm.fit.regional,
             outcome2.gperv.brm.fit.regional,
             outcome2.gigrcolv.brm.fit.regional,
             outcome2.ggndv.brm.fit.regional,
             outcome2.gassv.brm.fit.regional),
        pars = c("b", "sd", "cor", "sigma"),
        ci = 0.95, format = "latex", caption = "Outcome2 Models Regional Model (T) - Cross Interaction",
        file = paste(filepath, "LatexTables/", "outcome2ModelFinalRegionalZ", sep=""), 
        custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)

mcmcReg(list(outcome2.guaiv.brm.fit.regional,
             outcome2.gfuov.brm.fit.regional,
             outcome2.gpdiv.brm.fit.regional,
             outcome2.ginscolv.brm.fit.regional,
             outcome2.ghumv.brm.fit.regional,
             outcome2.gperv.brm.fit.regional,
             outcome2.gigrcolv.brm.fit.regional,
             outcome2.ggndv.brm.fit.regional,
             outcome2.gassv.brm.fit.regional),
        pars = c("b", "sd", "cor", "sigma"),
        ci = 0.90, format = "latex", caption = "Outcome2 Models Regional Model (T) alpha = .10 - Cross Interaction",
        file = paste(filepath, "LatexTables/", "outcome2ModelFinalRegionalZ10", sep=""), 
        custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)



#3/6. Outcome2 Models Global
rm(brm.outcome.fit.names)
brm.outcome.fit.names <- "outcome2.control.brm.fit.global"

for (v in glo_v) {
  brm.outcome.fit.names <- c(brm.outcome.fit.names, paste("outcome2.", v, ".brm.fit.global", sep = ""))
}

#Fitting Control Variable Only Model
outcome2.control.brm.fit <- brm(outcome2 ~ mfr2013.z + firmsize.adj.cj + strategycj + (1 | countryx), data = globalData, family = "student")



brm.outcome2.model.global <- outcome2 ~ mfr2013.z + firmsize.adj.cj + strategycj + agility2cj + 
  agility2cj * cul  + (1 + agility2cj | countryx)

#model estimations 
for (i in 1:9) {
  globalData$cul <- as.numeric(unlist(globalData[, c(glo_vz[i])]))
  assign(brm.outcome.fit.names[i+1], brm(brm.outcome2.model.global, data = globalData, core = 8, chains = 4, iter = 3000, refresh = 0, family = "student", control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(20)
}


#Model Output using mcmcReg
mcmcReg(list(outcome2.guaiv.brm.fit.global,
             outcome2.gfuov.brm.fit.global,
             outcome2.gpdiv.brm.fit.global,
             outcome2.ginscolv.brm.fit.global,
             outcome2.ghumv.brm.fit.global,
             outcome2.gperv.brm.fit.global,
             outcome2.gigrcolv.brm.fit.global,
             outcome2.ggndv.brm.fit.global,
             outcome2.gassv.brm.fit.global),
        pars = c("b", "sd", "cor", "sigma"),
        ci = 0.95, format = "latex", caption = "Outcome2 Models Global Model (T) - Cross Interaction",
        file = paste(filepath, "LatexTables/", "outcome2ModelFinalGlobalZ", sep=""), 
        custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)


#Model Output using mcmcReg
mcmcReg(list(outcome2.guaiv.brm.fit.global,
             outcome2.gfuov.brm.fit.global,
             outcome2.gpdiv.brm.fit.global,
             outcome2.ginscolv.brm.fit.global,
             outcome2.ghumv.brm.fit.global,
             outcome2.gperv.brm.fit.global,
             outcome2.gigrcolv.brm.fit.global,
             outcome2.ggndv.brm.fit.global,
             outcome2.gassv.brm.fit.global),
        pars = c("b", "sd", "cor", "sigma"),
        ci = 0.90, format = "latex", caption = "Outcome2 Models Global Model (T) Alpha = .10- Cross Interaction",
        file = paste(filepath, "LatexTables/", "outcome2ModelFinalGlobalZ10", sep=""), 
        custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)
