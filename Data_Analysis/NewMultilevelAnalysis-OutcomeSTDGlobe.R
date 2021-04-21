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
#outcome2.control.brm.fit <- brm(outcome2 ~ mfr2013lnstd + network2xcj + firmsize.adj.cj + (1 | countryx), data = CultureData)

brm.outcome2.model <- outcome2 ~ mfr2013.z + firmsize.adj.cj + agility2cj + network2xcj + 
                        agility2cj * (cul + network2xcj + strategycj) + 
                        strategycj:agility2cj:cul + agility2cj:network2xcj:cul + strategycj:agility2cj:network2xcj + 
                        (1 + network2xcj + agility2cj | countryx)


#model estimations 
for (i in 1:9) {
  CultureData$cul <- as.numeric(unlist(CultureData[, c(glo_vz[i])]))
  assign(brm.outcome.fit.names[i+1], brm(brm.outcome2.model, data = CultureData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(20)
}


#Model Output using mcmcReg
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
        ci = 0.95, format = "latex", caption = "Outcome2 Models (Z) - Cross Interaction",
        file = paste(filepath, "LatexTables/", "outcome2ModelFinalZ", sep=""), 
        custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)


#2/6. Outcome2 Models Regional
rm(brm.outcome.fit.names)
brm.outcome.fit.names <- "outcome2.control.brm.fit.regional"

for (v in glo_v) {
  brm.outcome.fit.names <- c(brm.outcome.fit.names, paste("outcome2.", v, ".brm.fit.regional", sep = ""))
}

#Fitting Control Variable Only Model
#outcome2.control.brm.fit <- brm(outcome2 ~ mfr2013lnstd + network2xcj + firmsize.adj.cj + (1 | countryx), data = CultureData)

brm.outcome2.model.regional <- outcome2 ~ mfr2013.z + firmsize.adj.cj + agility2cj + 
  agility2cj * (cul + strategycj) + strategycj:agility2cj:cul + (1 + agility2cj | countryx)


#model estimations 
for (i in 1:9) {
  regionalData$cul <- as.numeric(unlist(regionalData[, c(glo_vc[i])]))
  assign(brm.outcome.fit.names[i+1], brm(brm.outcome2.model.regional, data = regionalData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
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
        ci = 0.95, format = "latex", caption = "Outcome2 Models Regional Model - Cross Interaction",
        file = paste(filepath, "LatexTables/", "outcome2ModelFinalRegional", sep=""), 
        custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)


#3/6. Outcome2 Models Global
rm(brm.outcome.fit.names)
brm.outcome.fit.names <- "outcome2.control.brm.fit.global"

for (v in glo_v) {
  brm.outcome.fit.names <- c(brm.outcome.fit.names, paste("outcome2.", v, ".brm.fit.global", sep = ""))
}

#Fitting Control Variable Only Model
#outcome2.control.brm.fit <- brm(outcome2 ~ mfr2013lnstd + network2xcj + firmsize.adj.cj + (1 | countryx), data = CultureData)

brm.outcome2.model.global <- outcome2 ~ mfr2013std + firmsize.adj.cj + agility2cj + 
  agility2cj * (cul + strategycj) + strategycj:agility2cj:cul + (1 + agility2cj | countryx)


#model estimations 
for (i in 1:9) {
  globalData$cul <- as.numeric(unlist(globalData[, c(glo_vc[i])]))
  assign(brm.outcome.fit.names[i+1], brm(brm.outcome2.model.global, data = globalData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
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
        ci = 0.95, format = "latex", caption = "Outcome2 Models Global Model - Cross Interaction",
        file = paste(filepath, "LatexTables/", "outcome2ModelFinalGlobal", sep=""), 
        custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)
