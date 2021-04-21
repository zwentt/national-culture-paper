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


CultureData <- read_dta("/Users/zwen/Documents/GitHub/national-culture-paper/Data_Analysis/cultureDataFinal.dta")
#CultureData <- read_dta("D:/Documents/GitHub/national-culture-paper/Data_Analysis/cultureDataFinal.dta")

filepath <- "/Users/zwen/Documents/GitHub/national-culture-paper/Data_Analysis/"

#Work directory
setwd(filepath)




#gfuov_c + gpdiv_c + ginscolv_c + ghumv_c + gperv_c + gigrcolv_c + ggndv_c + gassv_c

# 1. Null Model

agility.null <- agility ~ 1 + (1 | countryx)
sensing.null <- sensing ~ 1 + (1 | countryx)
proactive.null <- proactive ~ 1 + (1 | countryx)

outcome.null <- outcome ~ 1 + (1 | countryx)
flexoutcome.null <- flexOutcome ~ 1 + (1 | countryx)
speedoutcome.null <- speedOutcome ~ 1 + (1 | countryx)

fit.agility.null <- brm(agility.null, data = CultureData, refresh = 0, core = 7)
fit.sensing.null <- brm(sensing.null, data = CultureData, refresh = 0, core = 7)
fit.proactive.null <- brm(proactive.null, data = CultureData, refresh = 0, core = 7)

fit.outcome.null <- brm(outcome.null, data = CultureData, refresh = 0, core = 7)
fit.flexoutcome.null <- brm(flexoutcome.null, data = CultureData, refresh = 0, core = 7)
fit.speedoutcome.null <- brm(speedoutcome.null, data = CultureData, refresh = 0, core = 7)


mcmcReg(list(fit.agility.null, 
             fit.sensing.null, 
             fit.proactive.null,
             fit.outcome.null, 
             fit.flexoutcome.null, 
             fit.speedoutcome.null),
        pars = c("b_Intercept", "sd_countryx__Intercept", "sigma"), 
        ci = 0.95, format = "latex", caption = "Null Models (Set 1)",
        file = paste(filepath, "LatexTables/", "nullModels", sep=""), 
        custom.model.names = c("Agility Practices", "Sensing Practices", "Proactive Practices",
                               "Agility Outcome", "Flexible Outcome", "Speed Outcome"))


agility2.null <- agility2 ~ 1 + (1 | countryx)
sensing2.null <- sensing2 ~ 1 + (1 | countryx)
proactive2.null <- proactive2 ~ 1 + (1 | countryx)

outcome2.null <- outcome2 ~ 1 + (1 | countryx)
flexoutcome2.null <- flexOutcome2 ~ 1 + (1 | countryx)
speedoutcome2.null <- speedOutcome2 ~ 1 + (1 | countryx)

fit.agility2.null <- brm(agility2.null, data = CultureData, refresh = 0, core = 7)
fit.sensing2.null <- brm(sensing2.null, data = CultureData, refresh = 0, core = 7)
fit.proactive2.null <- brm(proactive2.null, data = CultureData, refresh = 0, core = 7)

fit.outcome2.null <- brm(outcome2.null, data = CultureData, refresh = 0, core = 7)
fit.flexoutcome2.null <- brm(flexoutcome2.null, data = CultureData, refresh = 0, core = 7)
fit.speedoutcome2.null <- brm(speedoutcome2.null, data = CultureData, refresh = 0, core = 7)


mcmcReg(list(fit.agility2.null, 
             fit.sensing2.null, 
             fit.proactive2.null,
             fit.outcome2.null, 
             fit.flexoutcome2.null, 
             fit.speedoutcome2.null),
        pars = c("b_Intercept", "sd_countryx__Intercept", "sigma"), 
        ci = 0.95, format = "latex", caption = "Null Models (Set 2)",
        file = paste(filepath, "LatexTables/", "nullModels2", sep=""), 
        custom.model.names = c("Agility Practices2", "Sensing Practices2", "Proactive Practices2",
                               "Agility Outcome2", "Flexible Outcome2", "Speed Outcome2"))


# 1/6. Agility Cross Interaction Model 

rm(brm.fit.names)
brm.fit.names <- "agility.control.brm.fit"

for (v in glo_v) {
  brm.fit.names <- c(brm.fit.names, paste("agility.", v, ".brm.fit", sep = ""))
}

#Fitting Control Variable Only Model
#agility.control.brm.fit <- brm(agility ~ mfr2013std + network2xcj + firmsize.adj.cj + strategycj + competitive1cj + (1 | countryx), data = CultureData)

#All other models 
brm.agility.model <- agility ~ mfr2013.z + firmsize.adj.cj  + strategycj + (competitive1cj + network2xcj) *cul + competitive1cj:network2xcj:cul + (1 + network2xcj + competitive1cj | countryx)


#model estimations 
for (i in 1:9) {
  CultureData$cul <- as.numeric(unlist(CultureData[, c(glo_vz[i])]))
  assign(brm.fit.names[i+1], brm(brm.agility.model, data = CultureData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(15)
}


#Model Output using mcmcReg
mcmcReg(list(agility.guaiv.brm.fit,
             agility.gfuov.brm.fit,
             agility.gpdiv.brm.fit,
             agility.ginscolv.brm.fit,
             agility.ghumv.brm.fit,
             agility.gperv.brm.fit,
             agility.gigrcolv.brm.fit,
             agility.ggndv.brm.fit,
             agility.gassv.brm.fit),
        pars = c("b", "sd", "sigma", "cor"),
        ci = 0.95, format = "latex", caption = "Agility Models (Z) - Cross Interaction",
        file = paste(filepath, "LatexTables/", "agilityCrossModelsCompetitiveZ", sep=""), 
        custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)

# 2/6. Agility2 Cross Interaction Model - Competitive1 Variable

rm(brm.fit.names)
brm.fit.names <- "agility2.control.brm.fit"

for (v in glo_v) {
  brm.fit.names <- c(brm.fit.names, paste("agility2.", v, ".brm.fit", sep = ""))
}

#Fitting Control Variable Only Model
#agility2.control.brm.fit <- brm(agility2 ~ mfr2013lnstd + strategycj + network2xcj + firmsize.adj.cj + competitive1cj + (1 | countryx), data = CultureData)

#All other models 
brm.agility2.model <- agility2 ~ mfr2013.z + firmsize.adj.cj  + strategycj + (competitive1cj + network2xcj) *cul + competitive1cj:network2xcj:cul + (1 + network2xcj + competitive1cj | countryx)


#model estimations 
for (i in 1:9) {
  CultureData$cul <- as.numeric(unlist(CultureData[, c(glo_vz[i])]))
  assign(brm.fit.names[i+1], brm(brm.agility2.model, data = CultureData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(15)
}


#Model Output using mcmcReg
mcmcReg(list(agility2.guaiv.brm.fit,
             agility2.gfuov.brm.fit,
             agility2.gpdiv.brm.fit,
             agility2.ginscolv.brm.fit,
             agility2.ghumv.brm.fit,
             agility2.gperv.brm.fit,
             agility2.gigrcolv.brm.fit,
             agility2.ggndv.brm.fit,
             agility2.gassv.brm.fit),
        pars = c("b", "sd", "sigma", "cor"),
        ci = 0.95, format = "latex", caption = "Agility2 Models (Z) - Cross Interaction",
        file = paste(filepath, "LatexTables/", "agility2CrossModelsCompetitiveZ", sep=""), 
        custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)



# 3/6. Agility2 Cross Interaction Model Regional - Competitive1 Variable

rm(brm.fit.names)
brm.fit.names <- "agility2.control.brm.fit.regional"

for (v in glo_v) {
  brm.fit.names <- c(brm.fit.names, paste("agility2.", v, ".brm.fit.regional", sep = ""))
}

#Fitting Control Variable Only Model
#agility2.control.brm.fit <- brm(agility2 ~ mfr2013lnstd + strategycj + network2xcj + firmsize.adj.cj + competitive1cj + (1 | countryx), data = CultureData)

#All other models 
brm.agility2.model.regional <- agility2 ~ mfr2013.z + firmsize.adj.cj  + strategycj + (competitive1cj) *cul + competitive1cj:cul + (1 + competitive1cj | countryx)


#model estimations 
for (i in 1:9) {
  regionalData$cul <- as.numeric(unlist(regionalData[, c(glo_vz[i])]))
  assign(brm.fit.names[i+1], brm(brm.agility2.model.regional, data = regionalData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(15)
}


#Model Output using mcmcReg
mcmcReg(list(agility2.guaiv.brm.fit.regional,
             agility2.gfuov.brm.fit.regional,
             agility2.gpdiv.brm.fit.regional,
             agility2.ginscolv.brm.fit.regional,
             agility2.ghumv.brm.fit.regional,
             agility2.gperv.brm.fit.regional,
             agility2.gigrcolv.brm.fit.regional,
             agility2.ggndv.brm.fit.regional,
             agility2.gassv.brm.fit.regional),
        pars = c("b", "sd", "cor", "sigma"),
        ci = 0.95, format = "latex", caption = "Agility2 Models for Regional Firms (Z) - Cross Interaction",
        file = paste(filepath, "LatexTables/", "agility2CrossModelsCompetitiveRegionalZ", sep=""), 
        custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)


# 4/6. Agility2 Cross Interaction Model Global - Competitive1 Variable

rm(brm.fit.names)
brm.fit.names <- "agility2.control.brm.fit.global"

for (v in glo_v) {
  brm.fit.names <- c(brm.fit.names, paste("agility2.", v, ".brm.fit.global", sep = ""))
}

#Fitting Control Variable Only Model
#agility2.control.brm.fit <- brm(agility2 ~ mfr2013lnstd + strategycj + network2xcj + firmsize.adj.cj + competitive1cj + (1 | countryx), data = CultureData)

#All other models 
brm.agility2.model.global <- agility2 ~ mfr2013.z + firmsize.adj.cj  + strategycj + (competitive1cj) *cul + competitive1cj:cul + 
  (1 + competitive1cj | countryx)


#model estimations 
for (i in 1:9) {
  globalData$cul <- as.numeric(unlist(globalData[, c(glo_vz[i])]))
  assign(brm.fit.names[i+1], brm(brm.agility2.model.global, data = globalData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(15)
}


#Model Output using mcmcReg
mcmcReg(list(agility2.guaiv.brm.fit.global,
             agility2.gfuov.brm.fit.global,
             agility2.gpdiv.brm.fit.global,
             agility2.ginscolv.brm.fit.global,
             agility2.ghumv.brm.fit.global,
             agility2.gperv.brm.fit.global,
             agility2.gigrcolv.brm.fit.global,
             agility2.ggndv.brm.fit.global,
             agility2.gassv.brm.fit.global),
        pars = c("b", "sd", "cor", "sigma"),
        ci = 0.95, format = "latex", caption = "Agility2 Models for Global Firms (Z) - Cross Interaction",
        file = paste(filepath, "LatexTables/", "agility2CrossModelsCompetitiveGlobalZ", sep=""), 
        custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)

