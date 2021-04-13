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
CultureData <- read_dta("D:/Documents/GitHub/national-culture-paper/Data_Analysis/cultureDataFinal.dta")

filepath <- "/Users/zwen/Documents/GitHub/national-culture-paper/Data_Analysis/"

#Work directory
setwd(filepath)


#Variables 
hof = c("pdi", "idv", "lto", "ivr", "mas", "uai")
glo_v = c("guaiv", "gfuov", "gpdiv", "ginscolv", "ghumv", "gperv", "gigrcolv", "ggndv", "gassv")
glo_p = c("guaip", "gfuop", "gpdip", "ginscolp", "ghump", "gpefp", "gigrcolp", "ggndp", "gassp")
glo_pc = c("guaip_c", "gfuop_c", "gpdip_c", "ginscolp_c", "ghump_c", "gpefp_c", "gigrcolp_c", "ggndp_c", "gassp_c")
glo_vc = c("guaiv_c", "gfuov_c", "gpdiv_c", "ginscolv_c", "ghumv_c", "gperv_c", "gigrcolv_c", "ggndv_c", "gassv_c")

#Additional data conditioning
#firmsize variable log transform then center within cluster

CultureData$firmsize.adj <- log10(CultureData$firmsizecont)
CultureData$firmsize.adj.cj <- CultureData$firmsize.adj - ave(CultureData$firmsize.adj, CultureData$country)

regionalData <- subset(CultureData, network2x == 0)
globalData <- subset(CultureData, network2x == 1)
eachCountry <- subset(CultureData, pickone == 1)




#1/6. Outcome Models 
rm(brm.outcome.fit.names)
brm.outcome.fit.names <- "outcome.control.brm.fit"

for (v in glo_v) {
  brm.outcome.fit.names <- c(brm.outcome.fit.names, paste("outcome.", v, ".brm.fit", sep = ""))
}

#Fitting Control Variable Only Model
outcome.control.brm.fit <- brm(outcome ~ mfr2013lnstd + network2xcj + firmsize.adj.cj + (1 | countryx), data = CultureData)

brm.outcome.model <- outcome ~ mfr2013lnstd +  firmsize.adj.cj + network2xcj * agilitycj * cul + (1 + network2xcj + agilitycj | countryx)
#brm.outcome.model <- outcome ~ mfr2013lnstd + (network2xcj + firmsize.adj.cj) * agilitycj * cul + (1 + firmsize.adj.cj + network2xcj + agilitycj | countryx)


#model estimations 
for (i in 1:9) {
  CultureData$cul <- as.numeric(unlist(CultureData[, c(glo_vc[i])]))
  assign(brm.outcome.fit.names[i+1], brm(brm.outcome.model, data = CultureData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(20)
}

#Model Output using mcmcReg
mcmcReg(list(outcome.control.brm.fit,
             outcome.guaiv.brm.fit,
             outcome.gfuov.brm.fit,
             outcome.gpdiv.brm.fit,
             outcome.ginscolv.brm.fit,
             outcome.ghumv.brm.fit,
             outcome.gperv.brm.fit,
             outcome.gigrcolv.brm.fit,
             outcome.ggndv.brm.fit,
             outcome.gassv.brm.fit),
        pars = c("b", "sd", "sigma"),
        ci = 0.95, format = "latex", caption = "Outcome Models - Cross Interaction",
        file = paste(filepath, "LatexTables/", "04122021-outcomeModel", sep=""), 
        custom.model.names = c("Control", "UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)



#2/6. Outcome Models 
rm(brm.outcome2.fit.names)
brm.outcome2.fit.names <- "outcome2.control.brm.fit"

for (v in glo_v) {
  brm.outcome2.fit.names <- c(brm.outcome2.fit.names, paste("outcome2.", v, ".brm.fit", sep = ""))
}

#Fitting Control Variable Only Model
outcome2.control.brm.fit <- brm(outcome2 ~ mfr2013lnstd + network2xcj + firmsize.adj.cj + (1 | countryx), data = CultureData)

brm.outcome2.model <- outcome2 ~ mfr2013lnstd + firmsize.adj.cj + network2xcj * agility2cj * cul + (1 + network2xcj + agility2cj | countryx)
#brm.outcome2.model <- outcome2 ~ mfr2013lnstd + (network2xcj + firmsize.adj.cj) * agility2cj * cul + (1 + firmsize.adj.cj + network2xcj + agility2cj | countryx)


#model estimations 
for (i in 1:9) {
  CultureData$cul <- as.numeric(unlist(CultureData[, c(glo_vc[i])]))
  assign(brm.outcome2.fit.names[i+1], brm(brm.outcome2.model, data = CultureData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(20)
}


#Model Output using mcmcReg
mcmcReg(list(outcome2.control.brm.fit,
             outcome2.guaiv.brm.fit,
             outcome2.gfuov.brm.fit,
             outcome2.gpdiv.brm.fit,
             outcome2.ginscolv.brm.fit,
             outcome2.ghumv.brm.fit,
             outcome2.gperv.brm.fit,
             outcome2.gigrcolv.brm.fit,
             outcome2.ggndv.brm.fit,
             outcome2.gassv.brm.fit),
        pars = c("b", "sd", "sigma"),
        ci = 0.95, format = "latex", caption = "Outcome2 Models - Cross Interaction",
        file = paste(filepath, "LatexTables/", "04122021-outcome2Model", sep=""), 
        custom.model.names = c("Control", "UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)


#3/6. Outcome Models - Regional 
rm(brm.outcome.fit.names.regional)
brm.outcome.fit.names.regional <- "outcome.control.brm.fit.regional"

for (v in glo_v) {
  brm.outcome.fit.names.regional <- c(brm.outcome.fit.names.regional, paste("outcome.", v, ".brm.fit.regional", sep = ""))
}

#Fitting Control Variable Only Model
outcome.control.brm.fit.regional <- brm(outcome ~ mfr2013lnstd + firmsize.adj.cj + (1 | countryx), data = regionalData)

brm.outcome.model.regional <- outcome ~ mfr2013lnstd + firmsize.adj.cj + agilitycj * cul + (1 + agilitycj | countryx)
#brm.outcome.model.regional <- outcome ~ mfr2013lnstd + firmsize.adj.cj * agilitycj * cul + (1 + firmsize.adj.cj + agilitycj | countryx)


#model estimations 
for (i in 1:9) {
  regionalData$cul <- as.numeric(unlist(regionalData[, c(glo_vc[i])]))
  assign(brm.outcome.fit.names.regional[i+1], brm(brm.outcome.model.regional, data = regionalData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(20)
}


#Model Output using mcmcReg
mcmcReg(list(outcome.control.brm.fit.regional,
             outcome.guaiv.brm.fit.regional,
             outcome.gfuov.brm.fit.regional,
             outcome.gpdiv.brm.fit.regional,
             outcome.ginscolv.brm.fit.regional,
             outcome.ghumv.brm.fit.regional,
             outcome.gperv.brm.fit.regional,
             outcome.gigrcolv.brm.fit.regional,
             outcome.ggndv.brm.fit.regional,
             outcome.gassv.brm.fit.regional),
        pars = c("b", "sd", "sigma"),
        ci = 0.95, format = "latex", caption = "Outcome Models (Regional) - Cross Interaction",
        file = paste(filepath, "LatexTables/", "04122021-outcomeModelRegional", sep=""), 
        custom.model.names = c("Control", "UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)


#4/6. Outcome Models - Global 
rm(brm.outcome.fit.names.global)
brm.outcome.fit.names.global <- "outcome.control.brm.fit.global"

for (v in glo_v) {
  brm.outcome.fit.names.global <- c(brm.outcome.fit.names.global, paste("outcome.", v, ".brm.fit.global", sep = ""))
}

#Fitting Control Variable Only Model
outcome.control.brm.fit.global <- brm(outcome ~ mfr2013lnstd + firmsize.adj.cj + (1 | countryx), data = globalData)

brm.outcome.model.global <- outcome ~ mfr2013lnstd + firmsize.adj.cj + agilitycj * cul + (1 + agilitycj | countryx)
#brm.outcome.model.global <- outcome ~ mfr2013lnstd + firmsize.adj.cj * agilitycj * cul + (1 + firmsize.adj.cj + agilitycj | countryx)


#model estimations 
for (i in 1:9) {
  globalData$cul <- as.numeric(unlist(globalData[, c(glo_vc[i])]))
  assign(brm.outcome.fit.names.global[i+1], brm(brm.outcome.model.global, data = globalData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(20)
}


#Model Output using mcmcReg
mcmcReg(list(outcome.control.brm.fit.global,
             outcome.guaiv.brm.fit.global,
             outcome.gfuov.brm.fit.global,
             outcome.gpdiv.brm.fit.global,
             outcome.ginscolv.brm.fit.global,
             outcome.ghumv.brm.fit.global,
             outcome.gperv.brm.fit.global,
             outcome.gigrcolv.brm.fit.global,
             outcome.ggndv.brm.fit.global,
             outcome.gassv.brm.fit.global),
        pars = c("b", "sd", "sigma"),
        ci = 0.95, format = "latex", caption = "Outcome Models (Global) - Cross Interaction",
        file = paste(filepath, "LatexTables/", "04122021-outcomeModelGlobal", sep=""), 
        custom.model.names = c("Control", "UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)




#5/6. Outcome2 Models - Regional 
rm(brm.outcome2.fit.names.regional)
brm.outcome2.fit.names.regional <- "outcome2.control.brm.fit.regional"

for (v in glo_v) {
  brm.outcome2.fit.names.regional <- c(brm.outcome2.fit.names.regional, paste("outcome2.", v, ".brm.fit.regional", sep = ""))
}

#Fitting Control Variable Only Model
outcome2.control.brm.fit.regional <- brm(outcome2 ~ mfr2013lnstd + firmsize.adj.cj + (1 | countryx), data = regionalData)

brm.outcome2.model.regional <- outcome2 ~ mfr2013lnstd + firmsize.adj.cj + agility2cj * cul + (1 + agility2cj | countryx)
#brm.outcome2.model.regional <- outcome2 ~ mfr2013lnstd + firmsize.adj.cj * agility2cj * cul + (1 + firmsize.adj.cj + agility2cj | countryx)

#model estimations 
for (i in 1:9) {
  regionalData$cul <- as.numeric(unlist(regionalData[, c(glo_vc[i])]))
  assign(brm.outcome2.fit.names.regional[i+1], brm(brm.outcome2.model.regional, data = regionalData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(20)
}


#Model Output using mcmcReg
mcmcReg(list(outcome2.control.brm.fit.regional,
             outcome2.guaiv.brm.fit.regional,
             outcome2.gfuov.brm.fit.regional,
             outcome2.gpdiv.brm.fit.regional,
             outcome2.ginscolv.brm.fit.regional,
             outcome2.ghumv.brm.fit.regional,
             outcome2.gperv.brm.fit.regional,
             outcome2.gigrcolv.brm.fit.regional,
             outcome2.ggndv.brm.fit.regional,
             outcome2.gassv.brm.fit.regional),
        pars = c("b", "sd", "sigma"),
        ci = 0.95, format = "latex", caption = "Outcome2 Models (Regional) - Cross Interaction",
        file = paste(filepath, "LatexTables/", "04122021-outcome2ModelRegional", sep=""), 
        custom.model.names = c("Control", "UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)


#6/6. Outcome2 Models - Global 
rm(brm.outcome2.fit.names.global)
brm.outcome2.fit.names.global <- "outcome2.control.brm.fit.global"

for (v in glo_v) {
  brm.outcome2.fit.names.global <- c(brm.outcome2.fit.names.global, paste("outcome2.", v, ".brm.fit.global", sep = ""))
}

#Fitting Control Variable Only Model
outcome2.control.brm.fit.global <- brm(outcome2 ~ mfr2013lnstd + firmsize.adj.cj + (1 | countryx), data = globalData)

brm.outcome2.model.global <- outcome2 ~ mfr2013lnstd + firmsize.adj.cj + agility2cj * cul + (1 + agility2cj | countryx)
#brm.outcome2.model.global <- outcome2 ~ mfr2013lnstd + firmsize.adj.cj * agility2cj * cul + (1 + firmsize.adj.cj + agility2cj | countryx)


#model estimations 
for (i in 1:9) {
  globalData$cul <- as.numeric(unlist(globalData[, c(glo_vc[i])]))
  assign(brm.outcome2.fit.names.global[i+1], brm(brm.outcome2.model.global, data = globalData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(20)
}


#Model Output using mcmcReg
mcmcReg(list(outcome2.control.brm.fit.global,
             outcome2.guaiv.brm.fit.global,
             outcome2.gfuov.brm.fit.global,
             outcome2.gpdiv.brm.fit.global,
             outcome2.ginscolv.brm.fit.global,
             outcome2.ghumv.brm.fit.global,
             outcome2.gperv.brm.fit.global,
             outcome2.gigrcolv.brm.fit.global,
             outcome2.ggndv.brm.fit.global,
             outcome2.gassv.brm.fit.global),
        pars = c("b", "sd", "sigma"),
        ci = 0.95, format = "latex", caption = "Outcome2 Models (Global) - Cross Interaction",
        file = paste(filepath, "LatexTables/", "04122021-outcome2ModelGlobal", sep=""), 
        custom.model.names = c("Control", "UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)
