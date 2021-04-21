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



#1/6. flexOutcome Models 
rm(brm.flexOutcome.fit.names)
brm.flexOutcome.fit.names <- "flexOutcome.control.brm.fit"

for (v in glo_v) {
  brm.flexOutcome.fit.names <- c(brm.flexOutcome.fit.names, paste("flexOutcome.", v, ".brm.fit", sep = ""))
}

#Fitting Control Variable Only Model
flexOutcome.control.brm.fit <- brm(flexOutcome ~ mfr2013lnstd + network2xcj + firmsize.adj.cj + (1 | countryx), data = CultureData)

brm.flexOutcome.model <- flexOutcome ~ mfr2013lnstd +  firmsize.adj.cj + network2xcj * agilitycj * cul + (1 + network2xcj + agilitycj | countryx)
#brm.flexOutcome.model <- flexOutcome ~ mfr2013lnstd + (network2xcj + firmsize.adj.cj) * agilitycj * cul + (1 + firmsize.adj.cj + network2xcj + agilitycj | countryx)


#model estimations 
for (i in 1:9) {
  CultureData$cul <- as.numeric(unlist(CultureData[, c(glo_vc[i])]))
  assign(brm.flexOutcome.fit.names[i+1], brm(brm.flexOutcome.model, data = CultureData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(20)
}

#Model Output using mcmcReg
mcmcReg(list(flexOutcome.control.brm.fit,
             flexOutcome.guaiv.brm.fit,
             flexOutcome.gfuov.brm.fit,
             flexOutcome.gpdiv.brm.fit,
             flexOutcome.ginscolv.brm.fit,
             flexOutcome.ghumv.brm.fit,
             flexOutcome.gperv.brm.fit,
             flexOutcome.gigrcolv.brm.fit,
             flexOutcome.ggndv.brm.fit,
             flexOutcome.gassv.brm.fit),
        pars = c("b", "sd", "sigma"),
        ci = 0.95, format = "latex", caption = "flexOutcome Models - Cross Interaction",
        file = paste(filepath, "LatexTables/", "04122021-flexOutcomeModel", sep=""), 
        custom.model.names = c("Control", "UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)



#2/6. flexOutcome2 Models 
rm(brm.flexOutcome2.fit.names)
brm.flexOutcome2.fit.names <- "flexOutcome2.control.brm.fit"

for (v in glo_v) {
  brm.flexOutcome2.fit.names <- c(brm.flexOutcome2.fit.names, paste("flexOutcome2.", v, ".brm.fit", sep = ""))
}

#Fitting Control Variable Only Model
flexOutcome2.control.brm.fit <- brm(flexOutcome2 ~ mfr2013lnstd + network2xcj + firmsize.adj.cj + (1 | countryx), data = CultureData)

brm.flexOutcome2.model <- flexOutcome2 ~ mfr2013lnstd + firmsize.adj.cj + network2xcj * agility2cj * cul + (1 + network2xcj + agility2cj | countryx)
#brm.flexOutcome2.model <- flexOutcome2 ~ mfr2013lnstd + (network2xcj + firmsize.adj.cj) * agility2cj * cul + (1 + firmsize.adj.cj + network2xcj + agility2cj | countryx)


#model estimations 
for (i in 1:9) {
  CultureData$cul <- as.numeric(unlist(CultureData[, c(glo_vc[i])]))
  assign(brm.flexOutcome2.fit.names[i+1], brm(brm.flexOutcome2.model, data = CultureData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(20)
}


#Model Output using mcmcReg
mcmcReg(list(flexOutcome2.control.brm.fit,
             flexOutcome2.guaiv.brm.fit,
             flexOutcome2.gfuov.brm.fit,
             flexOutcome2.gpdiv.brm.fit,
             flexOutcome2.ginscolv.brm.fit,
             flexOutcome2.ghumv.brm.fit,
             flexOutcome2.gperv.brm.fit,
             flexOutcome2.gigrcolv.brm.fit,
             flexOutcome2.ggndv.brm.fit,
             flexOutcome2.gassv.brm.fit),
        pars = c("b", "sd", "sigma"),
        ci = 0.95, format = "latex", caption = "flexOutcome2 Models - Cross Interaction",
        file = paste(filepath, "LatexTables/", "04122021-flexOutcome2Model", sep=""), 
        custom.model.names = c("Control", "UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)


#3/6. flexOutcome Models - Regional 
rm(brm.flexOutcome.fit.names.regional)
brm.flexOutcome.fit.names.regional <- "flexOutcome.control.brm.fit.regional"

for (v in glo_v) {
  brm.flexOutcome.fit.names.regional <- c(brm.flexOutcome.fit.names.regional, paste("flexOutcome.", v, ".brm.fit.regional", sep = ""))
}

#Fitting Control Variable Only Model
flexOutcome.control.brm.fit.regional <- brm(flexOutcome ~ mfr2013lnstd + firmsize.adj.cj + (1 | countryx), data = regionalData)

brm.flexOutcome.model.regional <- flexOutcome ~ mfr2013lnstd + firmsize.adj.cj + agilitycj * cul + (1 + agilitycj | countryx)
#brm.flexOutcome.model.regional <- flexOutcome ~ mfr2013lnstd + firmsize.adj.cj * agilitycj * cul + (1 + firmsize.adj.cj + agilitycj | countryx)


#model estimations 
for (i in 1:9) {
  regionalData$cul <- as.numeric(unlist(regionalData[, c(glo_vc[i])]))
  assign(brm.flexOutcome.fit.names.regional[i+1], brm(brm.flexOutcome.model.regional, data = regionalData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(20)
}


#Model Output using mcmcReg
mcmcReg(list(flexOutcome.control.brm.fit.regional,
             flexOutcome.guaiv.brm.fit.regional,
             flexOutcome.gfuov.brm.fit.regional,
             flexOutcome.gpdiv.brm.fit.regional,
             flexOutcome.ginscolv.brm.fit.regional,
             flexOutcome.ghumv.brm.fit.regional,
             flexOutcome.gperv.brm.fit.regional,
             flexOutcome.gigrcolv.brm.fit.regional,
             flexOutcome.ggndv.brm.fit.regional,
             flexOutcome.gassv.brm.fit.regional),
        pars = c("b", "sd", "sigma"),
        ci = 0.95, format = "latex", caption = "flexOutcome Models (Regional) - Cross Interaction",
        file = paste(filepath, "LatexTables/", "04122021-flexOutcomeModelRegional", sep=""), 
        custom.model.names = c("Control", "UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)


#4/6. flexOutcome Models - Global 
rm(brm.flexOutcome.fit.names.global)
brm.flexOutcome.fit.names.global <- "flexOutcome.control.brm.fit.global"

for (v in glo_v) {
  brm.flexOutcome.fit.names.global <- c(brm.flexOutcome.fit.names.global, paste("flexOutcome.", v, ".brm.fit.global", sep = ""))
}

#Fitting Control Variable Only Model
flexOutcome.control.brm.fit.global <- brm(flexOutcome ~ mfr2013lnstd + firmsize.adj.cj + (1 | countryx), data = globalData)

brm.flexOutcome.model.global <- flexOutcome ~ mfr2013lnstd + firmsize.adj.cj + agilitycj * cul + (1 + agilitycj | countryx)
#brm.flexOutcome.model.global <- flexOutcome ~ mfr2013lnstd + firmsize.adj.cj * agilitycj * cul + (1 + firmsize.adj.cj + agilitycj | countryx)


#model estimations 
for (i in 1:9) {
  globalData$cul <- as.numeric(unlist(globalData[, c(glo_vc[i])]))
  assign(brm.flexOutcome.fit.names.global[i+1], brm(brm.flexOutcome.model.global, data = globalData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(20)
}


#Model Output using mcmcReg
mcmcReg(list(flexOutcome.control.brm.fit.global,
             flexOutcome.guaiv.brm.fit.global,
             flexOutcome.gfuov.brm.fit.global,
             flexOutcome.gpdiv.brm.fit.global,
             flexOutcome.ginscolv.brm.fit.global,
             flexOutcome.ghumv.brm.fit.global,
             flexOutcome.gperv.brm.fit.global,
             flexOutcome.gigrcolv.brm.fit.global,
             flexOutcome.ggndv.brm.fit.global,
             flexOutcome.gassv.brm.fit.global),
        pars = c("b", "sd", "sigma"),
        ci = 0.95, format = "latex", caption = "flexOutcome Models (Global) - Cross Interaction",
        file = paste(filepath, "LatexTables/", "04122021-flexOutcomeModelGlobal", sep=""), 
        custom.model.names = c("Control", "UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)




#5/6. flexOutcome2 Models - Regional 
rm(brm.flexOutcome2.fit.names.regional)
brm.flexOutcome2.fit.names.regional <- "flexOutcome2.control.brm.fit.regional"

for (v in glo_v) {
  brm.flexOutcome2.fit.names.regional <- c(brm.flexOutcome2.fit.names.regional, paste("flexOutcome2.", v, ".brm.fit.regional", sep = ""))
}

#Fitting Control Variable Only Model
flexOutcome2.control.brm.fit.regional <- brm(flexOutcome2 ~ mfr2013lnstd + firmsize.adj.cj + (1 | countryx), data = regionalData)

brm.flexOutcome2.model.regional <- flexOutcome2 ~ mfr2013lnstd + firmsize.adj.cj + agility2cj * cul + (1 + agility2cj | countryx)
#brm.flexOutcome2.model.regional <- flexOutcome2 ~ mfr2013lnstd + firmsize.adj.cj * agility2cj * cul + (1 + firmsize.adj.cj + agility2cj | countryx)

#model estimations 
for (i in 1:9) {
  regionalData$cul <- as.numeric(unlist(regionalData[, c(glo_vc[i])]))
  assign(brm.flexOutcome2.fit.names.regional[i+1], brm(brm.flexOutcome2.model.regional, data = regionalData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(20)
}


#Model Output using mcmcReg
mcmcReg(list(flexOutcome2.control.brm.fit.regional,
             flexOutcome2.guaiv.brm.fit.regional,
             flexOutcome2.gfuov.brm.fit.regional,
             flexOutcome2.gpdiv.brm.fit.regional,
             flexOutcome2.ginscolv.brm.fit.regional,
             flexOutcome2.ghumv.brm.fit.regional,
             flexOutcome2.gperv.brm.fit.regional,
             flexOutcome2.gigrcolv.brm.fit.regional,
             flexOutcome2.ggndv.brm.fit.regional,
             flexOutcome2.gassv.brm.fit.regional),
        pars = c("b", "sd", "sigma"),
        ci = 0.95, format = "latex", caption = "flexOutcome2 Models (Regional) - Cross Interaction",
        file = paste(filepath, "LatexTables/", "04122021-flexOutcome2ModelRegional", sep=""), 
        custom.model.names = c("Control", "UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)


#6/6. flexOutcome2 Models - Global 
rm(brm.flexOutcome2.fit.names.global)
brm.flexOutcome2.fit.names.global <- "flexOutcome2.control.brm.fit.global"

for (v in glo_v) {
  brm.flexOutcome2.fit.names.global <- c(brm.flexOutcome2.fit.names.global, paste("flexOutcome2.", v, ".brm.fit.global", sep = ""))
}

#Fitting Control Variable Only Model
flexOutcome2.control.brm.fit.global <- brm(flexOutcome2 ~ mfr2013lnstd + firmsize.adj.cj + (1 | countryx), data = globalData)

brm.flexOutcome2.model.global <- flexOutcome2 ~ mfr2013lnstd + firmsize.adj.cj + agility2cj * cul + (1 + agility2cj | countryx)
#brm.flexOutcome2.model.global <- flexOutcome2 ~ mfr2013lnstd + firmsize.adj.cj * agility2cj * cul + (1 + firmsize.adj.cj + agility2cj | countryx)


#model estimations 
for (i in 1:9) {
  globalData$cul <- as.numeric(unlist(globalData[, c(glo_vc[i])]))
  assign(brm.flexOutcome2.fit.names.global[i+1], brm(brm.flexOutcome2.model.global, data = globalData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(20)
}


#Model Output using mcmcReg
mcmcReg(list(flexOutcome2.control.brm.fit.global,
             flexOutcome2.guaiv.brm.fit.global,
             flexOutcome2.gfuov.brm.fit.global,
             flexOutcome2.gpdiv.brm.fit.global,
             flexOutcome2.ginscolv.brm.fit.global,
             flexOutcome2.ghumv.brm.fit.global,
             flexOutcome2.gperv.brm.fit.global,
             flexOutcome2.gigrcolv.brm.fit.global,
             flexOutcome2.ggndv.brm.fit.global,
             flexOutcome2.gassv.brm.fit.global),
        pars = c("b", "sd", "sigma"),
        ci = 0.95, format = "latex", caption = "flexOutcome2 Models (Global) - Cross Interaction",
        file = paste(filepath, "LatexTables/", "04122021-flexOutcome2ModelGlobal", sep=""), 
        custom.model.names = c("Control", "UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)




#1/6. speedOutcome Models 
rm(brm.speedOutcome.fit.names)
brm.speedOutcome.fit.names <- "speedOutcome.control.brm.fit"

for (v in glo_v) {
  brm.speedOutcome.fit.names <- c(brm.speedOutcome.fit.names, paste("speedOutcome.", v, ".brm.fit", sep = ""))
}

#Fitting Control Variable Only Model
speedOutcome.control.brm.fit <- brm(speedOutcome ~ mfr2013lnstd + network2xcj + firmsize.adj.cj + (1 | countryx), data = CultureData)

brm.speedOutcome.model <- speedOutcome ~ mfr2013lnstd +  firmsize.adj.cj + network2xcj * agilitycj * cul + (1 + network2xcj + agilitycj | countryx)
#brm.speedOutcome.model <- speedOutcome ~ mfr2013lnstd + (network2xcj + firmsize.adj.cj) * agilitycj * cul + (1 + firmsize.adj.cj + network2xcj + agilitycj | countryx)


#model estimations 
for (i in 1:9) {
  CultureData$cul <- as.numeric(unlist(CultureData[, c(glo_vc[i])]))
  assign(brm.speedOutcome.fit.names[i+1], brm(brm.speedOutcome.model, data = CultureData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(20)
}

#Model Output using mcmcReg
mcmcReg(list(speedOutcome.control.brm.fit,
             speedOutcome.guaiv.brm.fit,
             speedOutcome.gfuov.brm.fit,
             speedOutcome.gpdiv.brm.fit,
             speedOutcome.ginscolv.brm.fit,
             speedOutcome.ghumv.brm.fit,
             speedOutcome.gperv.brm.fit,
             speedOutcome.gigrcolv.brm.fit,
             speedOutcome.ggndv.brm.fit,
             speedOutcome.gassv.brm.fit),
        pars = c("b", "sd", "sigma"),
        ci = 0.95, format = "latex", caption = "speedOutcome Models - Cross Interaction",
        file = paste(filepath, "LatexTables/", "04122021-speedOutcomeModel", sep=""), 
        custom.model.names = c("Control", "UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)



#2/6. speedOutcome2 Models 
rm(brm.speedOutcome2.fit.names)
brm.speedOutcome2.fit.names <- "speedOutcome2.control.brm.fit"

for (v in glo_v) {
  brm.speedOutcome2.fit.names <- c(brm.speedOutcome2.fit.names, paste("speedOutcome2.", v, ".brm.fit", sep = ""))
}

#Fitting Control Variable Only Model
speedOutcome2.control.brm.fit <- brm(speedOutcome2 ~ mfr2013lnstd + network2xcj + firmsize.adj.cj + (1 | countryx), data = CultureData)

brm.speedOutcome2.model <- speedOutcome2 ~ mfr2013lnstd + firmsize.adj.cj + network2xcj * agility2cj * cul + (1 + network2xcj + agility2cj | countryx)
#brm.speedOutcome2.model <- speedOutcome2 ~ mfr2013lnstd + (network2xcj + firmsize.adj.cj) * agility2cj * cul + (1 + firmsize.adj.cj + network2xcj + agility2cj | countryx)


#model estimations 
for (i in 1:9) {
  CultureData$cul <- as.numeric(unlist(CultureData[, c(glo_vc[i])]))
  assign(brm.speedOutcome2.fit.names[i+1], brm(brm.speedOutcome2.model, data = CultureData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(20)
}


#Model Output using mcmcReg
mcmcReg(list(speedOutcome2.control.brm.fit,
             speedOutcome2.guaiv.brm.fit,
             speedOutcome2.gfuov.brm.fit,
             speedOutcome2.gpdiv.brm.fit,
             speedOutcome2.ginscolv.brm.fit,
             speedOutcome2.ghumv.brm.fit,
             speedOutcome2.gperv.brm.fit,
             speedOutcome2.gigrcolv.brm.fit,
             speedOutcome2.ggndv.brm.fit,
             speedOutcome2.gassv.brm.fit),
        pars = c("b", "sd", "sigma"),
        ci = 0.95, format = "latex", caption = "speedOutcome2 Models - Cross Interaction",
        file = paste(filepath, "LatexTables/", "04122021-speedOutcome2Model", sep=""), 
        custom.model.names = c("Control", "UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)


#3/6. speedOutcome Models - Regional 
rm(brm.speedOutcome.fit.names.regional)
brm.speedOutcome.fit.names.regional <- "speedOutcome.control.brm.fit.regional"

for (v in glo_v) {
  brm.speedOutcome.fit.names.regional <- c(brm.speedOutcome.fit.names.regional, paste("speedOutcome.", v, ".brm.fit.regional", sep = ""))
}

#Fitting Control Variable Only Model
speedOutcome.control.brm.fit.regional <- brm(speedOutcome ~ mfr2013lnstd + firmsize.adj.cj + (1 | countryx), data = regionalData)

brm.speedOutcome.model.regional <- speedOutcome ~ mfr2013lnstd + firmsize.adj.cj + agilitycj * cul + (1 + agilitycj | countryx)
#brm.speedOutcome.model.regional <- speedOutcome ~ mfr2013lnstd + firmsize.adj.cj * agilitycj * cul + (1 + firmsize.adj.cj + agilitycj | countryx)


#model estimations 
for (i in 1:9) {
  regionalData$cul <- as.numeric(unlist(regionalData[, c(glo_vc[i])]))
  assign(brm.speedOutcome.fit.names.regional[i+1], brm(brm.speedOutcome.model.regional, data = regionalData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(20)
}


#Model Output using mcmcReg
mcmcReg(list(speedOutcome.control.brm.fit.regional,
             speedOutcome.guaiv.brm.fit.regional,
             speedOutcome.gfuov.brm.fit.regional,
             speedOutcome.gpdiv.brm.fit.regional,
             speedOutcome.ginscolv.brm.fit.regional,
             speedOutcome.ghumv.brm.fit.regional,
             speedOutcome.gperv.brm.fit.regional,
             speedOutcome.gigrcolv.brm.fit.regional,
             speedOutcome.ggndv.brm.fit.regional,
             speedOutcome.gassv.brm.fit.regional),
        pars = c("b", "sd", "sigma"),
        ci = 0.95, format = "latex", caption = "speedOutcome Models (Regional) - Cross Interaction",
        file = paste(filepath, "LatexTables/", "04122021-speedOutcomeModelRegional", sep=""), 
        custom.model.names = c("Control", "UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)


#4/6. speedOutcome Models - Global 
rm(brm.speedOutcome.fit.names.global)
brm.speedOutcome.fit.names.global <- "speedOutcome.control.brm.fit.global"

for (v in glo_v) {
  brm.speedOutcome.fit.names.global <- c(brm.speedOutcome.fit.names.global, paste("speedOutcome.", v, ".brm.fit.global", sep = ""))
}

#Fitting Control Variable Only Model
speedOutcome.control.brm.fit.global <- brm(speedOutcome ~ mfr2013lnstd + firmsize.adj.cj + (1 | countryx), data = globalData)

brm.speedOutcome.model.global <- speedOutcome ~ mfr2013lnstd + firmsize.adj.cj + agilitycj * cul + (1 + agilitycj | countryx)
#brm.speedOutcome.model.global <- speedOutcome ~ mfr2013lnstd + firmsize.adj.cj * agilitycj * cul + (1 + firmsize.adj.cj + agilitycj | countryx)


#model estimations 
for (i in 1:9) {
  globalData$cul <- as.numeric(unlist(globalData[, c(glo_vc[i])]))
  assign(brm.speedOutcome.fit.names.global[i+1], brm(brm.speedOutcome.model.global, data = globalData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(20)
}


#Model Output using mcmcReg
mcmcReg(list(speedOutcome.control.brm.fit.global,
             speedOutcome.guaiv.brm.fit.global,
             speedOutcome.gfuov.brm.fit.global,
             speedOutcome.gpdiv.brm.fit.global,
             speedOutcome.ginscolv.brm.fit.global,
             speedOutcome.ghumv.brm.fit.global,
             speedOutcome.gperv.brm.fit.global,
             speedOutcome.gigrcolv.brm.fit.global,
             speedOutcome.ggndv.brm.fit.global,
             speedOutcome.gassv.brm.fit.global),
        pars = c("b", "sd", "sigma"),
        ci = 0.95, format = "latex", caption = "speedOutcome Models (Global) - Cross Interaction",
        file = paste(filepath, "LatexTables/", "04122021-speedOutcomeModelGlobal", sep=""), 
        custom.model.names = c("Control", "UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)




#5/6. speedOutcome2 Models - Regional 
rm(brm.speedOutcome2.fit.names.regional)
brm.speedOutcome2.fit.names.regional <- "speedOutcome2.control.brm.fit.regional"

for (v in glo_v) {
  brm.speedOutcome2.fit.names.regional <- c(brm.speedOutcome2.fit.names.regional, paste("speedOutcome2.", v, ".brm.fit.regional", sep = ""))
}

#Fitting Control Variable Only Model
speedOutcome2.control.brm.fit.regional <- brm(speedOutcome2 ~ mfr2013lnstd + firmsize.adj.cj + (1 | countryx), data = regionalData)

brm.speedOutcome2.model.regional <- speedOutcome2 ~ mfr2013lnstd + firmsize.adj.cj + agility2cj * cul + (1 + agility2cj | countryx)
#brm.speedOutcome2.model.regional <- speedOutcome2 ~ mfr2013lnstd + firmsize.adj.cj * agility2cj * cul + (1 + firmsize.adj.cj + agility2cj | countryx)

#model estimations 
for (i in 1:9) {
  regionalData$cul <- as.numeric(unlist(regionalData[, c(glo_vc[i])]))
  assign(brm.speedOutcome2.fit.names.regional[i+1], brm(brm.speedOutcome2.model.regional, data = regionalData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(20)
}


#Model Output using mcmcReg
mcmcReg(list(speedOutcome2.control.brm.fit.regional,
             speedOutcome2.guaiv.brm.fit.regional,
             speedOutcome2.gfuov.brm.fit.regional,
             speedOutcome2.gpdiv.brm.fit.regional,
             speedOutcome2.ginscolv.brm.fit.regional,
             speedOutcome2.ghumv.brm.fit.regional,
             speedOutcome2.gperv.brm.fit.regional,
             speedOutcome2.gigrcolv.brm.fit.regional,
             speedOutcome2.ggndv.brm.fit.regional,
             speedOutcome2.gassv.brm.fit.regional),
        pars = c("b", "sd", "sigma"),
        ci = 0.95, format = "latex", caption = "speedOutcome2 Models (Regional) - Cross Interaction",
        file = paste(filepath, "LatexTables/", "04122021-speedOutcome2ModelRegional", sep=""), 
        custom.model.names = c("Control", "UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)


#6/6. speedOutcome2 Models - Global 
rm(brm.speedOutcome2.fit.names.global)
brm.speedOutcome2.fit.names.global <- "speedOutcome2.control.brm.fit.global"

for (v in glo_v) {
  brm.speedOutcome2.fit.names.global <- c(brm.speedOutcome2.fit.names.global, paste("speedOutcome2.", v, ".brm.fit.global", sep = ""))
}

#Fitting Control Variable Only Model
speedOutcome2.control.brm.fit.global <- brm(speedOutcome2 ~ mfr2013lnstd + firmsize.adj.cj + (1 | countryx), data = globalData)

brm.speedOutcome2.model.global <- speedOutcome2 ~ mfr2013lnstd + firmsize.adj.cj + agility2cj * cul + (1 + agility2cj | countryx)
#brm.speedOutcome2.model.global <- speedOutcome2 ~ mfr2013lnstd + firmsize.adj.cj * agility2cj * cul + (1 + firmsize.adj.cj + agility2cj | countryx)


#model estimations 
for (i in 1:9) {
  globalData$cul <- as.numeric(unlist(globalData[, c(glo_vc[i])]))
  assign(brm.speedOutcome2.fit.names.global[i+1], brm(brm.speedOutcome2.model.global, data = globalData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(20)
}


#Model Output using mcmcReg
mcmcReg(list(speedOutcome2.control.brm.fit.global,
             speedOutcome2.guaiv.brm.fit.global,
             speedOutcome2.gfuov.brm.fit.global,
             speedOutcome2.gpdiv.brm.fit.global,
             speedOutcome2.ginscolv.brm.fit.global,
             speedOutcome2.ghumv.brm.fit.global,
             speedOutcome2.gperv.brm.fit.global,
             speedOutcome2.gigrcolv.brm.fit.global,
             speedOutcome2.ggndv.brm.fit.global,
             speedOutcome2.gassv.brm.fit.global),
        pars = c("b", "sd", "sigma"),
        ci = 0.95, format = "latex", caption = "speedOutcome2 Models (Global) - Cross Interaction",
        file = paste(filepath, "LatexTables/", "04122021-speedOutcome2ModelGlobal", sep=""), 
        custom.model.names = c("Control", "UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)
