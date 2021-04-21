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




#1/6. Outcome Sub Models 
rm(brm.outcomeSub.fit.names)
brm.outcomeSub.fit.names <- "outcomeSub.control.brm.fit"

for (v in glo_v) {
  brm.outcomeSub.fit.names <- c(brm.outcomeSub.fit.names, paste("outcomeSub.", v, ".brm.fit", sep = ""))
}

#Fitting Control Variable Only Model
outcomeSub.control.brm.fit <- brm(outcome ~ mfr2013lnstd + network2xcj + firmsize.adj.cj + (1 | countryx), data = CultureData)

brm.outcomeSub.model <- outcome ~ mfr2013lnstd + firmsize.adj.cj + network2xcj * (sensingcj + proactivecj) * cul + (1 + network2xcj + sensingcj + proactivecj | countryx)


#model estimations 
for (i in 1:9) {
  CultureData$cul <- as.numeric(unlist(CultureData[, c(glo_vc[i])]))
  assign(brm.outcomeSub.fit.names[i+1], brm(brm.outcomeSub.model, data = CultureData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(20)
}

#Model Output using mcmcReg
mcmcReg(list(outcomeSub.control.brm.fit,
             outcomeSub.guaiv.brm.fit,
             outcomeSub.gfuov.brm.fit,
             outcomeSub.gpdiv.brm.fit,
             outcomeSub.ginscolv.brm.fit,
             outcomeSub.ghumv.brm.fit,
             outcomeSub.gperv.brm.fit,
             outcomeSub.gigrcolv.brm.fit,
             outcomeSub.ggndv.brm.fit,
             outcomeSub.gassv.brm.fit),
        pars = c("b", "sd", "sigma"),
        ci = 0.95, format = "latex", caption = "Outcome Sub Models - Cross Interaction",
        file = paste(filepath, "LatexTables/", "outcomeSubModel", sep=""), 
        custom.model.names = c("Control", "UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)



#2/6. Outcome2 Sub Models 
rm(brm.outcome2Sub.fit.names)
brm.outcome2Sub.fit.names <- "outcome2Sub.control.brm.fit"

for (v in glo_v) {
  brm.outcome2Sub.fit.names <- c(brm.outcome2Sub.fit.names, paste("outcome2Sub.", v, ".brm.fit", sep = ""))
}

#Fitting Control Variable Only Model
outcome2Sub.control.brm.fit <- brm(outcome2 ~ mfr2013lnstd + network2xcj + firmsize.adj.cj + (1 | countryx), data = CultureData)


brm.outcome2Sub.model <- outcome2 ~ mfr2013lnstd + firmsize.adj.cj + network2xcj * (sensing2cj + proactive2cj) * cul + (1 + network2xcj + sensingcj + proactivecj | countryx)


#model estimations 
for (i in 1:9) {
  CultureData$cul <- as.numeric(unlist(CultureData[, c(glo_vc[i])]))
  assign(brm.outcome2Sub.fit.names[i+1], brm(brm.outcome2Sub.model, data = CultureData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(20)
}


#Model Output using mcmcReg
mcmcReg(list(outcome2Sub.control.brm.fit,
             outcome2Sub.guaiv.brm.fit,
             outcome2Sub.gfuov.brm.fit,
             outcome2Sub.gpdiv.brm.fit,
             outcome2Sub.ginscolv.brm.fit,
             outcome2Sub.ghumv.brm.fit,
             outcome2Sub.gperv.brm.fit,
             outcome2Sub.gigrcolv.brm.fit,
             outcome2Sub.ggndv.brm.fit,
             outcome2Sub.gassv.brm.fit),
        pars = c("b", "sd", "sigma"),
        ci = 0.95, format = "latex", caption = "Outcome2 Sub Models - Cross Interaction",
        file = paste(filepath, "LatexTables/", "outcome2SubModel", sep=""), 
        custom.model.names = c("Control", "UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)


#3/6. Outcome Sub Models - Regional 
rm(brm.outcomeSub.fit.names.regional)
brm.outcomeSub.fit.names.regional <- "outcomeSub.control.brm.fit.regional"

for (v in glo_v) {
  brm.outcomeSub.fit.names.regional <- c(brm.outcomeSub.fit.names.regional, paste("outcomeSub.", v, ".brm.fit.regional", sep = ""))
}

#Fitting Control Variable Only Model
outcomeSub.control.brm.fit.regional <- brm(outcome ~ mfr2013lnstd + firmsize.adj.cj + (1 | countryx), data = regionalData)

brm.outcomeSub.model.regional <- outcome ~ mfr2013lnstd + firmsize.adj.cj + (sensingcj + proactivecj) * cul + (1 + sensingcj + proactivecj | countryx)


#model estimations 
for (i in 1:9) {
  regionalData$cul <- as.numeric(unlist(regionalData[, c(glo_vc[i])]))
  assign(brm.outcomeSub.fit.names.regional[i+1], brm(brm.outcomeSub.model.regional, data = regionalData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(20)
}


#Model Output using mcmcReg
mcmcReg(list(outcomeSub.control.brm.fit.regional,
             outcomeSub.guaiv.brm.fit.regional,
             outcomeSub.gfuov.brm.fit.regional,
             outcomeSub.gpdiv.brm.fit.regional,
             outcomeSub.ginscolv.brm.fit.regional,
             outcomeSub.ghumv.brm.fit.regional,
             outcomeSub.gperv.brm.fit.regional,
             outcomeSub.gigrcolv.brm.fit.regional,
             outcomeSub.ggndv.brm.fit.regional,
             outcomeSub.gassv.brm.fit.regional),
        pars = c("b", "sd", "sigma"),
        ci = 0.95, format = "latex", caption = "Outcome Sub Models (Regional) - Cross Interaction",
        file = paste(filepath, "LatexTables/", "outcomeSubModelRegional", sep=""), 
        custom.model.names = c("Control", "UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)


#4/6. Outcome Sub Models - Global 
rm(brm.outcomeSub.fit.names.global)
brm.outcomeSub.fit.names.global <- "outcomeSub.control.brm.fit.global"

for (v in glo_v) {
  brm.outcomeSub.fit.names.global <- c(brm.outcomeSub.fit.names.global, paste("outcomeSub.", v, ".brm.fit.global", sep = ""))
}

#Fitting Control Variable Only Model
outcomeSub.control.brm.fit.global <- brm(outcome ~ mfr2013lnstd + firmsize.adj.cj + (1 | countryx), data = globalData)

brm.outcomeSub.model.global <- outcome ~ mfr2013lnstd + firmsize.adj.cj + (sensingcj + proactivecj) * cul + (1 + sensingcj + proactivecj | countryx)


#model estimations 
for (i in 1:9) {
  globalData$cul <- as.numeric(unlist(globalData[, c(glo_vc[i])]))
  assign(brm.outcomeSub.fit.names.global[i+1], brm(brm.outcomeSub.model.global, data = globalData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(20)
}


#Model Output using mcmcReg
mcmcReg(list(outcomeSub.control.brm.fit.global,
             outcomeSub.guaiv.brm.fit.global,
             outcomeSub.gfuov.brm.fit.global,
             outcomeSub.gpdiv.brm.fit.global,
             outcomeSub.ginscolv.brm.fit.global,
             outcomeSub.ghumv.brm.fit.global,
             outcomeSub.gperv.brm.fit.global,
             outcomeSub.gigrcolv.brm.fit.global,
             outcomeSub.ggndv.brm.fit.global,
             outcomeSub.gassv.brm.fit.global),
        pars = c("b", "sd", "sigma"),
        ci = 0.95, format = "latex", caption = "Outcome Sub Models (Global) - Cross Interaction",
        file = paste(filepath, "LatexTables/", "outcomeSubModelGlobal", sep=""), 
        custom.model.names = c("Control", "UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)



#5/6. Outcome2 Sub Models - Regional 
rm(brm.outcome2Sub.fit.names.regional)
brm.outcome2Sub.fit.names.regional <- "outcome2Sub.control.brm.fit.regional"

for (v in glo_v) {
  brm.outcome2Sub.fit.names.regional <- c(brm.outcome2Sub.fit.names.regional, paste("outcome2Sub.", v, ".brm.fit.regional", sep = ""))
}

#Fitting Control Variable Only Model
outcome2Sub.control.brm.fit.regional <- brm(outcome2 ~ mfr2013lnstd + firmsize.adj.cj + (1 | countryx), data = regionalData)

brm.outcome2Sub.model.regional <- outcome2 ~ mfr2013lnstd + firmsize.adj.cj + (sensing2cj + proactive2cj) * cul + (1 + sensing2cj + proactive2cj | countryx)


#model estimations 
for (i in 1:9) {
  regionalData$cul <- as.numeric(unlist(regionalData[, c(glo_vc[i])]))
  assign(brm.outcome2Sub.fit.names.regional[i+1], brm(brm.outcome2Sub.model.regional, data = regionalData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(20)
}


#Model Output using mcmcReg
mcmcReg(list(outcome2Sub.control.brm.fit.regional,
             outcome2Sub.guaiv.brm.fit.regional,
             outcome2Sub.gfuov.brm.fit.regional,
             outcome2Sub.gpdiv.brm.fit.regional,
             outcome2Sub.ginscolv.brm.fit.regional,
             outcome2Sub.ghumv.brm.fit.regional,
             outcome2Sub.gperv.brm.fit.regional,
             outcome2Sub.gigrcolv.brm.fit.regional,
             outcome2Sub.ggndv.brm.fit.regional,
             outcome2Sub.gassv.brm.fit.regional),
        pars = c("b", "sd", "sigma"),
        ci = 0.95, format = "latex", caption = "Outcome2 Sub Models (Regional) - Cross Interaction",
        file = paste(filepath, "LatexTables/", "outcome2SubModelRegional", sep=""), 
        custom.model.names = c("Control", "UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)



#6/6. Outcome2 Sub Models - Global 
rm(brm.outcome2Sub.fit.names.global)
brm.outcome2Sub.fit.names.global <- "outcome2Sub.control.brm.fit.global"

for (v in glo_v) {
  brm.outcome2Sub.fit.names.global <- c(brm.outcome2Sub.fit.names.global, paste("outcome2Sub.", v, ".brm.fit.global", sep = ""))
}

#Fitting Control Variable Only Model
outcome2Sub.control.brm.fit.global <- brm(outcome2 ~ mfr2013lnstd + firmsize.adj.cj + (1 | countryx), data = globalData)

brm.outcome2Sub.model.global <- outcome2 ~ mfr2013lnstd + firmsize.adj.cj + (sensing2cj + proactive2cj) * cul + (1 + sensing2cj + proactive2cj | countryx)


#model estimations 
for (i in 1:9) {
  globalData$cul <- as.numeric(unlist(globalData[, c(glo_vc[i])]))
  assign(brm.outcome2Sub.fit.names.global[i+1], brm(brm.outcome2Sub.model.global, data = globalData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(20)
}


#Model Output using mcmcReg
mcmcReg(list(outcome2Sub.control.brm.fit.global,
             outcome2Sub.guaiv.brm.fit.global,
             outcome2Sub.gfuov.brm.fit.global,
             outcome2Sub.gpdiv.brm.fit.global,
             outcome2Sub.ginscolv.brm.fit.global,
             outcome2Sub.ghumv.brm.fit.global,
             outcome2Sub.gperv.brm.fit.global,
             outcome2Sub.gigrcolv.brm.fit.global,
             outcome2Sub.ggndv.brm.fit.global,
             outcome2Sub.gassv.brm.fit.global),
        pars = c("b", "sd", "sigma"),
        ci = 0.95, format = "latex", caption = "Outcome2 Sub Models (Global) - Cross Interaction",
        file = paste(filepath, "LatexTables/", "outcome2SubModelGlobal", sep=""), 
        custom.model.names = c("Control", "UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)


