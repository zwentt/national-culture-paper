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

#gfuov_c + gpdiv_c + ginscolv_c + ghumv_c + gperv_c + gigrcolv_c + ggndv_c + gassv_c

# 1. Null Model
agility2.null <- agility2 ~ 1 + (1 | countryx)
outcome2.null <- outcome2 ~ 1 + (1 | countryx)

fit.agility2.null <- brm(agility2.null, data = CultureData, refresh = 0, core = 8, file = "./brmfit/fit.agility2.null", file_refit = "on_change")
fit.outcome2.null <- brm(outcome2.null, data = CultureData, refresh = 0, core = 8, file = "./brmfit/fit.outcome2.null", file_refit = "on_change")

mcmcReg(list(fit.agility2.null, 
             fit.outcome2.null),
        pars = c("b", "sd", "sigma"), 
        ci = 0.95, format = "latex", caption = "Null Models (Set 2)",
        file = paste(filepath, "LatexTables/", "nullModels2", sep=""), regex = TRUE,
        custom.model.names = c("Agility Practices2",
                               "Agility Outcome2"))


# 2/6. Agility2 Cross Interaction Model - Competitive1 Variable

rm(brm.fit.names)
brm.fit.names <- "agility2.control.brm.fit"

for (v in glo_v) {
  brm.fit.names <- c(brm.fit.names, paste("agility2.", v, ".brm.fit", sep = ""))
}

#Fitting Control Variable Only Model
agility2.control.brm.fit <- brm(agility2 ~ mfr2013.z + strategycj + firmsize.adj.cj + competitive1cj + (1 | countryx), 
                                data = CultureData, cores = 8,
                                file = paste("./brmfit/", brm.fit.names[1], sep = "") , file_refit = "on_change")

#All other models 
brm.agility2.model <- agility2 ~ mfr2013.z + firmsize.adj.cj  + strategycj + (competitive1cj + network4xcj) *cul + competitive1cj:network4xcj:cul + (1 + network4xcj + competitive1cj | countryx)


#model estimations 
for (i in 1:9) {
  brm.fit.file <- paste("./brmfit/", brm.fit.names[i+1], sep = "")
  CultureData$cul <- as.numeric(unlist(CultureData[, c(glo_vz[i])]))
  assign(brm.fit.names[i+1], brm(brm.agility2.model, data = CultureData, cores = 8, chains = 4, 
                                 iter = 3000, refresh = 0, control = list(adapt_delta = 0.99), 
                                 file = brm.fit.file, file_refit = "on_change" ))
  
  #Give R a break to cool down the CPU
  Sys.sleep(3)
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
        pars = c("b", "sd", "sigma"),
        ci = 0.95, format = "latex", caption = "Agility2 Models (Z) - 4 Level Network",
        file = paste(filepath, "LatexTables/", "agility2FinalZ4", sep=""), 
        custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)

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
        pars = c("b", "sd", "sigma"),
        ci = 0.90, format = "latex", caption = "Agility2 Models (Z) alpha = .10 - 4 Level Network",
        file = paste(filepath, "LatexTables/", "agility2FinalZ410", sep=""), 
        custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)



#Updating using student family

brm.fit.names.t <- paste(brm.fit.names[1], ".t", sep = "")

for (n in 2:10) {
  brm.fit.names.t <- c(brm.fit.names.t, paste(brm.fit.names[n], ".t", sep = ""))
}


for (m in 1:10) {
  print(brm.fit.names[m])
  z.model <- eval(parse(text = brm.fit.names[m]))
  
  fileName <- paste("./brmfit/", brm.fit.names.t[m], sep = "")
  assign(brm.fit.names.t[m], update(z.model, family = "student", cores = 8, chains = 4, 
                                    iter = 3000, refresh = 0, control = list(adapt_delta = 0.99),
                                    file = fileName, file_refit = "on_change"))
  
  Sys.sleep(15)
  
}


#Model Output using mcmcReg
mcmcReg(list(agility2.guaiv.brm.fit.t,
             agility2.gfuov.brm.fit.t,
             agility2.gpdiv.brm.fit.t,
             agility2.ginscolv.brm.fit.t,
             agility2.ghumv.brm.fit.t,
             agility2.gperv.brm.fit.t,
             agility2.gigrcolv.brm.fit.t,
             agility2.ggndv.brm.fit.t,
             agility2.gassv.brm.fit.t),
        pars = c("b", "sd", "sigma"),
        ci = 0.95, format = "latex", caption = "Agility2 Models (T) - 4 Level Network",
        file = paste(filepath, "LatexTables/", "agility2FinalT4", sep=""), 
        custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)

#Model Output using mcmcReg
mcmcReg(list(agility2.guaiv.brm.fit.t,
             agility2.gfuov.brm.fit.t,
             agility2.gpdiv.brm.fit.t,
             agility2.ginscolv.brm.fit.t,
             agility2.ghumv.brm.fit.t,
             agility2.gperv.brm.fit.t,
             agility2.gigrcolv.brm.fit.t,
             agility2.ggndv.brm.fit.t,
             agility2.gassv.brm.fit.t),
        pars = c("b", "sd", "sigma"),
        ci = 0.90, format = "latex", caption = "Agility2 Models (T) alpha = .10 - 4 Level Network",
        file = paste(filepath, "LatexTables/", "agility2FinalT410", sep=""), 
        custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)





{
  
  #1/6. Null Models
  
  
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
  
  
  
  
  # 3/6. Agility2 Cross Interaction Model Regional - Competitive1 Variable
  
  rm(brm.fit.names)
  brm.fit.names <- "agility2.control.brm.fit.regional"
  
  for (v in glo_v) {
    brm.fit.names <- c(brm.fit.names, paste("agility2.", v, ".brm.fit.regional", sep = ""))
  }
  
  #Fitting Control Variable Only Model
  agility2.control.brm.fit.regional <- brm(agility2 ~ mfr2013.z + strategycj + firmsize.adj.cj + competitive1cj + (1 | countryx), data = regionalData)
  
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
          pars = c("b", "sd", "sigma"),
          ci = 0.95, format = "latex", caption = "Agility2 Models for Regional Firms (T) - Cross Interaction",
          file = paste(filepath, "LatexTables/", "agility2CrossModelsCompetitiveRegionalT", sep=""), 
          custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)
  
  
  mcmcReg(list(agility2.guaiv.brm.fit.regional,
               agility2.gfuov.brm.fit.regional,
               agility2.gpdiv.brm.fit.regional,
               agility2.ginscolv.brm.fit.regional,
               agility2.ghumv.brm.fit.regional,
               agility2.gperv.brm.fit.regional,
               agility2.gigrcolv.brm.fit.regional,
               agility2.ggndv.brm.fit.regional,
               agility2.gassv.brm.fit.regional),
          pars = c("b", "sd", "sigma"),
          ci = 0.90, format = "latex", caption = "Agility2 Models for Regional Firms (T) alpha = .10 - Cross Interaction",
          file = paste(filepath, "LatexTables/", "agility2CrossModelsCompetitiveRegionalT10", sep=""), 
          custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)
  
  
  # 4/6. Agility2 Cross Interaction Model Global - Competitive1 Variable
  
  rm(brm.fit.names)
  brm.fit.names <- "agility2.control.brm.fit.global"
  
  for (v in glo_v) {
    brm.fit.names <- c(brm.fit.names, paste("agility2.", v, ".brm.fit.global", sep = ""))
  }
  
  #Fitting Control Variable Only Model
  agility2.control.brm.fit.global <- brm(agility2 ~ mfr2013.z + strategycj + network2xcj + firmsize.adj.cj + competitive1cj + (1 | countryx), data = globalData)
  
  #All other models 
  brm.agility2.model.global <- agility2 ~ mfr2013.z + firmsize.adj.cj  + strategycj + (competitive1cj) *cul + competitive1cj:cul +  (1 + competitive1cj | countryx)
  
  
  #model estimations 
  for (i in 1:9) {
    globalData$cul <- as.numeric(unlist(globalData[, c(glo_vz[i])]))
    assign(brm.fit.names[i+1], brm(brm.agility2.model.global, data = globalData, core = 8, chains = 4, iter = 3000, refresh = 0, family = "student", control = list(adapt_delta = 0.99)))
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
          ci = 0.95, format = "latex", caption = "Agility2 Models for Global Firms (T) - Cross Interaction",
          file = paste(filepath, "LatexTables/", "agility2CrossModelsCompetitiveGlobalT", sep=""), 
          custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)
  
  
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
          ci = 0.95, format = "latex", caption = "Agility2 Models for Global Firms (T) alpha = .10 - Cross Interaction",
          file = paste(filepath, "LatexTables/", "agility2CrossModelsCompetitiveGlobalT10", sep=""), 
          custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)

}
