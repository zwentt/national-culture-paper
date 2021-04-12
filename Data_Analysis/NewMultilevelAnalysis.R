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



#Correlation Coefficient Matrix 
stargazer(cor(eachCountry[glo_v]), type = "text", title = "Correlation Coefficients: GLOBE Value Dimensions",
          out = paste(filepath, "LatexTables/", "globe_v_corr.tex", sep=""))

stargazer(cor(eachCountry[glo_p]), type = "text", title = "Correlation Coefficients: GLOBE Practice Dimensions",
          out = paste(filepath, "LatexTables/", "globe_p_corr.tex", sep=""))

stargazer(cor(CultureData[c("agility", "agility2", "agilitycj", "agility2cj", "outcome", "outcome2", "outcomecj", "outcome2cj")], use = "na.or.complete"), 
          type = "text", title = "Correlation Coefficients: Key Variables",
          out = paste(filepath, "LatexTables/", "key_vars.tex", sep=""))


# stargazer(cor(CultureData[c("sensing", "sensing2", "proactive", "proactive2", 
#                             "flexOutcome","flexOutcome2", "speedOutcome", "speedOutcome2")], 
#                             use = "na.or.complete"), 
#           type = "text", title = "Correlation Coefficients: Key Variables",
#           out = paste(filepath, "LatexTables/", "secondary_vars.tex", sep=""))


stargazer(cor(CultureData[c("agility","outcome", "sensing", "proactive", "flexOutcome", "speedOutcome")], use = "na.or.complete"), 
          type = "text", title = "Correlation Coefficients: Key Variables (Set1)",
          out = paste(filepath, "LatexTables/", "key_vars_set1.tex", sep=""))

stargazer(cor(CultureData[c("agility2","outcome2", "sensing2", "proactive2", "flexOutcome2", "speedOutcome2")], use = "na.or.complete"), 
          type = "text", title = "Correlation Coefficients: Key Variables (Set2)",
          out = paste(filepath, "LatexTables/", "key_vars_set2.tex", sep=""))




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


mcmcReg(list(fit.agility.null, fit.sensing.null, fit.proactive.null,
             fit.outcome.null, fit.flexoutcome.null, fit.speedoutcome.null),
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


mcmcReg(list(fit.agility2.null, fit.sensing2.null, fit.proactive2.null,
             fit.outcome2.null, fit.flexoutcome2.null, fit.speedoutcome2.null),
        pars = c("b_Intercept", "sd_countryx__Intercept", "sigma"), 
        ci = 0.95, format = "latex", caption = "Null Models (Set 2)",
        file = paste(filepath, "LatexTables/", "nullModels2", sep=""), 
        custom.model.names = c("Agility Practices2", "Sensing Practices2", "Proactive Practices2",
                               "Agility Outcome2", "Flexible Outcome2", "Speed Outcome2"))


# 2. Agility Cross Interaction Model 

#gfuov_c + gpdiv_c + ginscolv_c + ghumv_c + gperv_c + gigrcolv_c + ggndv_c + gassv_c

rm(brm.fit.names)
brm.fit.names <- "agility.control.brm.fit"

for (v in glo_v) {
  brm.fit.names <- c(brm.fit.names, paste("agility.", v, ".brm.fit", sep = ""))
}

#Fitting Control Variable Only Model
agility.control.brm.fit <- brm(agility2 ~ mfr2013lnstd + network2xcj * firmsize.adj.cj + (1 | countryx), data = CultureData)

#All other models 
brm.agility.model <- agility2 ~ mfr2013lnstd + network2xcj*firmsize.adj.cj*cul + (1 + firmsize.adj.cj + network2xcj | countryx)


#model estimations 
for (i in 1:9) {
  CultureData$cul <- as.numeric(unlist(CultureData[, c(glo_pc[i])]))
  assign(brm.fit.names[i+1], brm(brm.agility.model, data = CultureData, core = 8, chains = 4, iter = 3000, refresh = 0, control = list(adapt_delta = 0.99)))
  #Give R a break to cool down the CPU
  Sys.sleep(15)
}


#Model Output using mcmcReg
mcmcReg(list(agility.control.brm.fit,
             agility.guaiv.brm.fit,
             agility.gfuov.brm.fit,
             agility.gpdiv.brm.fit,
             agility.ginscolv.brm.fit,
             agility.ghumv.brm.fit,
             agility.gperv.brm.fit,
             agility.gigrcolv.brm.fit,
             agility.ggndv.brm.fit,
             agility.gassv.brm.fit),
        pars = c("b", "sd", "sigma"),
        ci = 0.95, format = "latex", caption = "Agility2 Models - Cross Interaction (GLOBE Practice)",
        file = paste(filepath, "LatexTables/", "agility2CrossModelsPractice", sep=""), 
        custom.model.names = c("Control", "UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)


