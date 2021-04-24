#Data Conditioning
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

CultureData <- read_dta("/Users/zwen/Documents/GitHub/national-culture-paper/Data_Analysis/cultureDataFinal.dta")
#CultureData <- read_dta("D:/Documents/GitHub/national-culture-paper/Data_Analysis/cultureDataFinal.dta")

filepath <- "/Users/zwen/Documents/GitHub/national-culture-paper/Data_Analysis/"

#Work directory
setwd(filepath)

#Variables 
hof = c("pdi", "idv", "lto", "ivr", "mas", "uai")
glo_v = c("guaiv", "gfuov", "gpdiv", "ginscolv", "ghumv", "gperv", "gigrcolv", "ggndv", "gassv")
glo_p = c("guaip", "gfuop", "gpdip", "ginscolp", "ghump", "gpefp", "gigrcolp", "ggndp", "gassp")
glo_pc = c("guaip_c", "gfuop_c", "gpdip_c", "ginscolp_c", "ghump_c", "gpefp_c", "gigrcolp_c", "ggndp_c", "gassp_c")
glo_vc = c("guaiv_c", "gfuov_c", "gpdiv_c", "ginscolv_c", "ghumv_c", "gperv_c", "gigrcolv_c", "ggndv_c", "gassv_c")
glo_vz = c("guaiv.z", "gfuov.z", "gpdiv.z", "ginscolv.z", "ghumv.z", "gperv.z", "gigrcolv.z", "ggndv.z", "gassv.z")

#Additional data conditioning
#firmsize variable log transform then center within cluster

CultureData$firmsize.adj <- log10(CultureData$firmsizecont)
CultureData$firmsize.adj.cj <- CultureData$firmsize.adj - ave(CultureData$firmsize.adj, CultureData$country)
CultureData$mfr2013lnstd <- (log10(CultureData$mfr2013) - mean(log10(CultureData$mfr2013)))/sd(log10(CultureData$mfr2013))
CultureData$competitive1cj <- CultureData$competitive1 - ave(CultureData$competitive1, CultureData$country, FUN = function(x) mean(x, na.rm=T))
CultureData$competitive2cj <- CultureData$competitive2 - ave(CultureData$competitive2, CultureData$country, FUN = function(x) mean(x, na.rm=T))
CultureData$strategycj <- CultureData$strategy - ave(CultureData$strategy, CultureData$country, FUN = function(x) mean(x, na.rm=T))
CultureData$network4xcj <- CultureData$g1 - ave(CultureData$g1, CultureData$country, FUN = function(x) mean(x, na.rm=T))



eachCountry <- subset(CultureData, pickone == 1)

eachCountry$mfr2013.z <- (eachCountry$mfr2013 - mean(eachCountry$mfr2013))/sd(eachCountry$mfr2013)
eachCountry$guaiv.z <- (eachCountry$guaiv - mean(eachCountry$guaiv))/sd(eachCountry$guaiv)
eachCountry$gfuov.z <- (eachCountry$gfuov - mean(eachCountry$gfuov))/sd(eachCountry$gfuov)
eachCountry$gpdiv.z <- (eachCountry$gpdiv - mean(eachCountry$gpdiv))/sd(eachCountry$gpdiv)
eachCountry$ginscolv.z <- (eachCountry$ginscolv - mean(eachCountry$ginscolv))/sd(eachCountry$ginscolv)
eachCountry$ghumv.z <- (eachCountry$ghumv - mean(eachCountry$ghumv))/sd(eachCountry$ghumv)
eachCountry$gperv.z <- (eachCountry$gperv - mean(eachCountry$gperv))/sd(eachCountry$gperv)
eachCountry$gigrcolv.z <- (eachCountry$gigrcolv - mean(eachCountry$gigrcolv))/sd(eachCountry$gigrcolv)
eachCountry$ggndv.z <- (eachCountry$ggndv - mean(eachCountry$ggndv))/sd(eachCountry$ggndv)
eachCountry$gassv.z <- (eachCountry$gassv - mean(eachCountry$gassv))/sd(eachCountry$gassv)

eachCountry <- eachCountry[c("mfr2013.z", "guaiv.z", "gfuov.z", "gpdiv.z", "ginscolv.z", "ghumv.z", "gperv.z", "gigrcolv.z", "ggndv.z", "gassv.z", "country", "countryx")]

dim(CultureData)
dim(eachCountry)
#merge eachCountry dataframe with original CultureData dataframe 
CultureData <- full_join(eachCountry, CultureData, by="countryx")

regionalData <- subset(CultureData, network2x == 0)
globalData <- subset(CultureData, network2x == 1)


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


