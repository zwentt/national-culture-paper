options(scipen=10)
library(haven)
library(data.table)
library(sjstats)
library(lmtest)
library(readr)
library(ggplot2)
library(stargazer)
library(plm)


CultureData <- read_dta("/Users/zwen/Documents/GitHub/national-culture-paper/Data_Analysis/cultureDataFinal.dta")
CultureData <- read_dta("D:/Documents/GitHub/national-culture-paper/Data_Analysis/cultureDataFinal.dta")

filepath <- "/Users/zwen/Documents/GitHub/national-culture-paper/Data_Analysis/"

#Work directory
setwd(filepath)


filepath <- "D:/Documents/GitHub/national-culture-paper/Data_Analysis/"

#Variables
culturalMeasures = c("guaip", "gfuop", "gpdip", "ginscolp", "ghump", "gpefp", "gigrcolp", "ggndp", "gassp", 
                     "guaiv", "gfuov", "gpdiv", "ginscolv", "ghumv", "gperv", "gigrcolv", "ggndv", "gassv", 
                     "pdi", "idv", "lto", "ivr", "mas", "uai")

hof = c("pdi", "idv", "lto", "ivr", "mas", "uai")
glo_v = c("guaiv", "gfuov", "gpdiv", "ginscolv", "ghumv", "gperv", "gigrcolv", "ggndv", "gassv")
glo_p = c("guaip", "gfuop", "gpdip", "ginscolp", "ghump", "gpefp", "gigrcolp", "ggndp", "gassp")

agilityMeasures = c("sensing", "proactive", "agility")
agility2Measures = c("sensing2", "proactive2", "agility2")
outcomeMeasures = c("flexOutcome", "speedOutcome", "outcome")
outcome2Measures = c("flexOutcome2", "speedOutcome2", "outcome2")

allMeasures = c("sensing2", "proactive2", "agility2", "flexOutcome", "speedOutcome", "outcome")

#only retain needed data columns 
core.df <- CultureData[, c("agility2", "sensing2", "proactive2", "agility", "sensing", "proactive", 
                                   "flexOutcome2", "speedOutcome2", "outcome2", "flexOutcome", "speedOutcome", "outcome",
                                   "guaip", "gfuop", "gpdip", "ginscolp", "ghump", "gpefp", "gigrcolp", "ggndp", "gassp",
                                   "guaiv", "gfuov", "gpdiv", "ginscolv", "ghumv", "gperv", "gigrcolv", "ggndv", "gassv", 
                                   "pdi", "idv", "lto", "ivr", "mas", "uai",
                                   "network2x", "firmsizecont", "firmsizestd", "gmci2013", "y2013", "country", "countryx", "mfr2013lnstd", "mfr2013")]
#log gdp per capita
core.df$ly2013 <- log10(core.df$y2013)
core.df$mfr2013ln <- log10(core.df$mfr2013)


##Agility Model 

rm(model.fit.names)
model.fit.names <- "agility.control.ols.fit"

for (v in glo_v) {
  model.fit.names <- c(model.fit.names, paste("agility.", v, ".ols.fit", sep = ""))
}

#Control Variable Only Model
agility.control.ols.fit <- lm(agility ~ firmsizestd + I(firmsizestd*firmsizestd) + mfr2013ln, data = core.df)

#model estimations 
for (i in 1:9) {
  core.df$cul <- as.numeric(unlist(core.df[, c(glo_v[i])]))
  assign(model.fit.names[i+1], lm(agility ~ firmsizestd + I(firmsizestd*firmsizestd) + mfr2013ln + (network2x * cul), data = core.df))
  #assign(model.fit.names[i+1], lm(outcome ~ firmsizestd + I(firmsizestd*firmsizestd) + gmci2013 + network2x + agility + cul + I(network2x*cul) + I(agility*cul), data = core.df))
  #assign(model.fit.names[i+1], lm(agility2 ~ firmsizestd + gmci2013 + ly2013 + I(firmsizestd*firmsizestd) + network2x + cul + I(cul*network2x), data = core.df))
}


#find out new covariance structure for clustered standard error

G <- length(unique(core.df$countryx))
N <- length(core.df$countryx)


# dfa <- (G/(G - 1)) * (N - 1)/guaiv.lm$df.residual
# c_vcov <- dfa * vcovHC(guaiv.lm, type = "HC0", cluster = "group", adjust = T)
# c_se <- sqrt(diag(c_vcov))

{
  agility.control.dfa <- (G/(G - 1)) * (length(agility.control.ols.fit$fitted.values) - 1)/agility.control.ols.fit$df.residual
  agility.control.vcov <- agility.control.dfa * vcovHC(agility.control.ols.fit, type = "HC0", cluster = "group", adjust = T)
  agility.control.se <- sqrt(diag(agility.control.vcov))
  
  agility.guaiv.dfa <- (G/(G - 1)) * (length(agility.guaiv.ols.fit$fitted.values) - 1)/agility.guaiv.ols.fit$df.residual
  agility.guaiv.vcov <- agility.guaiv.dfa * vcovHC(agility.guaiv.ols.fit, type = "HC0", cluster = "group", adjust = T)
  agility.guaiv.se <- sqrt(diag(agility.guaiv.vcov))
  
  agility.gfuov.dfa <- (G/(G - 1)) * (length(agility.gfuov.ols.fit$fitted.values) - 1)/agility.gfuov.ols.fit$df.residual
  agility.gfuov.vcov <- agility.gfuov.dfa * vcovHC(agility.gfuov.ols.fit, type = "HC0", cluster = "group", adjust = T)
  agility.gfuov.se <- sqrt(diag(agility.gfuov.vcov))
  
  agility.gpdiv.dfa <- (G/(G - 1)) * (length(agility.gpdiv.ols.fit$fitted.values) - 1)/agility.gpdiv.ols.fit$df.residual
  agility.gpdiv.vcov <- agility.gpdiv.dfa * vcovHC(agility.gpdiv.ols.fit, type = "HC0", cluster = "group", adjust = T)
  agility.gpdiv.se <- sqrt(diag(agility.gpdiv.vcov))
  
  agility.ginscolv.dfa <- (G/(G - 1)) * (length(agility.ginscolv.ols.fit$fitted.values) - 1)/agility.ginscolv.ols.fit$df.residual
  agility.ginscolv.vcov <- agility.ginscolv.dfa * vcovHC(agility.ginscolv.ols.fit, type = "HC0", cluster = "group", adjust = T)
  agility.ginscolv.se <- sqrt(diag(agility.ginscolv.vcov))
  
  agility.ghumv.dfa <- (G/(G - 1)) * (length(agility.ghumv.ols.fit$fitted.values) - 1)/agility.ghumv.ols.fit$df.residual
  agility.ghumv.vcov <- agility.ghumv.dfa * vcovHC(agility.ghumv.ols.fit, type = "HC0", cluster = "group", adjust = T)
  agility.ghumv.se <- sqrt(diag(agility.ghumv.vcov))
  
  agility.gperv.dfa <- (G/(G - 1)) * (length(agility.gperv.ols.fit$fitted.values) - 1)/agility.gperv.ols.fit$df.residual
  agility.gperv.vcov <- agility.gperv.dfa * vcovHC(agility.gperv.ols.fit, type = "HC0", cluster = "group", adjust = T)
  agility.gperv.se <- sqrt(diag(agility.gperv.vcov))
  
  agility.gigrcolv.dfa <- (G/(G - 1)) * (length(agility.gigrcolv.ols.fit$fitted.values) - 1)/agility.gigrcolv.ols.fit$df.residual
  agility.gigrcolv.vcov <- agility.gigrcolv.dfa * vcovHC(agility.gigrcolv.ols.fit, type = "HC0", cluster = "group", adjust = T)
  agility.gigrcolv.se <- sqrt(diag(agility.gigrcolv.vcov))
  
  agility.ggndv.dfa <- (G/(G - 1)) * (length(agility.ggndv.ols.fit$fitted.values) - 1)/agility.ggndv.ols.fit$df.residual
  agility.ggndv.vcov <- agility.ggndv.dfa * vcovHC(agility.ggndv.ols.fit, type = "HC0", cluster = "group", adjust = T)
  agility.ggndv.se <- sqrt(diag(agility.ggndv.vcov))
  
  agility.gassv.dfa <- (G/(G - 1)) * (length(agility.gassv.ols.fit$fitted.values) - 1)/agility.gassv.ols.fit$df.residual
  agility.gassv.vcov <- agility.gassv.dfa * vcovHC(agility.gassv.ols.fit, type = "HC0", cluster = "group", adjust = T)
  agility.gassv.se <- sqrt(diag(agility.gassv.vcov))
}

stargazer(agility.control.ols.fit, 
          agility.guaiv.ols.fit,
          agility.gfuov.ols.fit,
          agility.gpdiv.ols.fit, 
          agility.ginscolv.ols.fit,
          agility.ghumv.ols.fit, 
          agility.gperv.ols.fit, 
          agility.gigrcolv.ols.fit, 
          agility.ggndv.ols.fit,
          agility.gassv.ols.fit,
          #out = "agility2.ols.estimates_all(clustered_se).html",
          se = list(agility.control.se,
                    agility.guaiv.se,
                    agility.gfuov.se,
                    agility.gpdiv.se,
                    agility.ginscolv.se,
                    agility.ghumv.se,
                    agility.gperv.se,
                    agility.gigrcolv.se,
                    agility.ggndv.se,
                    agility.gassv.se),
          covariate.labels = c("Firm Size",
                               "Firm Size Squared",
                               "Mfr Value",
                               "Network",
                               "Culture",
                               "Network * Culture"),
          title = "Level of National Culture and Agility Practices (Agility2): OLS Estimates for All Firms (Clustered Standard Error)",
          column.labels = c("Control",
                            "UAI",
                            "FUO",
                            "PDI",
                            "InsCollec",
                            "HUM",
                            "PERF",
                            "IngCollec",
                            "GEND",
                            "ASS"),
          keep.stat = c("n", "rsq", "f", "wald"), 
          df = FALSE, 
          out = paste(filepath, "LatexTables/", "OLSAgilityModels.tex", sep=""))

stargazer(agility.control.ols.fit, 
          agility.guaiv.ols.fit,
          agility.gfuov.ols.fit,
          agility.gpdiv.ols.fit, 
          agility.ginscolv.ols.fit,
          agility.ghumv.ols.fit, 
          agility.gperv.ols.fit, 
          agility.gigrcolv.ols.fit, 
          agility.ggndv.ols.fit,
          agility.gassv.ols.fit,
          covariate.labels = c("Firm Size",
                               "Firm Size Squared",
                               "Mfr Value",
                               "Network",
                               "Culture",
                               "Network * Culture"),
          title = "Level of National Culture and Agility Practices (Agility): OLS Estimates for All Firms (Default Standard Error)",
          column.labels = c("Control",
                            "UAI",
                            "FUO",
                            "PDI",
                            "InsCollec",
                            "HUM",
                            "PERF",
                            "IngCollec",
                            "GEND",
                            "ASS"),
          keep.stat = c("n", "rsq", "f", "wald"), 
          df = FALSE, 
          out = paste(filepath, "LatexTables/", "OLSAgilityModelsDefaultSE.tex", sep=""))


##Outcome Model

rm(model.fit.names)
model.fit.names <- "outcome.control.ols.fit"

for (v in glo_v) {
  model.fit.names <- c(model.fit.names, paste("outcome.", v, ".ols.fit", sep = ""))
}

#Control Variable Only Model
outcome.control.ols.fit <- lm(outcome ~ firmsizestd + I(firmsizestd*firmsizestd) + gmci2013, data = subset(core.df, network2x == 1))

#model estimations 
for (i in 1:9) {
  core.df$cul <- as.numeric(unlist(core.df[, c(glo_v[i])]))
  #assign(model.fit.names[i+1], lm(outcome ~ firmsizestd + I(firmsizestd*firmsizestd) + gmci2013 + cul * (network2x + agility2), data = subset(core.df, network2x == 0)))
  assign(model.fit.names[i+1], lm(outcome ~ firmsizestd + I(firmsizestd*firmsizestd) + gmci2013 + cul * agility2, data = subset(core.df, network2x == 1)))
}


#find out new covariance structure for clustered standard error

G <- length(unique(core.df$countryx))
N <- length(core.df$countryx)


# dfa <- (G/(G - 1)) * (N - 1)/guaiv.lm$df.residual
# c_vcov <- dfa * vcovHC(guaiv.lm, type = "HC0", cluster = "group", adjust = T)
# c_se <- sqrt(diag(c_vcov))

{
  outcome.control.dfa <- (G/(G - 1)) * (length(outcome.control.ols.fit$fitted.values) - 1)/outcome.control.ols.fit$df.residual
  outcome.control.vcov <- outcome.control.dfa * vcovHC(outcome.control.ols.fit, type = "HC0", cluster = "group", adjust = T)
  outcome.control.se <- sqrt(diag(outcome.control.vcov))
  
  outcome.guaiv.dfa <- (G/(G - 1)) * (length(outcome.guaiv.ols.fit$fitted.values) - 1)/outcome.guaiv.ols.fit$df.residual
  outcome.guaiv.vcov <- outcome.guaiv.dfa * vcovHC(outcome.guaiv.ols.fit, type = "HC0", cluster = "group", adjust = T)
  outcome.guaiv.se <- sqrt(diag(outcome.guaiv.vcov))
  
  outcome.gfuov.dfa <- (G/(G - 1)) * (length(outcome.gfuov.ols.fit$fitted.values) - 1)/outcome.gfuov.ols.fit$df.residual
  outcome.gfuov.vcov <- outcome.gfuov.dfa * vcovHC(outcome.gfuov.ols.fit, type = "HC0", cluster = "group", adjust = T)
  outcome.gfuov.se <- sqrt(diag(outcome.gfuov.vcov))
  
  outcome.gpdiv.dfa <- (G/(G - 1)) * (length(outcome.gpdiv.ols.fit$fitted.values) - 1)/outcome.gpdiv.ols.fit$df.residual
  outcome.gpdiv.vcov <- outcome.gpdiv.dfa * vcovHC(outcome.gpdiv.ols.fit, type = "HC0", cluster = "group", adjust = T)
  outcome.gpdiv.se <- sqrt(diag(outcome.gpdiv.vcov))
  
  outcome.ginscolv.dfa <- (G/(G - 1)) * (length(outcome.ginscolv.ols.fit$fitted.values) - 1)/outcome.ginscolv.ols.fit$df.residual
  outcome.ginscolv.vcov <- outcome.ginscolv.dfa * vcovHC(outcome.ginscolv.ols.fit, type = "HC0", cluster = "group", adjust = T)
  outcome.ginscolv.se <- sqrt(diag(outcome.ginscolv.vcov))
  
  outcome.ghumv.dfa <- (G/(G - 1)) * (length(outcome.ghumv.ols.fit$fitted.values) - 1)/outcome.ghumv.ols.fit$df.residual
  outcome.ghumv.vcov <- outcome.ghumv.dfa * vcovHC(outcome.ghumv.ols.fit, type = "HC0", cluster = "group", adjust = T)
  outcome.ghumv.se <- sqrt(diag(outcome.ghumv.vcov))
  
  outcome.gperv.dfa <- (G/(G - 1)) * (length(outcome.gperv.ols.fit$fitted.values) - 1)/outcome.gperv.ols.fit$df.residual
  outcome.gperv.vcov <- outcome.gperv.dfa * vcovHC(outcome.gperv.ols.fit, type = "HC0", cluster = "group", adjust = T)
  outcome.gperv.se <- sqrt(diag(outcome.gperv.vcov))
  
  outcome.gigrcolv.dfa <- (G/(G - 1)) * (length(outcome.gigrcolv.ols.fit$fitted.values) - 1)/outcome.gigrcolv.ols.fit$df.residual
  outcome.gigrcolv.vcov <- outcome.gigrcolv.dfa * vcovHC(outcome.gigrcolv.ols.fit, type = "HC0", cluster = "group", adjust = T)
  outcome.gigrcolv.se <- sqrt(diag(outcome.gigrcolv.vcov))
  
  outcome.ggndv.dfa <- (G/(G - 1)) * (length(outcome.ggndv.ols.fit$fitted.values) - 1)/outcome.ggndv.ols.fit$df.residual
  outcome.ggndv.vcov <- outcome.ggndv.dfa * vcovHC(outcome.ggndv.ols.fit, type = "HC0", cluster = "group", adjust = T)
  outcome.ggndv.se <- sqrt(diag(outcome.ggndv.vcov))
  
  outcome.gassv.dfa <- (G/(G - 1)) * (length(outcome.gassv.ols.fit$fitted.values) - 1)/outcome.gassv.ols.fit$df.residual
  outcome.gassv.vcov <- outcome.gassv.dfa * vcovHC(outcome.gassv.ols.fit, type = "HC0", cluster = "group", adjust = T)
  outcome.gassv.se <- sqrt(diag(outcome.gassv.vcov))
}


#columnNames <- c("control", glo_v)

stargazer(outcome.control.ols.fit, 
          outcome.guaiv.ols.fit,
          outcome.gfuov.ols.fit,
          outcome.gpdiv.ols.fit, 
          outcome.ginscolv.ols.fit,
          outcome.ghumv.ols.fit, 
          outcome.gperv.ols.fit, 
          outcome.gigrcolv.ols.fit, 
          outcome.ggndv.ols.fit,
          outcome.gassv.ols.fit,
          out = "outcome.ols.estimates_multinational(clustered_se).html",
          se = list(outcome.control.se,
                    outcome.guaiv.se,
                    outcome.gfuov.se,
                    outcome.gpdiv.se,
                    outcome.ginscolv.se,
                    outcome.ghumv.se,
                    outcome.gperv.se,
                    outcome.gigrcolv.se,
                    outcome.ggndv.se,
                    outcome.gassv.se),
          covariate.labels = c("Firm Size",
                               "Firm Size Squared",
                               "GMC Index",
                               "Culture",
                               #"Network",
                               "Agility",
                               #"Network * Culture",
                               "Agility * Culture"),
          title = "Moderating Role of National Culture (Agility2 - Outcome): OLS Estimates for Multinational Firms (Clustered Standard Error)",
          column.labels = c("Control Model",
                            "Uncertainty Avoidance",
                            "Future Orientation",
                            "Power Distance",
                            "Institutional Collectivism",
                            "Humane Orientation",
                            "Performance Orientation",
                            "In-group Collectivism",
                            "Gender Egalitarianism",
                            "Assertiveness"),
          keep.stat = c("n", "rsq", "f", "wald"), 
          df = FALSE)








#lm without cluster
stargazer(guaiv.lm, guaiv.lm, se=list(NULL, c_se), column.labels = c("default", "robust"), type= "text")










dim(core.df)

core.df <- na.omit(core.df)

#functions to calculate the adjusted degree of freedom 






























control.lm.fit <- lm(outcome ~ gmci2013 + firmsizecont + network2x + agility2 + network2x*agility2, data = CultureData)

guaiv.lm.fit <- lm(outcome ~ gmci2013 + firmsizecont + network2x + agility2 + guaiv + agility2*network2x + guaiv*network2x + agility2*network2x*guaiv, 
                   data = CultureData)


gfuov.lm.fit <- lm(outcome ~ gmci2013 + firmsizecont + network2x + agility2 + gfuov + agility2*network2x + gfuov*network2x + agility2*network2x*gfuov, 
                   data = CultureData)


stargazer(control.lm.fit, guaiv.lm.fit, gfuov.lm.fit, type = "text")



library(clubSandwich)
clusterSEData <- read_dta("https://www.kellogg.northwestern.edu/faculty/petersen/htm/papers/se/test_data.dta")

ls(clusterSEData)
dim(clusterSEData)
regularSE.lm <- lm(y ~ x, data = clusterSEData)
summary(regularSE.lm)



clusterSE.lm <- coef_test(regularSE.lm, vcov="CR1", cluster=clusterSEData$firmid, test = "naive-t")
clusterSE.lm


stargazer(clusterSE.lm, type = "text")




#Testing Clustered
library(plm)
library(lmtest)

options(digits = 8)

clusterSEData.p <- pdata.frame(clusterSEData, index = c("firmid", "year"))

head(clusterSEData)

m1 <- lm(y ~ x, data = clusterSEData)
pm1 <- lm(y ~ x, data = clusterSEData.p)



#regular se
summary(m1)
stargazer(coeftest(m1), type = "text")

waldtest(m1)

#regular se and white se
whiteSE <- coeftest(m1, vcov = vcovHC(m1, type = "HC1"))
whiteSE

stargazer(coeftest(m1), whiteSE, type = "text")
waldtest(m1, vcov = vcovHC(m1, type = "HC1"))

#clustered se 
G <- length(unique(clusterSEData.p$firmid))
N <- length(clusterSEData.p$firmid)
dfa <- (G/(G-1)) * (N-1) / pm1$df.residual
dfa2 <- (G/(G-1)) * (N-1) / m1$df.residual

firm_c_vcov <- dfa * vcovHC(pm1, type = "HC0", cluster = "group", adjust = T)
clusterSE <- coeftest(pm1, vcov = firm_c_vcov)

waldtest()

stargazer(coeftest(m1), whiteSE, clusterSE, coeftest(m1, vcov = firm_c_vcov), type = "text")
