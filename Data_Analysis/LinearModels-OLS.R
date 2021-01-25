options(scipen=999)
library(haven)
library(data.table)
library(sjstats)
library(lmtest)
library(readr)
library(ggplot2)
library(stargazer)
library(plm)

cultureData <- read_dta("https://github.com/zwentt/national-culture-paper/raw/main/Data_Analysis/CultureData.dta")

filepath <- "D:/OneDrive - University of Toledo/Desktop/@ National Culture/national-culture-paper/Data_Analysis/"

#Work directory
setwd(filepath)

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
core.df <- cultureData[, c("agility2", "sensing2", "proactive2", "agility", "sensing", "proactive", 
                                   "flexOutcome2", "speedOutcome2", "outcome2", "flexOutcome", "speedOutcome", "outcome",
                                   "guaip", "gfuop", "gpdip", "ginscolp", "ghump", "gpefp", "gigrcolp", "ggndp", "gassp",
                                   "guaiv", "gfuov", "gpdiv", "ginscolv", "ghumv", "gperv", "gigrcolv", "ggndv", "gassv", 
                                   "pdi", "idv", "lto", "ivr", "mas", "uai",
                                   "network2x", "firmsizecont", "gmci2013", "y2013", "country", "countryx")]





# compute Stata like df-adjustment
G <- length(unique(core.df$countryx))
N <- length(core.df$countryx)


controlVars <- c("firmsizecont", "gmci2013", "y2013", "I(firmsizecont*firmsizecont)", "network2x")

dv <- "agility2"

allModels <- c("agility2 ~ firmsizecont + gmci2013 + I(firmsizecont*firmsizecont)")


for (cultureVar in glo_v) {
  
  allVars <- c(controlVars, cultureVar, paste("I(", cultureVar, "*network2x", ")", sep=""))
  RHSVars <- ""
  
  #concatenating all variables with "+" symbol
  for (var in allVars) {
    RHSVars <- paste(RHSVars, var, sep = " + ")
  }
  
  #eliminate first "+" symbol
  RHSVars <- substr(RHSVars, 4, 500)
  fullModel <- paste(dv, " ~ ", RHSVars, sep = "")
  
  allModels <- c(allModels, fullModel)

}

model.fit.names <- "control.ols.fit"

for (v in glo_v) {
  model.fit.names <- c(model.fit.names, paste(v, ".ols.fit", sep = ""))
}



control.ols.fit <- lm(agility2 ~ firmsizecont + gmci2013 + y2013 + I(firmsizecont*firmsizecont), data = core.df)


for (i in 1:9) {
  
  print(glo_v[i])
  core.df$cul <- core.df[, glo_v[i]]
  transform(core.df, cul = as.numeric(cul))
  
  assign(model.fit.names[i+1], lm(agility2 ~ firmsizecont + gmci2013 + y2013 + I(firmsizecont*firmsizecont) + network2x + cul + I(cul*network2x), data = core.df))
}

columnNames <- c("control", glo_v)

stargazer(control.ols.fit, 
          guaiv.ols.fit,
          gfuov.ols.fit,
          gpdiv.ols.fit, 
          ginscolv.ols.fit,
          ghumv.ols.fit, 
          gperv.ols.fit, 
          gigrcolv.ols.fit, 
          ggndv.ols.fit,
          gassv.ols.fit,
          out = "agility.ols.estimates.html", 
          covariate.labels = c("Firm Size", 
                               "GMCI 2013",
                               "GPD per capita 2013", 
                               "Firm Size Squared",
                               "Manufacturing Network",
                               "Culture",
                               "Culture x Network"),
          title = "OLS Estimates for All Firms",
          column.labels = columnNames,
          keep.stat = c("n", "rsq", "f", "wald"))








guaiv.lm <- lm(agility2 ~ 
                 firmsizecont + I(firmsizecont*firmsizecont) + gmci2013 +
                 guaiv + network2x + guaiv*network2x, 
               data = core.df)




dfa <- (G/(G - 1)) * (N - 1)/guaiv.lm$df.residual
c_vcov <- dfa * vcovHC(guaiv.lm, type = "HC0", cluster = "group", adjust = T)
c_se <- sqrt(diag(c_vcov))



#lm without cluster
stargazer(guaiv.lm, guaiv.lm, se=list(NULL, c_se), column.labels = c("default", "robust"), type= "text")

#lm with cluster
stargazer()











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
