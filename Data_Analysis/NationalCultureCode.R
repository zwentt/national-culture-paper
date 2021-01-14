library(haven)
library(lme4)
library(blme)
library(lmerTest)
library(dplyr)
library(data.table)
library(sjstats)
library(lmtest)
library(readr)
library(ggplot2)
library(extrafont)
library(margins)

options(scipen=999)

#Reading data from Windows 10
CultureData <- read_dta("D:/OneDrive - University of Toledo/Desktop/@ National Culture/national-culture-paper/Data_Analysis/CultureData.dta")

#Reading data from Github - use with caution! have not tried this!
#CultureData <- read_dta("https://github.com/zwentt/national-culture-paper/raw/main/Data_Analysis/CultureData.dta")

#Reading data from Macbook
#CultureData <- read_dta("Desktop/OneDrive - University of Toledo/Desktop/@ National Culture/Data_Analysis/CultureData.dta")

#Path to save output files
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


#Define a function to return a star or stars according to their signifcance
sigStar <- function(tval)  {
  
  if (abs(tval) <= abs(qt(.05, df = 30)))  # critical t = 1.697261
    " "
  else if (abs(tval) <= abs(qt(.025, df = 30))) # critical t = 2.042272
    "+"
  else if ((abs(tval) <= abs(qt(.005, df = 30)))) # critical t = 2.749996
    "*"
  else if ((abs(tval) <= abs(qt(.0005, df = 30)))) # critical t = 3.645959
    "**"
  else 
    "***"
}



#The beginning of the multilevel analysis block. 
{
#1. null models
counter <- 1

iterate <- c("overall", "domestic", "multinational")

#empty the container when necessary
rm(nullModel_estimates)
#network = ""
for (dv in allMeasures) {
  
  for (network in iterate) {
    
    nullModel <- eval(paste(dv, " ~ ", " + (1 | countryx)", sep = ""))
    olsModel <- eval(paste(dv, "~ 1"))
    
    if (network == "overall") {
      nullModel.fit <- blmer(nullModel, data = CultureData, REML = FALSE)
      olsModel.fit <- lm(nullModel, data = CultureData)
    }
    
    if (network == "domestic") {
      nullModel.fit <- blmer(nullModel, data = subset(CultureData, network2x == 0), REML = FALSE)
      olsModel.fit <- lm(nullModel, data = CultureData, network2x == 0)
    }
    
    if (network == "multinational") {
      nullModel.fit <- blmer(nullModel, data = subset(CultureData, network2x == 1), REML = FALSE)
      olsModel.fit <- lm(nullModel, data = CultureData, network2x == 1)
    }
    
    print(summary(nullModel.fit))
    
    print("Likelihood ratio test")
    print(lrtest(olsModel.fit, nullModel.fit))
    
    MdlSUM  <- summary(nullModel.fit)
    COEF    <- round(summary(nullModel.fit)$coefficients, 4)
    VAR     <- round(as.data.frame(VarCorr(nullModel.fit))["vcov"], 4)
    MdlINFO <- round(as.data.frame(logLik(nullModel.fit)*-2, 4))
    LRTest  <- round(as.data.frame(lrtest(olsModel.fit, nullModel.fit)), 4)
    
    estimates <- data.frame(
      model_name = paste("null_", network, sep=""),
      dv = dv, 
      
      intercept = COEF["(Intercept)", "Estimate"],
      intercept_se = COEF["(Intercept)", "Std. Error"],
      intercept_t = COEF["(Intercept)", "t value"],
      intercept_sig = sigStar(COEF["(Intercept)", "t value"]),
      
      var_firm = VAR[2, 1],
      var_country = VAR[1,1],
      deviance = as.numeric(logLik(controlVarModel.fit))*-2,

      para_count = attr(logLik(controlVarModel.fit), "df"),
      sigularity = isSingular(interceptModel.fit, tol = .00000000001),
      convWarning = MdlSUM$optinfo$conv$opt,
      
      lr_chisq = LRTest[2, "Chisq"],
      lr_pvalue = LRTest[2, "Pr(>Chisq)"],
      icc_value = VAR[1,1]/(VAR[2, 1] + VAR[1,1]),

      aic = AIC(nullModel.fit),
      bic = BIC(nullModel.fit),      
      group_count = as.numeric(summary(nullModel.fit)$ngrps),
      firm_count = nobs(nullModel.fit)
    )
    
    if (counter == 1)
      nullModel_estimates <- estimates
    else 
      nullModel_estimates <- rbind(nullModel_estimates, estimates)
    
    counter <- counter+1
    
  }
}

nullModels <- t(nullModel_estimates)
colnames(nullModels) <- nullModel_estimates[,1]
write.csv(nullModels, paste(filepath, "estimates.null.csv", sep=""), row.names = TRUE)




#======================== Manufacturing network and Agility Level ========================== 

#1. Control Variable Only Models
counter <- 1

#empty estimates container
rm(controlVar_estimates)
for (dv in agility2Measures) {

  vars <- c("firmsizestd", "gmci2016c", "I(firmsizestd*firmsizestd)", "(1 | countryx)")

  
  vars_incld <- ""
  for (var in vars) {
      vars_incld <- paste(vars_incld, var, sep = " + ")
  }
  vars_incld <- substr(vars_incld, 4, 4000)
  
  controlVarModel <- eval(paste(dv, " ~ ", vars_incld, sep = ""))
  controlVarModel.fit <- blmer(controlVarModel, data = CultureData, REML = FALSE, 
                               control=lmerControl(optimizer="Nelder_Mead", optCtrl=list(maxfun = 1e+05)), verbose = TRUE)
  
  print(summary(controlVarModel.fit))
  
  MdlSUM  <- summary(controlVarModel.fit)
  COEF    <- round(summary(controlVarModel.fit)$coefficients, 4)
  VAR     <- round(as.data.frame(VarCorr(controlVarModel.fit))["vcov"], 4)
  MdlINFO <- round(as.data.frame(logLik(controlVarModel.fit)*-2, 4))
  
  estimates <- data.frame(
                          model_name = paste("ControlVar_", dv, sep =" "), 
                          dv = dv, 
                          intercept = COEF["(Intercept)", "Estimate"],
                          intercept_se = COEF["(Intercept)", "Std. Error"],
                          intercept_t = COEF["(Intercept)", "t value"],
                          intercept_sig = sigStar(COEF["(Intercept)", "t value"]),
                          
                          firmsizestd = COEF["firmsizestd", "Estimate"], 
                          firmsizestd_se = COEF["firmsizestd", "Std. Error"],
                          firmsizestd_t = COEF["firmsizestd", "t value"],
                          firmsizestd_sig = sigStar(COEF["firmsizestd", "t value"]),

                          gmci2016c = COEF["gmci2016c", "Estimate"],
                          gmci2016c_se = COEF["gmci2016c", "Std. Error"],
                          gmci2016c_t = COEF["gmci2016c", "t value"],
                          gmci2016c_sig = sigStar(COEF["gmci2016c", "t value"]),
                          
                          firmsize_sq = COEF["I(firmsizestd * firmsizestd)", "Estimate"],
                          firmsize_sq_se = COEF["I(firmsizestd * firmsizestd)", "Std. Error"],
                          firmsize_sq_t = COEF["I(firmsizestd * firmsizestd)", "t value"],
                          firmsize_sq_sig = sigStar(COEF["I(firmsizestd * firmsizestd)", "t value"]),
                          
                          var_firm = VAR[2, 1],
                          var_country = VAR[1,1],
                          deviance = as.numeric(logLik(controlVarModel.fit))*-2,
                          para_count = attr(logLik(controlVarModel.fit), "df"),
                          sigularity = isSingular(controlVarModel.fit, tol = .00000000001),
                          convWarning = MdlSUM$optinfo$conv$opt,
                          
                          aic = AIC(controlVarModel.fit),
                          bic = BIC(controlVarModel.fit),      
                          group_count = as.numeric(summary(controlVarModel.fit)$ngrps),
                          firm_count = nobs(controlVarModel.fit)
                          )
  
  #Initialize a dataframe to contain all the estimates from the loop
  if (counter == 1)
    controlVar_estimates <- estimates
  else 
    controlVar_estimates <- rbind(controlVar_estimates, estimates)
  
  counter <- counter+1
}
  

t.a.controlVar <- t(controlVar_estimates)
colnames(t.a.controlVar) <- controlVar_estimates[,1]
write.csv(t.a.controlVar, paste(filepath, "estimates.a.controlVar.csv", sep=""), row.names = TRUE)



#2. Random Intercept Model
counter <- 1
dv <- "agility2"
#Empty estimation results when necessary
rm(intercept_estimates)

for (varRI in glo_v) {
    
  #variable setup
  culc <- paste(varRI, "_c", sep="")
  vars <- c("firmsizestd", "network2xcj", "gmci2016c", "I(firmsizestd*firmsizestd)", paste("(", culc, " | country)", sep=""))
    
  vars_incld <- ""
  for (v in vars) {
    vars_incld <- paste(vars_incld, v, sep = " + ")
  }
  vars_incld <- substr(vars_incld, 4, 4000)
    
  RIModel <- eval(paste(dv, " ~ ", vars_incld, sep = ""))
    
  currentModel <- RIModel
    
  currentModel.fit <- blmer(currentModel, data = CultureData, REML = FALSE, 
                       control=lmerControl(optimizer="Nelder_Mead", optCtrl=list(maxfun = 1e+05)), verbose = TRUE)
    
  print(summary(currentModel.fit))
    
  MdlSUM  <- summary(currentModel.fit)
  COEF    <- round(summary(currentModel.fit)$coefficients, 4)
  VAR     <- as.data.frame(VarCorr(currentModel.fit))
  MdlINFO <- round(as.data.frame(logLik(currentModel.fit)*-2, 4))
    
  estimates <- data.frame(
    model_name = paste("RI_", culc, sep =""), 
    dv = dv,
    cul = culc,
    intercept = COEF["(Intercept)", "Estimate"],
    intercept_se = COEF["(Intercept)", "Std. Error"],
    intercept_t = COEF["(Intercept)", "t value"],
    intercept_sig = sigStar(COEF["(Intercept)", "t value"]),
    
    firmsizestd = COEF["firmsizestd", "Estimate"], 
    firmsizestd_se = COEF["firmsizestd", "Std. Error"],
    firmsizestd_t = COEF["firmsizestd", "t value"],
    firmsizestd_sig = sigStar(COEF["firmsizestd", "t value"]),
    
    network2xcj = COEF["network2xcj", "Estimate"],
    network2xcj_se = COEF["network2xcj", "Std. Error"],
    network2xcj_t = COEF["network2xcj", "t value"],
    network2xcj_sig = sigStar(COEF["network2xcj", "t value"]),
    
    gmci2016c = COEF["gmci2016c", "Estimate"],
    gmci2016c_se = COEF["gmci2016c", "Std. Error"],
    gmci2016c_t = COEF["gmci2016c", "t value"],
    gmci2016c_sig = sigStar(COEF["gmci2016c", "t value"]),
    
    firmsize_sq = COEF["I(firmsizestd * firmsizestd)", "Estimate"],
    firmsize_sq_se = COEF["I(firmsizestd * firmsizestd)", "Std. Error"],
    firmsize_sq_t = COEF["I(firmsizestd * firmsizestd)", "t value"],
    firmsize_sq_sig = sigStar(COEF["I(firmsizestd * firmsizestd)", "t value"]),
    
    var_firm = VAR[4, "vcov"],
    var_intercept = VAR[1,"vcov"],
    var_culc = VAR[2,"vcov"],
    cov_int_culc = VAR[3,"vcov"],
    
    deviance = as.numeric(logLik(currentModel.fit))*-2,
    para_count = attr(logLik(currentModel.fit), "df"),
    sigularity = isSingular(currentModel.fit, tol = .00000000001),
    convWarning = MdlSUM$optinfo$conv$opt,
    
    aic = AIC(currentModel.fit),
    bic = BIC(currentModel.fit),      
    group_count = as.numeric(summary(currentModel.fit)$ngrps),
    firm_count = nobs(currentModel.fit)
  )
    
  #Initialize a dataframe to contain all the estimates from the loop
  if (counter == 1)
    intercept_estimates <- estimates
  else 
    intercept_estimates <- rbind(intercept_estimates, estimates)
  
  counter <- counter+1
}

t.a.RI <- t(intercept_estimates)
colnames(t.a.RI) <- intercept_estimates[,1]
write.csv(t.a.RI, paste(filepath, "estimates.a.RI.csv", sep=""), row.names = TRUE)




#3. Random Intercept and Random Slope Model 
counter <- 1
dv <- "agility2"
#Empty the results container file when necessary
rm(slope_estimates)

for (varRIRS in glo_v) {
  
  #empty the model estimates container
  #if (length(slope_estimates[, 1]) >= length(glo_v))
    
  
  #variable setup
  culc <- paste(varRIRS, "_c", sep="")
  vars <- c(culc, "firmsizestd", "network2xcj", "gmci2016c", "I(firmsizestd*firmsizestd)", paste("(", culc, " | country)", sep=""))
  
  vars_incld <- ""
  for (v in vars) {
    vars_incld <- paste(vars_incld, v, sep = " + ")
  }
  vars_incld <- substr(vars_incld, 4, 4000)
  
  RIRSModel <- eval(paste(dv, " ~ ", vars_incld, sep = ""))
  
  currentModel <- RIRSModel
  
  currentModel.fit <- blmer(currentModel, data = CultureData, REML = FALSE, 
                            control=lmerControl(optimizer="Nelder_Mead", optCtrl=list(maxfun = 1e+05)), verbose = TRUE)
  
  print(summary(currentModel.fit))
  
  MdlSUM  <- summary(currentModel.fit)
  COEF    <- round(summary(currentModel.fit)$coefficients, 4)
  VAR     <- as.data.frame(VarCorr(currentModel.fit))
  MdlINFO <- round(as.data.frame(logLik(currentModel.fit)*-2, 4))
  
  estimates <- data.frame(
    model_name = paste("RI_", culc, sep =""), 
    dv = dv,
    cul = culc,
    intercept = COEF["(Intercept)", "Estimate"],
    intercept_se = COEF["(Intercept)", "Std. Error"],
    intercept_t = COEF["(Intercept)", "t value"],
    intercept_sig = sigStar(COEF["(Intercept)", "t value"]),
    
    culc = COEF[culc, "Estimate"],
    culc_se = COEF[culc, "Std. Error"],
    culc_t = COEF[culc, "t value"],
    culc_sig = sigStar(COEF[culc, "t value"]),
    
    firmsizestd = COEF["firmsizestd", "Estimate"], 
    firmsizestd_se = COEF["firmsizestd", "Std. Error"],
    firmsizestd_t = COEF["firmsizestd", "t value"],
    firmsizestd_sig = sigStar(COEF["firmsizestd", "t value"]),
    
    network2xcj = COEF["network2xcj", "Estimate"],
    network2xcj_se = COEF["network2xcj", "Std. Error"],
    network2xcj_t = COEF["network2xcj", "t value"],
    network2xcj_sig = sigStar(COEF["network2xcj", "t value"]),
    
    gmci2016c = COEF["gmci2016c", "Estimate"],
    gmci2016c_se = COEF["gmci2016c", "Std. Error"],
    gmci2016c_t = COEF["gmci2016c", "t value"],
    gmci2016c_sig = sigStar(COEF["gmci2016c", "t value"]),
    
    firmsize_sq = COEF["I(firmsizestd * firmsizestd)", "Estimate"],
    firmsize_sq_se = COEF["I(firmsizestd * firmsizestd)", "Std. Error"],
    firmsize_sq_t = COEF["I(firmsizestd * firmsizestd)", "t value"],
    firmsize_sq_sig = sigStar(COEF["I(firmsizestd * firmsizestd)", "t value"]),
    
    var_firm = VAR[4, "vcov"],
    var_intercept = VAR[1,"vcov"],
    var_culc = VAR[2,"vcov"],
    cov_int_culc = VAR[3,"vcov"],
    
    deviance = as.numeric(logLik(currentModel.fit))*-2,
    para_count = attr(logLik(currentModel.fit), "df"),
    sigularity = isSingular(currentModel.fit, tol = .00000000001),
    convWarning = MdlSUM$optinfo$conv$opt,
    
    aic = AIC(currentModel.fit),
    bic = BIC(currentModel.fit),      
    group_count = as.numeric(summary(currentModel.fit)$ngrps),
    firm_count = nobs(currentModel.fit)
  )
  
  #Initialize a dataframe to contain all the estimates from the loop
  if (counter == 1)
    slope_estimates <- estimates
  else 
    slope_estimates <- rbind(slope_estimates, estimates)
  
  counter <- counter + 1
}

t.a.RIRS <- t(slope_estimates)
colnames(t.a.RIRS) <- slope_estimates[,1]
write.csv(t.a.RIRS, paste(filepath, "estimates.a.RIRS.csv", sep=""), row.names = TRUE)


#4. Cross Level Model 
counter <- 1
dv <- "agility2"
#Empty the results container file when necessary
rm(cross_estimates)

for (varCross in glo_v) {
  
  #empty the model estimates container
  #if (length(slope_estimates[, 1]) >= length(glo_v))
  
  
  #variable setup
  culc <- paste(varCross, "_c", sep="")
  vars <- c(culc, "firmsizestd", "network2xcj", "gmci2016c", "I(firmsizestd*firmsizestd)", paste("(", culc, " * network2xcj)", sep=""), paste("(", culc, " | country)", sep=""))
  
  vars_incld <- ""
  for (v in vars) {
    vars_incld <- paste(vars_incld, v, sep = " + ")
  }
  vars_incld <- substr(vars_incld, 4, 4000)
  
  RIRSModel <- eval(paste(dv, " ~ ", vars_incld, sep = ""))
  
  currentModel <- RIRSModel
  
  print(currentModel)
  
  currentModel.fit <- blmer(currentModel, data = CultureData, REML = FALSE, 
                            control=lmerControl(optimizer="Nelder_Mead", optCtrl=list(maxfun = 1e+05)), verbose = TRUE)
  
  print(summary(currentModel.fit))
  
  MdlSUM  <- summary(currentModel.fit)
  COEF    <- round(summary(currentModel.fit)$coefficients, 4)
  VAR     <- as.data.frame(VarCorr(currentModel.fit))
  MdlINFO <- round(as.data.frame(logLik(currentModel.fit)*-2, 4))
  
  estimates <- data.frame(
    model_name = paste("RIRS_", culc, sep =""), 
    dv = dv,
    cul = culc,
    intercept = COEF["(Intercept)", "Estimate"],
    intercept_se = COEF["(Intercept)", "Std. Error"],
    intercept_t = COEF["(Intercept)", "t value"],
    intercept_sig = sigStar(COEF["(Intercept)", "t value"]),
    
    culc = COEF[culc, "Estimate"],
    culc_se = COEF[culc, "Std. Error"],
    culc_t = COEF[culc, "t value"],
    culc_sig = sigStar(COEF[culc, "t value"]),
    
    firmsizestd = COEF["firmsizestd", "Estimate"], 
    firmsizestd_se = COEF["firmsizestd", "Std. Error"],
    firmsizestd_t = COEF["firmsizestd", "t value"],
    firmsizestd_sig = sigStar(COEF["firmsizestd", "t value"]),
    
    network2xcj = COEF["network2xcj", "Estimate"],
    network2xcj_se = COEF["network2xcj", "Std. Error"],
    network2xcj_t = COEF["network2xcj", "t value"],
    network2xcj_sig = sigStar(COEF["network2xcj", "t value"]),
    
    gmci2016c = COEF["gmci2016c", "Estimate"],
    gmci2016c_se = COEF["gmci2016c", "Std. Error"],
    gmci2016c_t = COEF["gmci2016c", "t value"],
    gmci2016c_sig = sigStar(COEF["gmci2016c", "t value"]),
    
    firmsize_sq = COEF["I(firmsizestd * firmsizestd)", "Estimate"],
    firmsize_sq_se = COEF["I(firmsizestd * firmsizestd)", "Std. Error"],
    firmsize_sq_t = COEF["I(firmsizestd * firmsizestd)", "t value"],
    firmsize_sq_sig = sigStar(COEF["I(firmsizestd * firmsizestd)", "t value"]),
    
    culc_network_int = COEF[7, "Estimate"],
    culc_network_int_se = COEF[7, "Std. Error"],
    culc_network_int_t = COEF[7, "t value"],
    culc_network_int_sig = sigStar(COEF[7, "t value"]),
    
    
    var_firm = VAR[4, "vcov"],
    var_intercept = VAR[1,"vcov"],
    var_culc = VAR[2,"vcov"],
    cov_int_culc = VAR[3,"vcov"],
    
    deviance = as.numeric(logLik(currentModel.fit))*-2,
    para_count = attr(logLik(currentModel.fit), "df"),
    sigularity = isSingular(currentModel.fit, tol = .00000000001),
    convWarning = MdlSUM$optinfo$conv$opt,
    
    aic = AIC(currentModel.fit),
    bic = BIC(currentModel.fit),      
    group_count = as.numeric(summary(currentModel.fit)$ngrps),
    firm_count = nobs(currentModel.fit)
  )
  
  #Initialize a dataframe to contain all the estimates from the loop
  if (counter == 1)
    cross_estimates <- estimates
  else 
    cross_estimates <- rbind(cross_estimates, estimates)
  
  counter <- counter + 1
}

t.a.cross <- t(cross_estimates)
colnames(t.a.cross) <- cross_estimates[,1]
write.csv(t.a.cross, paste(filepath, "estimates.a.cross.csv", sep=""), row.names = TRUE)


#======================== Agility and Culture - Outcome Model ========================== 

#1. outcome Control Variable Only Models
counter <- 1

#Empty container if necessary
rm(o.controlVar_estimates)
for (dv in outcomeMeasures) {
  
  vars <- c("firmsizestd", "gmci2016c", "I(firmsizestd*firmsizestd)", "(1 | countryx)")
  
  
  vars_incld <- ""
  for (var in vars) {
    vars_incld <- paste(vars_incld, var, sep = " + ")
  }
  vars_incld <- substr(vars_incld, 4, 4000)
  
  o.controlVarModel <- eval(paste(dv, " ~ ", vars_incld, sep = ""))
  currentModel <- o.controlVarModel
  
  currentModel.fit <- blmer(currentModel, data = CultureData, REML = FALSE, 
                               control=lmerControl(optimizer="Nelder_Mead", optCtrl=list(maxfun = 1e+05)), verbose = TRUE)
  
  print(summary(currentModel.fit))
  
  MdlSUM  <- summary(currentModel.fit)
  COEF    <- round(summary(currentModel.fit)$coefficients, 4)
  VAR     <- round(as.data.frame(VarCorr(currentModel.fit))["vcov"], 4)
  MdlINFO <- round(as.data.frame(logLik(currentModel.fit)*-2, 4))
  
  estimates <- data.frame(
    model_name = paste("o.ControlVar_", dv, sep =" "), 
    dv = dv, 
    intercept = COEF["(Intercept)", "Estimate"],
    intercept_se = COEF["(Intercept)", "Std. Error"],
    intercept_t = COEF["(Intercept)", "t value"],
    intercept_sig = sigStar(COEF["(Intercept)", "t value"]),
    
    firmsizestd = COEF["firmsizestd", "Estimate"], 
    firmsizestd_se = COEF["firmsizestd", "Std. Error"],
    firmsizestd_t = COEF["firmsizestd", "t value"],
    firmsizestd_sig = sigStar(COEF["firmsizestd", "t value"]),
    
    gmci2016c = COEF["gmci2016c", "Estimate"],
    gmci2016c_se = COEF["gmci2016c", "Std. Error"],
    gmci2016c_t = COEF["gmci2016c", "t value"],
    gmci2016c_sig = sigStar(COEF["gmci2016c", "t value"]),
    
    firmsize_sq = COEF["I(firmsizestd * firmsizestd)", "Estimate"],
    firmsize_sq_se = COEF["I(firmsizestd * firmsizestd)", "Std. Error"],
    firmsize_sq_t = COEF["I(firmsizestd * firmsizestd)", "t value"],
    firmsize_sq_sig = sigStar(COEF["I(firmsizestd * firmsizestd)", "t value"]),
    
    var_firm = VAR[2, 1],
    var_country = VAR[1,1],
    deviance = as.numeric(logLik(currentModel.fit))*-2,
    para_count = attr(logLik(currentModel.fit), "df"),
    sigularity = isSingular(currentModel.fit, tol = .00000000001),
    convWarning = MdlSUM$optinfo$conv$opt,
    
    aic = AIC(currentModel.fit),
    bic = BIC(currentModel.fit),      
    group_count = as.numeric(summary(currentModel.fit)$ngrps),
    firm_count = nobs(currentModel.fit)
  )
  
  #Initialize a dataframe to contain all the estimates from the loop
  if (counter == 1)
    o.controlVar_estimates <- estimates
  else 
    o.controlVar_estimates <- rbind(o.controlVar_estimates, estimates)
  
  counter <- counter+1
}


t.o.controlVar <- t(o.controlVar_estimates)
colnames(t.o.controlVar) <- o.controlVar_estimates[,1]
write.csv(t.o.controlVar, paste(filepath, "estimates.o.controlVar.csv", sep=""), row.names = TRUE)



#2. outcome Random Intercept Model
counter <- 1
dv <- "outcome"
#Empty estimation results when necessary
rm(o.RI_estimates)

for (varRI in glo_v) {
  
  #variable setup
  culc <- paste(varRI, "_c", sep="")
  vars <- c("agility2cj", "firmsizestd", "network2xcj", "I(network2xcj*agility2cj)", "gmci2016c", "I(firmsizestd*firmsizestd)", paste("(", culc, " | country)", sep=""))
  
  vars_incld <- ""
  for (v in vars) {
    vars_incld <- paste(vars_incld, v, sep = " + ")
  }
  vars_incld <- substr(vars_incld, 4, 4000)
  
  o.RIModel <- eval(paste(dv, " ~ ", vars_incld, sep = ""))
  
  currentModel <- o.RIModel
  
  currentModel.fit <- blmer(currentModel, data = CultureData, REML = FALSE, 
                            control=lmerControl(optimizer="Nelder_Mead", optCtrl=list(maxfun = 1e+05)), verbose = TRUE)
  
  print(summary(currentModel.fit))
  
  MdlSUM  <- summary(currentModel.fit)
  COEF    <- round(summary(currentModel.fit)$coefficients, 4)
  VAR     <- as.data.frame(VarCorr(currentModel.fit))
  MdlINFO <- round(as.data.frame(logLik(currentModel.fit)*-2, 4))
  
  estimates <- data.frame(
    model_name = paste("o.RI_", culc, sep =""), 
    dv = dv,
    cul = culc,
    intercept = COEF["(Intercept)", "Estimate"],
    intercept_se = COEF["(Intercept)", "Std. Error"],
    intercept_t = COEF["(Intercept)", "t value"],
    intercept_sig = sigStar(COEF["(Intercept)", "t value"]),
    
    agility2cj = COEF["agility2cj", "Estimate"],
    agility2cj_se = COEF["agility2cj", "Std. Error"],
    agility2cj_t = COEF["agility2cj", "t value"],
    agility2cj_sig = sigStar(COEF["agility2cj", "t value"]),

    network2xcj = COEF["network2xcj", "Estimate"],
    network2xcj_se = COEF["network2xcj", "Std. Error"],
    network2xcj_t = COEF["network2xcj", "t value"],
    network2xcj_sig = sigStar(COEF["network2xcj", "t value"]),
    
    agility_network_int = COEF["I(network2xcj * agility2cj)", "Estimate"],
    agility_network_int_se = COEF["I(network2xcj * agility2cj)", "Std. Error"],
    agility_network_int_t = COEF["I(network2xcj * agility2cj)", "t value"],
    agility_network_int_sig = sigStar(COEF["I(network2xcj * agility2cj)", "t value"]),
    
    firmsizestd = COEF["firmsizestd", "Estimate"], 
    firmsizestd_se = COEF["firmsizestd", "Std. Error"],
    firmsizestd_t = COEF["firmsizestd", "t value"],
    firmsizestd_sig = sigStar(COEF["firmsizestd", "t value"]),
    
    gmci2016c = COEF["gmci2016c", "Estimate"],
    gmci2016c_se = COEF["gmci2016c", "Std. Error"],
    gmci2016c_t = COEF["gmci2016c", "t value"],
    gmci2016c_sig = sigStar(COEF["gmci2016c", "t value"]),
    
    firmsize_sq = COEF["I(firmsizestd * firmsizestd)", "Estimate"],
    firmsize_sq_se = COEF["I(firmsizestd * firmsizestd)", "Std. Error"],
    firmsize_sq_t = COEF["I(firmsizestd * firmsizestd)", "t value"],
    firmsize_sq_sig = sigStar(COEF["I(firmsizestd * firmsizestd)", "t value"]),
    
    var_firm = VAR[4, "vcov"],
    var_intercept = VAR[1,"vcov"],
    var_culc = VAR[2,"vcov"],
    cov_int_culc = VAR[3,"vcov"],
    
    deviance = as.numeric(logLik(currentModel.fit))*-2,
    para_count = attr(logLik(currentModel.fit), "df"),
    sigularity = isSingular(currentModel.fit, tol = .00000000001),
    convWarning = MdlSUM$optinfo$conv$opt,
    
    aic = AIC(currentModel.fit),
    bic = BIC(currentModel.fit),      
    group_count = as.numeric(summary(currentModel.fit)$ngrps),
    firm_count = nobs(currentModel.fit)
  )
  
  #Initialize a dataframe to contain all the estimates from the loop
  if (counter == 1)
    o.RI_estimates <- estimates
  else 
    o.RI_estimates <- rbind(o.RI_estimates, estimates)
  
  counter <- counter+1
}

t.o.RI <- t(o.RI_estimates)
colnames(t.o.RI) <- o.RI_estimates[,1]
write.csv(t.o.RI, paste(filepath, "estimates.o.RI.csv", sep=""), row.names = TRUE)


#3. outcome Random Intercept and Random Slope Model 
counter <- 1
dv <- "outcome"
#Empty the results container file when necessary
rm(o.RIRS_estimates)

for (varRIRS in glo_v) {
  
  #variable setup
  culc <- paste(varRIRS, "_c", sep="")
  vars <- c("agility2cj", culc, "firmsizestd", "network2xcj", "I(network2xcj*agility2cj)", "gmci2016c", "I(firmsizestd*firmsizestd)", paste("(", culc, " | country)", sep=""))
  
  vars_incld <- ""
  for (v in vars) {
    vars_incld <- paste(vars_incld, v, sep = " + ")
  }
  vars_incld <- substr(vars_incld, 4, 4000)
  
  o.RIRSModel <- eval(paste(dv, " ~ ", vars_incld, sep = ""))
  
  currentModel <- o.RIRSModel
  
  currentModel.fit <- blmer(currentModel, data = CultureData, REML = FALSE, 
                            control=lmerControl(optimizer="Nelder_Mead", optCtrl=list(maxfun = 1e+05)), verbose = TRUE)
  
  print(summary(currentModel.fit))
  
  MdlSUM  <- summary(currentModel.fit)
  COEF    <- round(summary(currentModel.fit)$coefficients, 4)
  VAR     <- as.data.frame(VarCorr(currentModel.fit))
  MdlINFO <- round(as.data.frame(logLik(currentModel.fit)*-2, 4))
  
  estimates <- data.frame(
    model_name = paste("RI_", culc, sep =""), 
    dv = dv,
    cul = culc,
    intercept = COEF["(Intercept)", "Estimate"],
    intercept_se = COEF["(Intercept)", "Std. Error"],
    intercept_t = COEF["(Intercept)", "t value"],
    intercept_sig = sigStar(COEF["(Intercept)", "t value"]),
    
    culc = COEF[culc, "Estimate"],
    culc_se = COEF[culc, "Std. Error"],
    culc_t = COEF[culc, "t value"],
    culc_sig = sigStar(COEF[culc, "t value"]),
    
    agility2cj = COEF["agility2cj", "Estimate"],
    agility2cj_se = COEF["agility2cj", "Std. Error"],
    agility2cj_t = COEF["agility2cj", "t value"],
    agility2cj_sig = sigStar(COEF["agility2cj", "t value"]),
    
    network2xcj = COEF["network2xcj", "Estimate"],
    network2xcj_se = COEF["network2xcj", "Std. Error"],
    network2xcj_t = COEF["network2xcj", "t value"],
    network2xcj_sig = sigStar(COEF["network2xcj", "t value"]),
    
    agility_network_int = COEF["I(network2xcj * agility2cj)", "Estimate"],
    agility_network_int_se = COEF["I(network2xcj * agility2cj)", "Std. Error"],
    agility_network_int_t = COEF["I(network2xcj * agility2cj)", "t value"],
    agility_network_int_sig = sigStar(COEF["I(network2xcj * agility2cj)", "t value"]),
    
    firmsizestd = COEF["firmsizestd", "Estimate"], 
    firmsizestd_se = COEF["firmsizestd", "Std. Error"],
    firmsizestd_t = COEF["firmsizestd", "t value"],
    firmsizestd_sig = sigStar(COEF["firmsizestd", "t value"]),
    
    gmci2016c = COEF["gmci2016c", "Estimate"],
    gmci2016c_se = COEF["gmci2016c", "Std. Error"],
    gmci2016c_t = COEF["gmci2016c", "t value"],
    gmci2016c_sig = sigStar(COEF["gmci2016c", "t value"]),
    
    firmsize_sq = COEF["I(firmsizestd * firmsizestd)", "Estimate"],
    firmsize_sq_se = COEF["I(firmsizestd * firmsizestd)", "Std. Error"],
    firmsize_sq_t = COEF["I(firmsizestd * firmsizestd)", "t value"],
    firmsize_sq_sig = sigStar(COEF["I(firmsizestd * firmsizestd)", "t value"]),
    
    var_firm = VAR[4, "vcov"],
    var_intercept = VAR[1,"vcov"],
    var_culc = VAR[2,"vcov"],
    cov_int_culc = VAR[3,"vcov"],
    
    deviance = as.numeric(logLik(currentModel.fit))*-2,
    para_count = attr(logLik(currentModel.fit), "df"),
    sigularity = isSingular(currentModel.fit, tol = .00000000001),
    convWarning = MdlSUM$optinfo$conv$opt,
    
    aic = AIC(currentModel.fit),
    bic = BIC(currentModel.fit),      
    group_count = as.numeric(summary(currentModel.fit)$ngrps),
    firm_count = nobs(currentModel.fit)
  )
  
  #Initialize a dataframe to contain all the estimates from the loop
  if (counter == 1)
    o.RIRS_estimates <- estimates
  else 
    o.RIRS_estimates <- rbind(o.RIRS_estimates, estimates)
  
  counter <- counter + 1
}


#transpose the estimates dataframe
t.o.RIRS <- t(o.RIRS_estimates)
colnames(t.o.RIRS) <- o.RIRS_estimates[,1]
write.csv(t.o.RIRS, paste(filepath, "estimates.o.RIRS.csv", sep=""), row.names = TRUE)



#4. outcome Cross Level Model 
counter <- 1
dv <- "outcome"
#Empty the results container file when necessary
rm(o.cross_estimates)

for (varCross in glo_v) {
  
  #empty the model estimates container
  #if (length(slope_estimates[, 1]) >= length(glo_v))
  
  
  #variable setup
  culc <- paste(varCross, "_c", sep="")
  vars <- c("agility2cj", culc, "firmsizestd", "network2xcj", "I(network2xcj*agility2cj)", "gmci2016c", "I(firmsizestd*firmsizestd)", 
            paste("I(agility2cj*", culc, ")", sep = ""),
            paste("I(network2xcj*", culc, ")", sep = ""),
            paste("I(network2xcj*agility2cj*", culc, ")", sep = ""),
            paste("(", culc, " | country)", sep="")
            )
  
  vars_incld <- ""
  for (v in vars) {
    vars_incld <- paste(vars_incld, v, sep = " + ")
  }
  vars_incld <- substr(vars_incld, 4, 4000)
  
  o.crossModel <- eval(paste(dv, " ~ ", vars_incld, sep = ""))
  
  currentModel <- o.crossModel
  
  currentModel.fit <- blmer(currentModel, data = CultureData, REML = FALSE, 
                            control=lmerControl(optimizer="Nelder_Mead", optCtrl=list(maxfun = 1e+05)), verbose = TRUE)
  
  print(summary(currentModel.fit))
  
  MdlSUM  <- summary(currentModel.fit)
  COEF    <- round(summary(currentModel.fit)$coefficients, 4)
  VAR     <- as.data.frame(VarCorr(currentModel.fit))
  MdlINFO <- round(as.data.frame(logLik(currentModel.fit)*-2, 4))
  
  estimates <- data.frame(
    model_name = paste("RI_", culc, sep =""), 
    dv = dv,
    cul = culc,
    intercept = COEF["(Intercept)", "Estimate"],
    intercept_se = COEF["(Intercept)", "Std. Error"],
    intercept_t = COEF["(Intercept)", "t value"],
    intercept_sig = sigStar(COEF["(Intercept)", "t value"]),
    
    culc = COEF[culc, "Estimate"],
    culc_se = COEF[culc, "Std. Error"],
    culc_t = COEF[culc, "t value"],
    culc_sig = sigStar(COEF[culc, "t value"]),
    
    agility2cj = COEF["agility2cj", "Estimate"],
    agility2cj_se = COEF["agility2cj", "Std. Error"],
    agility2cj_t = COEF["agility2cj", "t value"],
    agility2cj_sig = sigStar(COEF["agility2cj", "t value"]),
    
    network2xcj = COEF["network2xcj", "Estimate"],
    network2xcj_se = COEF["network2xcj", "Std. Error"],
    network2xcj_t = COEF["network2xcj", "t value"],
    network2xcj_sig = sigStar(COEF["network2xcj", "t value"]),
    
    agility_network_int = COEF["I(network2xcj * agility2cj)", "Estimate"],
    agility_network_int_se = COEF["I(network2xcj * agility2cj)", "Std. Error"],
    agility_network_int_t = COEF["I(network2xcj * agility2cj)", "t value"],
    agility_network_int_sig = sigStar(COEF["I(network2xcj * agility2cj)", "t value"]),
    
    firmsizestd = COEF["firmsizestd", "Estimate"], 
    firmsizestd_se = COEF["firmsizestd", "Std. Error"],
    firmsizestd_t = COEF["firmsizestd", "t value"],
    firmsizestd_sig = sigStar(COEF["firmsizestd", "t value"]),
    
    gmci2016c = COEF["gmci2016c", "Estimate"],
    gmci2016c_se = COEF["gmci2016c", "Std. Error"],
    gmci2016c_t = COEF["gmci2016c", "t value"],
    gmci2016c_sig = sigStar(COEF["gmci2016c", "t value"]),
    
    firmsize_sq = COEF["I(firmsizestd * firmsizestd)", "Estimate"],
    firmsize_sq_se = COEF["I(firmsizestd * firmsizestd)", "Std. Error"],
    firmsize_sq_t = COEF["I(firmsizestd * firmsizestd)", "t value"],
    firmsize_sq_sig = sigStar(COEF["I(firmsizestd * firmsizestd)", "t value"]),
    
    agility_culc_int = COEF[9, "Estimate"],
    agility_culc_int_se = COEF[9, "Std. Error"],
    agility_culc_int_t = COEF[9, "t value"],
    agility_culc_int_sig = sigStar(COEF[9, "t value"]),
    
    network_culc_int = COEF[10, "Estimate"],
    network_culc_int_se = COEF[10, "Std. Error"],
    network_culc_int_t = COEF[10, "t value"],
    network_culc_int_sig = sigStar(COEF[10, "t value"]),
    
    triple_int = COEF[11, "Estimate"],
    triple_int_se = COEF[11, "Std. Error"],
    triple_int_t = COEF[11, "t value"],
    triple_int_sig = sigStar(COEF[11, "t value"]),
    
    var_firm = VAR[4, "vcov"],
    var_intercept = VAR[1,"vcov"],
    var_culc = VAR[2,"vcov"],
    cov_int_culc = VAR[3,"vcov"],
    
    deviance = as.numeric(logLik(currentModel.fit))*-2,
    para_count = attr(logLik(currentModel.fit), "df"),
    sigularity = isSingular(currentModel.fit, tol = .00000000001),
    convWarning = MdlSUM$optinfo$conv$opt,
    
    aic = AIC(currentModel.fit),
    bic = BIC(currentModel.fit),
    group_count = as.numeric(summary(currentModel.fit)$ngrps),
    firm_count = nobs(currentModel.fit)
  )
  
  #Initialize a dataframe to contain all the estimates from the loop
  if (counter == 1)
    o.cross_estimates <- estimates
  else 
    o.cross_estimates <- rbind(o.cross_estimates, estimates)
  
  counter <- counter + 1
}

#transpose the estimates dataframe
t.o.cross <- t(o.cross_estimates)
colnames(t.o.cross) <- o.cross_estimates[,1]
write.csv(t.o.cross, paste(filepath, "estimates_o.cross.csv", sep=""), row.names = TRUE)



#5a. outcome Cross Level Model - domestic firms only
counter <- 1
dv <- "outcome"
#Empty the results container file when necessary
rm(o.cross_domestic_estimates)

for (varCross in glo_v) {
  
  #empty the model estimates container
  #if (length(slope_estimates[, 1]) >= length(glo_v))
  
  
  #variable setup
  culc <- paste(varCross, "_c", sep="")
  vars <- c("agility2cj", culc, "firmsizestd", "gmci2016c", "I(firmsizestd*firmsizestd)",             
            paste("I(agility2cj*", culc, ")", sep = ""),
            paste("(", culc, " | country)", sep="")
  )
  
  vars_incld <- ""
  for (v in vars) {
    vars_incld <- paste(vars_incld, v, sep = " + ")
  }
  vars_incld <- substr(vars_incld, 4, 4000)
  
  o.crossModel_domestic <- eval(paste(dv, " ~ ", vars_incld, sep = ""))
  
  currentModel <- o.crossModel_domestic
  
  currentModel.fit <- blmer(currentModel, data = subset(CultureData, network2x == 0), REML = FALSE, 
                            control=lmerControl(optimizer="Nelder_Mead", optCtrl=list(maxfun = 1e+05)), verbose = TRUE)
  
  print(summary(currentModel.fit))
  
  MdlSUM  <- summary(currentModel.fit)
  COEF    <- round(summary(currentModel.fit)$coefficients, 4)
  VAR     <- as.data.frame(VarCorr(currentModel.fit))
  MdlINFO <- round(as.data.frame(logLik(currentModel.fit)*-2, 4))
  
  estimates <- data.frame(
    model_name = paste("RI_", culc, sep =""), 
    dv = dv,
    cul = culc,
    intercept = COEF["(Intercept)", "Estimate"],
    intercept_se = COEF["(Intercept)", "Std. Error"],
    intercept_t = COEF["(Intercept)", "t value"],
    intercept_sig = sigStar(COEF["(Intercept)", "t value"]),
    
    culc = COEF[culc, "Estimate"],
    culc_se = COEF[culc, "Std. Error"],
    culc_t = COEF[culc, "t value"],
    culc_sig = sigStar(COEF[culc, "t value"]),
    
    agility2cj = COEF["agility2cj", "Estimate"],
    agility2cj_se = COEF["agility2cj", "Std. Error"],
    agility2cj_t = COEF["agility2cj", "t value"],
    agility2cj_sig = sigStar(COEF["agility2cj", "t value"]),
    
    firmsizestd = COEF["firmsizestd", "Estimate"], 
    firmsizestd_se = COEF["firmsizestd", "Std. Error"],
    firmsizestd_t = COEF["firmsizestd", "t value"],
    firmsizestd_sig = sigStar(COEF["firmsizestd", "t value"]),
    
    gmci2016c = COEF["gmci2016c", "Estimate"],
    gmci2016c_se = COEF["gmci2016c", "Std. Error"],
    gmci2016c_t = COEF["gmci2016c", "t value"],
    gmci2016c_sig = sigStar(COEF["gmci2016c", "t value"]),
    
    firmsize_sq = COEF["I(firmsizestd * firmsizestd)", "Estimate"],
    firmsize_sq_se = COEF["I(firmsizestd * firmsizestd)", "Std. Error"],
    firmsize_sq_t = COEF["I(firmsizestd * firmsizestd)", "t value"],
    firmsize_sq_sig = sigStar(COEF["I(firmsizestd * firmsizestd)", "t value"]),
    
    agility_culc_int = COEF[7, "Estimate"],
    agility_culc_int_se = COEF[7, "Std. Error"],
    agility_culc_int_t = COEF[7, "t value"],
    agility_culc_int_sig = sigStar(COEF[7, "t value"]),
    
    var_firm = VAR[4, "vcov"],
    var_intercept = VAR[1,"vcov"],
    var_culc = VAR[2,"vcov"],
    cov_int_culc = VAR[3,"vcov"],
    
    deviance = as.numeric(logLik(currentModel.fit))*-2,
    para_count = attr(logLik(currentModel.fit), "df"),
    sigularity = isSingular(currentModel.fit, tol = .00000000001),
    convWarning = MdlSUM$optinfo$conv$opt,
    
    aic = AIC(currentModel.fit),
    bic = BIC(currentModel.fit),
    group_count = as.numeric(summary(currentModel.fit)$ngrps),
    firm_count = nobs(currentModel.fit)
  )
  
  #Initialize a dataframe to contain all the estimates from the loop
  if (counter == 1)
    o.cross_domestic_estimates <- estimates
  else 
    o.cross_domestic_estimates <- rbind(o.cross_domestic_estimates, estimates)
  
  counter <- counter + 1
}

#transpose the estimates dataframe
t.o.cross.domestic <- t(o.cross_domestic_estimates)
colnames(t.o.cross.domestic) <- o.cross_domestic_estimates[,1]
write.csv(t.o.cross.domestic, paste(filepath, "estimates_o.cross.domestic.csv", sep=""), row.names = TRUE)


#5b. outcome Cross Level Model - multinational firms
counter <- 1
dv <- "outcome"
#Empty the results container file when necessary
rm(o.cross_multinational_estimates)

for (varCross in glo_v) {
  
  #empty the model estimates container
  #if (length(slope_estimates[, 1]) >= length(glo_v))
  
  
  #variable setup
  culc <- paste(varCross, "_c", sep="")
  vars <- c("agility2cj", culc, "firmsizestd", "gmci2016c", "I(firmsizestd*firmsizestd)",             
            paste("I(agility2cj*", culc, ")", sep = ""),
            paste("(", culc, " | country)", sep="")
  )
  
  vars_incld <- ""
  for (v in vars) {
    vars_incld <- paste(vars_incld, v, sep = " + ")
  }
  vars_incld <- substr(vars_incld, 4, 4000)
  
  o.crossModel_multinational <- eval(paste(dv, " ~ ", vars_incld, sep = ""))
  
  currentModel <- o.crossModel_multinational
  
  currentModel.fit <- blmer(currentModel, data = subset(CultureData, network2x == 1), REML = FALSE, 
                            control=lmerControl(optimizer="Nelder_Mead", optCtrl=list(maxfun = 1e+05)), verbose = TRUE)
  
  print(summary(currentModel.fit))
  
  MdlSUM  <- summary(currentModel.fit)
  COEF    <- round(summary(currentModel.fit)$coefficients, 4)
  VAR     <- as.data.frame(VarCorr(currentModel.fit))
  MdlINFO <- round(as.data.frame(logLik(currentModel.fit)*-2, 4))
  
  estimates <- data.frame(
    model_name = paste("RI_", culc, sep =""), 
    dv = dv,
    cul = culc,
    intercept = COEF["(Intercept)", "Estimate"],
    intercept_se = COEF["(Intercept)", "Std. Error"],
    intercept_t = COEF["(Intercept)", "t value"],
    intercept_sig = sigStar(COEF["(Intercept)", "t value"]),
    
    culc = COEF[culc, "Estimate"],
    culc_se = COEF[culc, "Std. Error"],
    culc_t = COEF[culc, "t value"],
    culc_sig = sigStar(COEF[culc, "t value"]),
    
    agility2cj = COEF["agility2cj", "Estimate"],
    agility2cj_se = COEF["agility2cj", "Std. Error"],
    agility2cj_t = COEF["agility2cj", "t value"],
    agility2cj_sig = sigStar(COEF["agility2cj", "t value"]),
    
    firmsizestd = COEF["firmsizestd", "Estimate"], 
    firmsizestd_se = COEF["firmsizestd", "Std. Error"],
    firmsizestd_t = COEF["firmsizestd", "t value"],
    firmsizestd_sig = sigStar(COEF["firmsizestd", "t value"]),
    
    gmci2016c = COEF["gmci2016c", "Estimate"],
    gmci2016c_se = COEF["gmci2016c", "Std. Error"],
    gmci2016c_t = COEF["gmci2016c", "t value"],
    gmci2016c_sig = sigStar(COEF["gmci2016c", "t value"]),
    
    firmsize_sq = COEF["I(firmsizestd * firmsizestd)", "Estimate"],
    firmsize_sq_se = COEF["I(firmsizestd * firmsizestd)", "Std. Error"],
    firmsize_sq_t = COEF["I(firmsizestd * firmsizestd)", "t value"],
    firmsize_sq_sig = sigStar(COEF["I(firmsizestd * firmsizestd)", "t value"]),
    
    agility_culc_int = COEF[7, "Estimate"],
    agility_culc_int_se = COEF[7, "Std. Error"],
    agility_culc_int_t = COEF[7, "t value"],
    agility_culc_int_sig = sigStar(COEF[7, "t value"]),
    
    var_firm = VAR[4, "vcov"],
    var_intercept = VAR[1,"vcov"],
    var_culc = VAR[2,"vcov"],
    cov_int_culc = VAR[3,"vcov"],
    
    deviance = as.numeric(logLik(currentModel.fit))*-2,
    para_count = attr(logLik(currentModel.fit), "df"),
    sigularity = isSingular(currentModel.fit, tol = .00000000001),
    convWarning = MdlSUM$optinfo$conv$opt,
    
    aic = AIC(currentModel.fit),
    bic = BIC(currentModel.fit),
    group_count = as.numeric(summary(currentModel.fit)$ngrps),
    firm_count = nobs(currentModel.fit)
  )
  
  #Initialize a dataframe to contain all the estimates from the loop
  if (counter == 1)
    o.cross_multinational_estimates <- estimates
  else 
    o.cross_multinational_estimates <- rbind(o.cross_multinational_estimates, estimates)
  
  counter <- counter + 1
}

#transpose the estimates dataframe
t.o.cross.multinational <- t(o.cross_multinational_estimates)
colnames(t.o.cross.multinational) <- o.cross_multinational_estimates[,1]
write.csv(t.o.cross.multinational, paste(filepath, "estimates_o.cross.multinational.csv", sep=""), row.names = TRUE)


}

#Visualization 1. Varying Level of Agility2

#The beginning of the visualization block. 
{

visual.agility <- ggplot(CultureData, aes(x = country, y = agility2, group = network2, shape = network2, color = network2)) +
  geom_point(alpha = .9, size = 2) +
  scale_shape_manual(values = c(2,6)) +
    scale_color_manual(values = c("red", "blue")) +
    stat_summary(fun = mean, geom = "line", size = 0.75)
#Theme setting
visual.agility <- visual.agility + 
  theme_classic() +
  theme(legend.position = "top", 
        axis.text.x = element_text(angle = 90),
        plot.title=element_text(family="Times New Roman", face="bold", size=12)) +
  labs(title = "Level of Agility by Country and Network", 
       y = "Level of Agility",
       x = "Country")


print(visual.agility)
ggsave("visual.agility.png")


#Visualization 2a. Agility and Outcome 
visual.agilityOutcome <- ggplot(CultureData, aes(x = agility2, y = outcome, group = country, shape = network2, color = network2)) +
  geom_point(alpha = .9, size = 2) +
  scale_shape_manual(values = c(2,6)) +
  geom_smooth(method = lm, se = FALSE, aes(group = network2)) +
  scale_color_manual(values = c("red", "blue")) +
  facet_wrap(~ country, ncol = 3)

#Theme setting
visual.agilityOutcome <- visual.agilityOutcome + 
  theme_classic() +
  theme(legend.position = "top", 
        axis.text.x = element_text(angle = 90),
        plot.title=element_text(face="bold", size=12)) +
  labs(title = "Level of Agility and Operational Outcome", 
       y = "Level of Operational Outcome",
       x = "Level of Agility")

print(visual.agilityOutcome)
ggsave("visual.agilityOutcome.png")


}

#superModel <- outcome ~ firmsizestd + gmci2016c + gassv_c + gfuov_c + ggndv_c + ghumv_c + gigrcolv_c + ginscolv_c + gpdiv_c + gperv_c + guaiv_c + sensing2 + proactive2 + I(firmsizestd*firmsizestd) + gassv_c*sensing2 + gfuov_c*sensing2 + ggndv_c*sensing2 + ghumv_c*sensing2 + gigrcolv_c*sensing2 + ginscolv_c*sensing2 + gpdiv_c*sensing2 + gperv_c*sensing2 + guaiv_c*sensing2 + gassv_c*proactive2 + gfuov_c*proactive2 + ggndv_c*proactive2 + ghumv_c*proactive2 + gigrcolv_c*proactive2 + ginscolv_c*proactive2 + gpdiv_c*proactive2 + gperv_c*proactive2 + guaiv_c*proactive2 + (gassv_c + gfuov_c + ggndv_c + ghumv_c + gigrcolv_c + ginscolv_c + gpdiv_c + gperv_c + guaiv_c | countryx)

#superModel2 <- outcome ~ firmsizestd + gmci2016c + gassv_c + gfuov_c + ggndv_c + ghumv_c + gigrcolv_c + ginscolv_c + gpdiv_c + gperv_c + guaiv_c + agility2 + gassv_c*agility2 + gfuov_c*agility2 + ggndv_c*agility2 + ghumv_c*agility2 + gigrcolv_c*agility2 + ginscolv_c*agility2 + gpdiv_c*agility2 + gperv_c*agility2 + guaiv_c*agility2 + I(firmsizestd*firmsizestd) +  (gassv_c + gfuov_c + ggndv_c + ghumv_c + gigrcolv_c + ginscolv_c + gpdiv_c + gperv_c + guaiv_c | countryx)



