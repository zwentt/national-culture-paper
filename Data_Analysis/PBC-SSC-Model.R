
#Null Model
agility2.null.model <- agility2 ~ 1 + (1 | countryx)
outcome2.null.model <- outcome2 ~ 1 + (1 | countryx)


#Agility Model 
agility2Model <- agility2 ~ mfr2013.z + firmsize.adj.cj + strategycj + competitive1cj + 
  network2xcj * (pbc + ssc) + 
  (1 + network2xcj | countryx)

agility2Model.sub <- agility2 ~ mfr2013.z + firmsize.adj.cj + strategycj + competitive1cj + 
  (pbc + ssc) + 
  (1 | countryx)

#Outcome Model 
outcome2Model <- outcome2 ~ mfr2013.z + firmsize.adj.cj + strategycj + 
  agility2cj * (pbc + ssc) * network2xcj + 
  (1 + agility2cj + network2xcj + network2xcj:agility2cj | countryx)

outcome2Model.sub <- outcome2 ~ mfr2013.z + firmsize.adj.cj + strategycj + 
  agility2cj * (pbc + ssc) + 
  (1 + agility2cj | countryx)



#Agility Null Model Fit 
agility2.null.file <- paste("./brmfit/", "agility2.fit.null", sep = "")

agility2.null.fit <- brm(agility2.null.model, data = CultureData, 
                    cores = 8, chains = 4, iter = 3000, refresh = 0, 
                    control = list(adapt_delta = 0.99), 
                    file = agility2.null.file, file_refit = "on_change")




#Agility Model Fit
agility2.file <- paste("./brmfit/", "agility2.fit.pbcssc", sep = "")

agility2.fit <- brm(agility2Model, data = CultureData, 
                   cores = 8, chains = 4, iter = 3000, refresh = 0, 
                   control = list(adapt_delta = 0.99), 
                   file = agility2.file, file_refit = "on_change")

#Agility on Regional Firm Fit
agility2.regional.file <- paste("./brmfit/", "agility2.fit.pbcssc.regional", sep = "")

agility2.regional.fit <- brm(agility2Model.sub, data = regionalData, 
                             cores = 8, chains = 4, iter = 3000, refresh = 0, 
                             control = list(adapt_delta = 0.99), 
                             file = agility2.regional.file, file_refit = "on_change")


#Agility on Global Firm Fit
agility2.global.file <- paste("./brmfit/", "agility2.fit.pbcssc.global", sep = "")

agility2.global.fit <- brm(agility2Model.sub, data = globalData, 
                           cores = 8, chains = 4, iter = 3000, refresh = 0, 
                           control = list(adapt_delta = 0.99), 
                           file = agility2.global.file, file_refit = "on_change")


#Agility Output to HTML or Latex File
mcmcReg(list(agility2.null.fit, agility2.regional.fit, agility2.global.fit, agility2.fit),
        pars = c("b", "sd", "cor", "sigma"),
        ci = 0.95, format = "html", caption = "Agility and Societal Culture",
        file = paste(filepath, "LatexTables/", "pbc-ssc-model-agility", sep=""), 
        custom.model.names = c("Null Model", "Regional Firms", "Global Firms", "Overall Model"), regex = TRUE)







#Outcome Null Model Fit 
outcome2.null.file <- paste("./brmfit/", "outcome2.null.fit", sep = "")

outcome2.null.fit <- brm(outcome2.null.model, data = CultureData, 
                         cores = 8, chains = 4, iter = 3000, refresh = 0, 
                         control = list(adapt_delta = 0.99), 
                         file = outcome2.null.file, file_refit = "on_change")


#Outcome Model Fit
outcome2.file <- paste("./brmfit/", "outcome2.fit.pbcssc", sep = "")

outcome2.fit <- brm(outcome2Model, data = CultureData, 
                    cores = 8, chains = 4, iter = 3000, refresh = 0, 
                    control = list(adapt_delta = 0.99),
                    file = outcome2.file, file_refit = "on_change")


#Outcome Model Regional Fit
outcome2.regional.file <- paste("./brmfit/", "outcome2.regional.fit.pbcssc", sep = "")

outcome2.regional.fit <- brm(outcome2Model.sub, data = regionalData,
                             cores = 8, chains = 4, iter = 3000, refresh = 0, 
                             control = list(adapt_delta = 0.99),
                             file = outcome2.regional.file, file_refit = "on_change")


#Outcome Model Global Fit

outcome2.global.file <- paste("./brmfit/", "outcome2.global.fit.pbcssc", sep = "")

outcome2.global.fit <- brm(outcome2Model.sub, data = globalData,
                           cores = 8, chains = 4, iter = 3000, refresh = 0, 
                           control = list(adapt_delta = 0.99),
                           file = outcome2.global.file, file_refit = "on_change")

#Outcome Output to HTML or Latex File
mcmcReg(list(outcome2.null.fit, outcome2.fit.regional, outcome2.fit.global, outcome2.fit),
        pars = c("b", "sd", "cor", "sigma"),
        ci = 0.95, format = "html", caption = "Agility, Operational Outcome, and Societal Culture",
        file = paste(filepath, "LatexTables/", "pbc-ssc-model-outcome", sep=""), 
        custom.model.names = c("Null Model", "Regional Firms", "Global Firms", "Overall Model"), regex = TRUE)

