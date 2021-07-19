#Agility Model 
rm(brm.fit.names)
brm.fit.names <- "agility2.control.brm.fit.simplified.global"

for (v in glo_v) {
  brm.fit.names <- c(brm.fit.names, paste("agility2.", v, ".brm.fit.simplified.global", sep = ""))
}

#Fitting Control Variable Only Model
agility2.control.brm.fit.simplified.global <- brm(agility2 ~ mfr2013.z + strategycj + firmsize.adj.cj + competitive1cj + (1 | countryx), 
                                                    data = globalData, cores = 8,
                                                    file = paste("./brmfit/", brm.fit.names[1], ".simplified.global" , sep = "") , file_refit = "on_change")

#All other models 
brm.agility2.model <- agility2 ~ mfr2013.z + firmsize.adj.cj  + strategycj + competitive1cj + cul +
  (1 | countryx)


#model estimations 
for (i in 1:9) {
  brm.fit.file <- paste("./brmfit/", brm.fit.names[i+1], ".simplified.global" , sep = "")
  globalData$cul <- as.numeric(unlist(globalData[, c(glo_vz[i])]))
  assign(brm.fit.names[i+1], brm(brm.agility2.model, data = globalData, cores = 8, chains = 4, 
                                 iter = 3000, refresh = 0, control = list(adapt_delta = 0.99), 
                                 file = brm.fit.file, file_refit = "on_change" ))
  
  #Give R a break to cool down the CPU
  Sys.sleep(5)
}

#Model Output using mcmcReg
mcmcReg(list(agility2.guaiv.brm.fit.simplified.global,
             agility2.gfuov.brm.fit.simplified.global,
             agility2.gpdiv.brm.fit.simplified.global,
             agility2.ginscolv.brm.fit.simplified.global,
             agility2.ghumv.brm.fit.simplified.global,
             agility2.gperv.brm.fit.simplified.global,
             agility2.gigrcolv.brm.fit.simplified.global,
             agility2.ggndv.brm.fit.simplified.global,
             agility2.gassv.brm.fit.simplified.global),
        pars = c("b", "sd", "sigma", "cor"),
        ci = 0.95, format = "latex", caption = "Agility2 Models - Simplified with 2 Level Network (Global Firms)",
        file = paste(filepath, "LatexTables/", "agility2-simplified-global", sep=""), 
        custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)

#Model Output using mcmcReg
mcmcReg(list(agility2.guaiv.brm.fit.simplified.global,
             agility2.gfuov.brm.fit.simplified.global,
             agility2.gpdiv.brm.fit.simplified.global,
             agility2.ginscolv.brm.fit.simplified.global,
             agility2.ghumv.brm.fit.simplified.global,
             agility2.gperv.brm.fit.simplified.global,
             agility2.gigrcolv.brm.fit.simplified.global,
             agility2.ggndv.brm.fit.simplified.global,
             agility2.gassv.brm.fit.simplified.global),
        pars = c("b", "sd", "sigma", "cor"),
        ci = 0.90, format = "latex", caption = "Agility2 Models - Simplified with 2 Level Network (Global Firms with alpha = .10)",
        file = paste(filepath, "LatexTables/", "agility2-simplified-global-alpha10", sep=""), 
        custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)



#Outcome Model 
rm(brm.outcome.fit.names)
brm.outcome.fit.names <- "outcome2.control.brm.fit.simplified.global"

for (v in glo_v) {
  brm.outcome.fit.names <- c(brm.outcome.fit.names, paste("outcome2.", v, ".brm.fit.simplified.global", sep = ""))
}

#Fitting Control Variable Only Model
outcome2.control.brm.fit.simplified.global <- brm(outcome2 ~ mfr2013.z + firmsize.adj.cj + strategycj + (1 | countryx), 
                                                    data = globalData, chains = 4, refresh = 0, cores = 8, 
                                                    file = "./brmfit/outcome2.control.brm.fit.simplified.global", file_refit = "on_change")


#Outcome Model
brm.outcome2.model <- outcome2 ~ mfr2013.z + firmsize.adj.cj + strategycj + 
  agility2cj:cul + 
  (1 + agility2cj | countryx)



#model estimations 
for (i in 1:9) {
  brm.fit.file <- paste("./brmfit/", brm.outcome.fit.names[i+1], ".simplified.global", sep = "")
  
  globalData$cul <- as.numeric(unlist(globalData[, c(glo_vz[i])]))
  assign(brm.outcome.fit.names[i+1], brm(brm.outcome2.model, data = globalData, cores = 8, chains = 4, 
                                         iter = 3000, refresh = 0, control = list(adapt_delta = 0.99),
                                         #family = "student",
                                         file = brm.fit.file, file_refit = "on_change"))
  #Give R a break to cool down the CPU
  Sys.sleep(5)
}



#Model Output using mcmcReg - 5% significance
mcmcReg(list(outcome2.guaiv.brm.fit.simplified.global,
             outcome2.gfuov.brm.fit.simplified.global,
             outcome2.gpdiv.brm.fit.simplified.global,
             outcome2.ginscolv.brm.fit.simplified.global,
             outcome2.ghumv.brm.fit.simplified.global,
             outcome2.gperv.brm.fit.simplified.global,
             outcome2.gigrcolv.brm.fit.simplified.global,
             outcome2.ggndv.brm.fit.simplified.global,
             outcome2.gassv.brm.fit.simplified.global),
        pars = c("sd", "sigma", "cor", "b"),
        ci = 0.95, format = "latex", caption = "Outcome2 Models - Simplified with 2 Level Network (Global Firms)",
        file = paste(filepath, "LatexTables/", "outcome2Final-simplified-global", sep=""), 
        custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)

#Model Output using mcmcReg - 10% significance
mcmcReg(list(outcome2.guaiv.brm.fit.simplified.global,
             outcome2.gfuov.brm.fit.simplified.global,
             outcome2.gpdiv.brm.fit.simplified.global,
             outcome2.ginscolv.brm.fit.simplified.global,
             outcome2.ghumv.brm.fit.simplified.global,
             outcome2.gperv.brm.fit.simplified.global,
             outcome2.gigrcolv.brm.fit.simplified.global,
             outcome2.ggndv.brm.fit.simplified.global,
             outcome2.gassv.brm.fit.simplified.global),
        pars = c("b", "sd","cor", "sigma"),
        ci = 0.90, format = "latex", caption = "Outcome2 Models - Simplified with 2 Level Network (Global Firms with alpha = .10)",
        file = paste(filepath, "LatexTables/", "outcome2Final-simplified-global-alpha10", sep=""), 
        custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)

