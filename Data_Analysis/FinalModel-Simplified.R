#Agility Model 

rm(brm.fit.names)
brm.fit.names <- "agility2.control.brm.fit.3way"

for (v in glo_v) {
  brm.fit.names <- c(brm.fit.names, paste("agility2.", v, ".brm.fit.3way", sep = ""))
}

#Fitting Control Variable Only Model
agility2.control.brm.fit.3way <- brm(agility2 ~ mfr2013.z + strategycj + firmsize.adj.cj + competitive1cj + (1 | countryx), 
                                     data = CultureData, cores = 8,
                                     file = paste("./brmfit/", brm.fit.names[1], ".3way" , sep = "") , file_refit = "on_change")

#All other models 
brm.agility2.model <- agility2 ~ mfr2013.z + firmsize.adj.cj  + strategycj + competitive1cj + network2xcj * cul + 
  (1 + network4xcj | countryx)


#model estimations 
for (i in 1:9) {
  brm.fit.file <- paste("./brmfit/", brm.fit.names[i+1], ".simplified" , sep = "")
  CultureData$cul <- as.numeric(unlist(CultureData[, c(glo_vz[i])]))
  assign(brm.fit.names[i+1], brm(brm.agility2.model, data = CultureData, cores = 8, chains = 4, 
                                 iter = 3000, refresh = 0, control = list(adapt_delta = 0.99), 
                                 file = brm.fit.file, file_refit = "on_change" ))
  
  #Give R a break to cool down the CPU
  Sys.sleep(10)
}

#Model Output using mcmcReg
mcmcReg(list(agility2.guaiv.brm.fit.3way,
             agility2.gfuov.brm.fit.3way,
             agility2.gpdiv.brm.fit.3way,
             agility2.ginscolv.brm.fit.3way,
             agility2.ghumv.brm.fit.3way,
             agility2.gperv.brm.fit.3way,
             agility2.gigrcolv.brm.fit.3way,
             agility2.ggndv.brm.fit.3way,
             agility2.gassv.brm.fit.3way),
        pars = c("b", "sd", "sigma", "cor"),
        ci = 0.95, format = "latex", caption = "Agility2 Models - Simplified with 2 Level Network",
        file = paste(filepath, "LatexTables/", "agility2-simplified", sep=""), 
        custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)

#Model Output using mcmcReg
mcmcReg(list(agility2.guaiv.brm.fit.3way,
             agility2.gfuov.brm.fit.3way,
             agility2.gpdiv.brm.fit.3way,
             agility2.ginscolv.brm.fit.3way,
             agility2.ghumv.brm.fit.3way,
             agility2.gperv.brm.fit.3way,
             agility2.gigrcolv.brm.fit.3way,
             agility2.ggndv.brm.fit.3way,
             agility2.gassv.brm.fit.3way),
        pars = c("b", "sd", "sigma", "cor"),
        ci = 0.90, format = "latex", caption = "Agility2 Models - Simplified with 2 Level Network (alpha = .10)",
        file = paste(filepath, "LatexTables/", "agility2-simplified-alpha10", sep=""), 
        custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)





#Outcome Model 
rm(brm.outcome.fit.names)
brm.outcome.fit.names <- "outcome2.control.brm.fit.simplified"

for (v in glo_v) {
  brm.outcome.fit.names <- c(brm.outcome.fit.names, paste("outcome2.", v, ".brm.fit.simplified", sep = ""))
}

#Fitting Control Variable Only Model
outcome2.control.brm.fit.simplified <- brm(outcome2 ~ mfr2013.z + firmsize.adj.cj + strategycj + (1 | countryx), 
                                     data = CultureData, chains = 4, refresh = 0, cores = 8, 
                                     file = "./brmfit/outcome2.control.brm.fit", file_refit = "on_change")


#Outcome Model
brm.outcome2.model <- outcome2 ~ mfr2013.z + firmsize.adj.cj + strategycj + network2xcj +
  agility2cj:cul + agility2cj:network2xcj:cul  +
  (1 + agility2cj + network2xcj:agility2cj | countryx)



#model estimations 
for (i in 1:9) {
  brm.fit.file <- paste("./brmfit/", brm.outcome.fit.names[i+1], ".simplified", sep = "")
  
  CultureData$cul <- as.numeric(unlist(CultureData[, c(glo_vz[i])]))
  assign(brm.outcome.fit.names[i+1], brm(brm.outcome2.model, data = CultureData, cores = 8, chains = 4, 
                                         iter = 3000, refresh = 0, control = list(adapt_delta = 0.99),
                                         #family = "student",
                                         file = brm.fit.file, file_refit = "on_change"))
  #Give R a break to cool down the CPU
  Sys.sleep(5)
}



#Model Output using mcmcReg - 5% significance
mcmcReg(list(outcome2.guaiv.brm.fit.simplified,
             outcome2.gfuov.brm.fit.simplified,
             outcome2.gpdiv.brm.fit.simplified,
             outcome2.ginscolv.brm.fit.simplified,
             outcome2.ghumv.brm.fit.simplified,
             outcome2.gperv.brm.fit.simplified,
             outcome2.gigrcolv.brm.fit.simplified,
             outcome2.ggndv.brm.fit.simplified,
             outcome2.gassv.brm.fit.simplified),
        pars = c("sd", "sigma", "cor", "b"),
        ci = 0.95, format = "latex", caption = "Outcome2 Models - Simplified with 2 Level Network",
        file = paste(filepath, "LatexTables/", "outcome2Final-simplified", sep=""), 
        custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)

#Model Output using mcmcReg - 10% significance
mcmcReg(list(outcome2.guaiv.brm.fit.simplified,
             outcome2.gfuov.brm.fit.simplified,
             outcome2.gpdiv.brm.fit.simplified,
             outcome2.ginscolv.brm.fit.simplified,
             outcome2.ghumv.brm.fit.simplified,
             outcome2.gperv.brm.fit.simplified,
             outcome2.gigrcolv.brm.fit.simplified,
             outcome2.ggndv.brm.fit.simplified,
             outcome2.gassv.brm.fit.simplified),
        pars = c("b", "sd","cor", "sigma"),
        ci = 0.90, format = "latex", caption = "Outcome2 Models - Simplified with 2 Level Network (alpha = .10)",
        file = paste(filepath, "LatexTables/", "outcome2Final-simplified-alpha10", sep=""), 
        custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)






#####################################################




#Model Updating Using student Distribution
brm.outcome.fit.names.t <- paste(brm.outcome.fit.names[1], ".t", sep = "")

for (n in 2:10) {
  brm.outcome.fit.names.t <- c(brm.outcome.fit.names.t, paste(brm.outcome.fit.names[n], ".t", sep = ""))
}


for (m in 1:10) {
  print(brm.outcome.fit.names[m])
  t.model <- eval(parse(text = brm.outcome.fit.names[m]))
  
  fileName <- paste("./brmfit/", brm.outcome.fit.names.t[m], sep = "")
  assign(brm.outcome.fit.names.t[m], update(t.model, cores = 8, chains = 4, 
                                            iter = 3000, refresh = 0, control = list(adapt_delta = 0.99),
                                            family = "student", 
                                            file = fileName, file_refit = "on_change"))
  Sys.sleep(10)
  
}



#Model Output using mcmcReg - 5% significance
mcmcReg(list(outcome2.guaiv.brm.fit.3way.t,
             outcome2.gfuov.brm.fit.3way.t,
             outcome2.gpdiv.brm.fit.3way.t,
             outcome2.ginscolv.brm.fit.3way.t,
             outcome2.ghumv.brm.fit.3way.t,
             outcome2.gperv.brm.fit.3way.t,
             outcome2.gigrcolv.brm.fit.3way.t,
             outcome2.ggndv.brm.fit.3way.t,
             outcome2.gassv.brm.fit.3way.t),
        pars = c("sd", "sigma", "b"),
        ci = 0.95, format = "latex", caption = "Outcome2 Models (T) - Three-way with 2 Level Network",
        file = paste(filepath, "LatexTables/", "outcome2FinalT2-3way", sep=""), 
        custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)

#Model Output using mcmcReg - 10% significance
mcmcReg(list(outcome2.guaiv.brm.fit.3way.t,
             outcome2.gfuov.brm.fit.3way.t,
             outcome2.gpdiv.brm.fit.3way.t,
             outcome2.ginscolv.brm.fit.3way.t,
             outcome2.ghumv.brm.fit.3way.t,
             outcome2.gperv.brm.fit.3way.t,
             outcome2.gigrcolv.brm.fit.3way.t,
             outcome2.ggndv.brm.fit.3way.t,
             outcome2.gassv.brm.fit.3way.t),
        pars = c("b", "sd","sigma"),
        ci = 0.90, format = "latex", caption = "Outcome2 Models (T) alpha = .10 - Three-way with 2 Level Network",
        file = paste(filepath, "LatexTables/", "outcome2FinalT210-3way", sep=""), 
        custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)


conditional_effects(agility2.gperv.brm.fit.3way)


#Post Analysis 

conditional_effects(agility2.gperv.brm.fit.3way, effects = "competitive1cj:cul", conditions = data.frame(network2xcj = c(-0.5, 0.5)))

ggsave(paste("./Plots/", "agility2.gperv.brm.fit.3way", ".png", sep = ""), width = 10, height = 6)

conditional_effects(outcome2.ginscolv.brm.fit.3way.t, effects = "agility2cj:cul", conditions = data.frame(network2xcj = c(-0.5, 0.5)))

ggsave(paste("./Plots/", "outcome2.ginscolv.brm.fit.3way.t", ".png", sep = ""), width = 10, height = 6)


conditional_effects(outcome2.gperv.brm.fit.3way.t, effects = "agility2cj:cul", conditions = data.frame(network2xcj = c(-0.5, 0.5)))

ggsave(paste("./Plots/", "outcome2.gperv.brm.fit.3way.t", ".png", sep = ""), width = 10, height = 6)

conditional_effects(outcome2.ggndv.brm.fit.3way.t, effects = "agility2cj:cul", conditions = data.frame(network2xcj = c(-0.5, 0.5)))

ggsave(paste("./Plots/", "outcome2.ggndv.brm.fit.3way.t", ".png", sep = ""), width = 10, height = 6)





#Outcome3 Model

rm(brm.outcome.fit.names)
brm.outcome.fit.names <- "outcome3.control.brm.fit.3way.t"

for (v in glo_v) {
  brm.outcome.fit.names <- c(brm.outcome.fit.names, paste("outcome3.", v, ".brm.fit.3way.t", sep = ""))
}

#Fitting Control Variable Only Model
outcome3.control.brm.fit.3way.t <- brm(outcome3 ~ mfr2013.z + firmsize.adj.cj + strategycj + (1 | countryx), 
                                       data = CultureData, chains = 4, refresh = 0, cores = 8, 
                                       family = "student",
                                       file = "./brmfit/outcome3.control.brm.fit.t", file_refit = "on_change")

brm.outcome3.model <- outcome3 ~ mfr2013.z + firmsize.adj.cj + strategycj + 
  (agility2cj + network2xcj) * cul  + agility2cj:network2xcj:cul  + 
  (1 + network2xcj + agility2cj + network2xcj:agility2cj | countryx)



#model estimations 
for (i in 1:9) {
  brm.fit.file <- paste("./brmfit/", brm.outcome.fit.names[i+1], ".3way.t", sep = "")
  
  CultureData$cul <- as.numeric(unlist(CultureData[, c(glo_vz[i])]))
  assign(brm.outcome.fit.names[i+1], brm(brm.outcome3.model, data = CultureData, cores = 8, chains = 4, 
                                         iter = 3000, refresh = 0, control = list(adapt_delta = 0.99),
                                         family = "student",
                                         file = brm.fit.file, file_refit = "on_change"))
  #Give R a break to cool down the CPU
  Sys.sleep(10)
}


mcmcReg(list(outcome3.guaiv.brm.fit.3way.t,
             outcome3.gfuov.brm.fit.3way.t,
             outcome3.gpdiv.brm.fit.3way.t,
             outcome3.ginscolv.brm.fit.3way.t,
             outcome3.ghumv.brm.fit.3way.t,
             outcome3.gperv.brm.fit.3way.t,
             outcome3.gigrcolv.brm.fit.3way.t,
             outcome3.ggndv.brm.fit.3way.t,
             outcome3.gassv.brm.fit.3way.t),
        pars = c("b", "sd","sigma"),
        ci = 0.95, format = "latex", caption = "Outcome3 Models (T) alpha = .05 - Three-way with 2 Level Network",
        file = paste(filepath, "LatexTables/", "outcome3FinalT205-3way", sep=""), 
        custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)


mcmcReg(list(outcome3.guaiv.brm.fit.3way.t,
             outcome3.gfuov.brm.fit.3way.t,
             outcome3.gpdiv.brm.fit.3way.t,
             outcome3.ginscolv.brm.fit.3way.t,
             outcome3.ghumv.brm.fit.3way.t,
             outcome3.gperv.brm.fit.3way.t,
             outcome3.gigrcolv.brm.fit.3way.t,
             outcome3.ggndv.brm.fit.3way.t,
             outcome3.gassv.brm.fit.3way.t),
        pars = c("b", "sd","sigma"),
        ci = 0.90, format = "latex", caption = "Outcome3 Models (T) alpha = .10 - Three-way with 2 Level Network",
        file = paste(filepath, "LatexTables/", "outcome3FinalT210-3way", sep=""), 
        custom.model.names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS"), regex = TRUE)