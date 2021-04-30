#Posterior Predictive Checks
#https://m-clark.github.io/easy-bayes/posterior-predictive-checks.html

models <- c("agility2", "outcome2")
glo_names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS")
modelNames <- c("Agility Model", "Outcome Model")

oneStepPPCheck <- function(modelIndex, cultureIndex) {
  
  modelClass <- models[modelIndex]
  modelName <- modelNames[modelIndex]
  cultureDimension <- glo_v[cultureIndex]
  cultureName <- glo_names[cultureIndex]

  #model <- eval(parse(text = paste(modelClass, ".", cultureDimension, ".brm.fit.t", sep ="")))
  model <- eval(parse(text = paste(modelClass, ".", cultureDimension, ".brm.fit.3way.t", sep ="")))
  theTitle <- paste("Posterior Predictive Check", " (", modelName, " [T] on ", cultureName, ")", sep = "")
  fileNameEnd <- paste(modelClass, "_", cultureDimension,".png", sep="")
  
  pp_check(model, nsamples = 100) + xlim(-4, 4) + 
    labs(title = theTitle, 
         subtitle = ("Note: Density overlay using 100 posterior samples"))
  ggsave(paste("./Plots/pp_check_default_", fileNameEnd, sep=""), width = 5, height = 5)
  
   pp_check(model, type = 'stat', stat = 'mean') + 
    labs(title = theTitle,
         subtitle = "Note: How well did Stan capture the mean?")
  ggsave(paste("./Plots/pp_check_mean_", fileNameEnd, sep=""), width = 5, height = 5)
  
  pp_check(model, type = 'stat', stat = 'sd') + 
    labs(title = theTitle,
         subtitle = "Note: How well did Stan capture the standard deviation?")
  ggsave(paste("./Plots/pp_check_sd_", fileNameEnd, sep=""), width = 5, height = 5)
  
  pp_check(model, type = 'error_scatter_avg') +   
    labs(title = theTitle,
         subtitle = "Note: Predictive Error")
  ggsave(paste("./Plots/pp_check_error_scatter_", fileNameEnd, sep=""), width = 5, height = 5)
  
  pp_check(model, type = 'intervals') +
    labs(title = theTitle,
         subtitle = "Note: Intervals")
  ggsave(paste("./Plots/pp_check_intervals_", fileNameEnd, sep=""), width = 5, height = 5)
  
  pp_check(model, x = 'cul', type = 'error_scatter_avg_vs_x') +
    labs(title = theTitle,
         subtitle = "Note: Predictive errors on Cultural Dimension")
  ggsave(paste("./Plots/pp_check_x_cul_", fileNameEnd, sep=""), width = 5, height = 5)
  
}

for (modelIndex in 2:2) {
  for (cultureIndex in 1:9) {
    print(cultureIndex)
    oneStepPPCheck(modelIndex = modelIndex, cultureIndex = cultureIndex)
  }
}



#Loo

looicMatrix.t <- data.frame(matrix(nrow = 10, ncol = 3))
colnames(looicMatrix.t) <- c("model", "agility2.z.3way", "outcome2.t.3way")

looicMatrix.t[1, 1] <- "control"
looicMatrix.t[1, 2] <- loo(agility2.control.brm.fit.3way, reloo = TRUE)$estimates[3, 1]
looicMatrix.t[1, 3] <- loo(outcome2.control.brm.fit.3way.t, reloo = TRUE)$estimates[3, 1]

for (modelIndex in 1:1) {
  for (cultureIndex in 1:9) {
    modelNameString <- paste(models[modelIndex], ".", glo_v[cultureIndex], ".brm.fit.3way", sep ="")
    print(modelNameString)
    model <- eval(parse(text = modelNameString))
    looOutcome <- loo(model, reloo = TRUE)
    #plot(looOutcome)
    #ggsave(paste("./Plots/pp_check_psis_", modelNameString, ".png", sep = ""), width = 5, height = 5)
    
    looicMatrix.t[cultureIndex + 1, 1] <- glo_v[cultureIndex]
    looicMatrix.t[cultureIndex + 1, 1 + modelIndex] <- looOutcome$estimates[3,1]
    
  }
}

# Do not run this section of code. 
{
  
  
  looicMatrix.z <- data.frame(matrix(nrow = 10, ncol = 3))
  colnames(looicMatrix.z) <- c("model", "agility2.z", "outcome2.z")
  
  looicMatrix.z[1, 1] <- "control"
  looicMatrix.z[1, 2] <- loo(agility2.control.brm.fit, reloo = TRUE)$estimates[3, 1]
  looicMatrix.z[1, 3] <- loo(outcome2.control.brm.fit, reloo = TRUE)$estimates[3, 1]
  
  for (modelIndex in 1:2) {
    for (cultureIndex in 1:9) {
      modelNameString <- paste(models[modelIndex], ".", glo_v[cultureIndex], ".brm.fit", sep ="")
      print(modelNameString)
      model <- eval(parse(text = modelNameString))
      looOutcome <- loo(model, reloo = TRUE)
      #plot(looOutcome)
      #ggsave(paste("./Plots/pp_check_psis_", modelNameString, ".png", sep = ""), width = 5, height = 5)
      
      looicMatrix.z[cultureIndex + 1, 1] <- glo_v[cultureIndex]
      looicMatrix.z[cultureIndex + 1, 1 + modelIndex] <- looOutcome$estimates[3,1]
      
    }
  }
  
  
  looicMatrix <- cbind(looicMatrix.z, looicMatrix.t[, 2:3])
  print(xtable(looicMatrix, type = "latex", caption = "looic Score"), file = "looic_score.tex")
  
  
}


print(xtable(looicMatrix.t, type = "latex", caption = "looic Score - Three-way Models"), file = "looic_score_3way.tex")






#looic generator
R2_Vector <- data.frame(matrix(nrow = 10, ncol = 3))
colnames(R2_Vector) <- c("model", "agility2.z", "outcome2.t")

R2_Vector[1, 1] <- "control"
R2_Vector[1, 2] <- bayes_R2(agility2.control.brm.fit)[1,1]
R2_Vector[1, 3] <- bayes_R2(outcome2.control.brm.fit.3way.t)[1,1]

for (modelIndex in 2:2) {
  for (cultureIndex in 1:9) {
    modelNameString <- paste(models[modelIndex], ".", glo_v[cultureIndex], ".brm.fit.3way.t", sep ="")
    print(modelNameString)
    model <- eval(parse(text = modelNameString))
    Outcome <- bayes_R2(model)
    #plot(looOutcome)
    #ggsave(paste("./Plots/pp_check_psis_", modelNameString, ".png", sep = ""), width = 5, height = 5)
    
    R2_Vector[cultureIndex + 1, 1] <- glo_v[cultureIndex]
    R2_Vector[cultureIndex + 1, 1 + modelIndex] <- Outcome[1,1]
  }
}


#Don't run this section of code
{
  
  R2_Vector.z <- data.frame(matrix(nrow = 10, ncol = 3))
  colnames(R2_Vector.z) <- c("model", "agility2.z", "outcome2.z")
  
  R2_Vector.z[1, 1] <- "control"
  R2_Vector.z[1, 2] <- bayes_R2(agility2.control.brm.fit)[1,1]
  R2_Vector.z[1, 3] <- bayes_R2(outcome2.control.brm.fit)[1,1]
  
  for (modelIndex in 1:2) {
    for (cultureIndex in 1:9) {
      modelNameString <- paste(models[modelIndex], ".", glo_v[cultureIndex], ".brm.fit", sep ="")
      print(modelNameString)
      model <- eval(parse(text = modelNameString))
      Outcome <- bayes_R2(model)
      #plot(looOutcome)
      #ggsave(paste("./Plots/pp_check_psis_", modelNameString, ".png", sep = ""), width = 5, height = 5)
      
      R2_Vector.z[cultureIndex + 1, 1] <- glo_v[cultureIndex]
      R2_Vector.z[cultureIndex + 1, 1 + modelIndex] <- Outcome[1,1]
    }
  }
  
  
  R2_Vector <- cbind(R2_Vector.z, R2_Vector.t[, 2:3])
}


print(xtable(R2_Vector, type = "latex", caption = "Bayes R2 Score - Three-way Interaction"), file = "Bayes_r2_3way.tex")

