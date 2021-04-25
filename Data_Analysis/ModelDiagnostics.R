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
  model <- eval(parse(text = paste(modelClass, ".", cultureDimension, ".brm.fit.t", sep ="")))
  theTitle <- paste("Posterior Predictive Check", " (", modelName, " [T] on ", cultureName, ")", sep = "")
  fileNameEnd <- paste(modelClass, "_", cultureDimension,".png", sep="")
  
  pp_check(model, nsamples = 100) + xlim(-4, 4) + 
    labs(title = theTitle, 
         subtitle = ("Note: Density overlay using 100 posterior samples"))
  ggsave(paste("./Plots/pp_check_default_", fileNameEnd, sep=""), width = 5, height = 5)
  
   pp_check(model, type = 'stat', stat = 'mean') + xlim(-0.3, 0.3) + 
    labs(title = theTitle,
         subtitle = "Note: How well did Stan capture the mean?")
  ggsave(paste("./Plots/pp_check_mean_", fileNameEnd, sep=""), width = 5, height = 5)
  
  pp_check(model, type = 'stat', stat = 'sd') + xlim(-1.5, 1.5) + 
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

for (modelIndex in 1:2) {
  for (cultureIndex in 1:9) {
    print(cultureIndex)
    oneStepPPCheck(modelIndex = modelIndex, cultureIndex = cultureIndex)
  }
}

#looic generator
R2_Vector <- data.frame(matrix(nrow = 9, ncol = 3))
colnames(R2_Vector) <- c("culture", "agility", "outcome")


for (modelIndex in 1:2) {
  for (cultureIndex in 1:9) {
    modelNameString <- paste(models[modelIndex], ".", glo_v[cultureIndex], ".brm.fit", sep ="")
    print(modelNameString)
    model <- eval(parse(text = modelNameString))
    Outcome <- bayes_R2(model)
    #plot(looOutcome)
    #ggsave(paste("./Plots/pp_check_psis_", modelNameString, ".png", sep = ""), width = 5, height = 5)
    
    R2_Vector[cultureIndex, 1] <- glo_v[cultureIndex]
    R2_Vector[cultureIndex, 1 + modelIndex] <- Outcome[1,1]
  }
}


R2_Vector.t
colnames(R2_Vector.t) <- c("culture", "agility2.t", "outcome2.t")
R2_Vector.z
colnames(R2_Vector.z) <- c("culture", "agility2.z", "outcome2.z")

R2_Vector <- cbind(R2_Vector.z, R2_Vector.t[, 2:3])
write.csv(R2_Vector, "./Data_Analysis/Bayes_R2.csv")
