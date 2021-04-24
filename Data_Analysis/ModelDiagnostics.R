#Posterior Predictive Checks
#https://m-clark.github.io/easy-bayes/posterior-predictive-checks.html

plot(loo(agility2.guaiv.brm.fit))

models <- c("agility2", "outcome2")
glo_names = c("UAI", "FUO", "PDI", "InsCol", "HUM", "PER", "IgrCol", "GND", "ASS")
modelNames <- c("Agility Model", "Outcome Model")

oneStepPPCheck <- function(modelIndex, cultureIndex) {
  
  modelClass <- models[modelIndex]
  modelName <- modelNames[modelIndex]
  cultureDimension <- glo_v[cultureIndex]
  cultureName <- glo_names[cultureIndex]
  
  model <- eval(parse(text = paste(modelClass, ".", cultureDimension, ".brm.fit", sep ="")))
  theTitle <- paste("Posterior Predictive Check", " (", modelName, " on ", cultureName, ")", sep = "")
  fileNameEnd <- paste(modelClass, "_", cultureDimension,".png", sep="")
  
  pp_check(model) + 
    labs(title = theTitle, 
         subtitle = ("Note: Density overlay using 10 posterior samples"))
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

oneStepPPCheck(2, 1)
