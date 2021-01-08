#source("NationalCultureCode.R")

library(ggeffects)

marginsAnalysis <- ggpredict(currentModel.fit, terms = c("gassv_c", "country"), type = "random")
plot(marginsAnalysis)
