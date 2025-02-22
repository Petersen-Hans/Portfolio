rm(list = ls())
load("LPDoubleSplineModels.Rdata")

fittedResults <- list()
test<-list()
for (county in names(doubleSplineRRLP)) {
  countyModels <- doubleSplineRRLP[[county]]
  for (modelName in names(countyModels)) {
    model <- countyModels[[modelName]]
    yearRange <- seq(model$startYear, model$startYear + 31)
    fittedValues <- model$solution[1] + model$solution[2] * yearRange + 
      ifelse(yearRange > model$breakpoint1, model$solution[3] * (yearRange - model$breakpoint1), 0) +
      ifelse(yearRange > model$breakpoint2, model$solution[4] * (yearRange - model$breakpoint2), 0)
    fittedResults[[paste0(county, "_", modelName)]] <- data.frame(Year = yearRange, FittedValues = fittedValues)
    model$fittedValues<-fittedValues
    tmp<-ifelse( model$solution[1] + model$solution[2] >  model$solution[1] + model$solution[2] +model$solution[3], 1 ,0)
    test[[paste0(county, "_", modelName)]]<-tmp
  }
}

# Save the fitted results to a file or examine them directly
save(fittedResults, file = "FittedResults.RData")
