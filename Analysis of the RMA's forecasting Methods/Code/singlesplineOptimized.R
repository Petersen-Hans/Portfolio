library(parallel)

singlesplineOptimized = function(df, buffer) {
  df <- df[order(df$CommodityYear), ]
  firstStart<-min(df$CommodityYear)
  lastStart<-max(df$CommodityYear)-31
  
  modelSize<-lastStart - firstStart
  bestModels<-vector("list", modelSize)
  
  cl<-makeCluster(detectCores()-1)
  on.exit(stopCluster(cl))
  
  clusterExport(cl, varlist = c("df", "firstStart", "lastStart","buffer"), envir = environment())
  clusterEvalQ(cl, library(stats))
  
  bestModels <- parLapply(cl, firstStart:lastStart, function(j) {
    bestModel <- NULL
    windowEnd <- j + 29
    dfWindow <- df[df$CommodityYear >= j & df$CommodityYear <= windowEnd, ]
    
    for (i in 6:(nrow(dfWindow)-buffer)) {
      breakpointYear <- dfWindow$CommodityYear[i]
      dfWindow$spline <- ifelse(dfWindow$CommodityYear > breakpointYear, dfWindow$CommodityYear - breakpointYear, 0)
      lm_model <- lm(dryPlYldFinal ~ CommodityYear + spline, data = dfWindow)
      forecastYear <- windowEnd + 2
      forecastInt <- forecastYear - breakpointYear
      lm_model$forecastVal <- predict(lm_model, newdata = data.frame(CommodityYear = forecastYear, spline = forecastInt))
      lm_model$SSE <- sum((lm_model$residuals)^2)
      lm_model$AIC <- AIC(lm_model)
      
      foreYear<-subset(df, df$CommodityYear == windowEnd+2)
      lm_model$forecastYear<-foreYear$CommodityYear
      lm_model$actualForecastYield<-foreYear$dryPlYldFinal
      lm_model$forecastError<-lm_model$forecastVal-lm_model$actualForecastYield
      
      if(is.null(bestModel) || lm_model$SSE < bestModel$SSE) {
        bestModel <- lm_model
        bestModel$breakpoint <- breakpointYear
      }
    }
    
    bestModel
  })

  names(bestModels) <- paste0("Best model for ", firstStart:lastStart)
  
  return(bestModels)
}

