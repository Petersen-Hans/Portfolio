library(parallel)

zerosplineOptimized <- function(df) {
  df <- df[order(df$yr), ]
  firstStart <- min(df$yr)
  lastStart <- max(df$yr) - 31
  
  modelSize <- lastStart - firstStart + 1
  bestModels <- vector("list", modelSize)
  
  cl <- makeCluster(detectCores() - 1)

  clusterExport(cl, varlist = c("df", "firstStart", "lastStart"), envir = environment())
  
  clusterEvalQ(cl, library(stats))
  
  results <- parLapply(cl, firstStart:lastStart, function(j) {
    windowEnd <- j + 29
    
    dfWindow <- subset(df, yr >= j & yr <= windowEnd)
    zero_model <- lm(ObservedValue ~ yr, data = dfWindow)
    zero_model$SSE <- sum(residuals(zero_model)^2)
    zero_model$AIC <- AIC(zero_model)
    forecastYear <- windowEnd + 2
    zero_model$forecastVal <- predict(zero_model, newdata = data.frame(yr = forecastYear))
    
    foreYear<-subset(df, df$yr == windowEnd+2)
    zero_model$forecastYear<-foreYear$yr
    zero_model$actualForecastYield<-foreYear$ObservedValue
    zero_model$forecastError<-zero_model$forecastVal-zero_model$actualForecastYield
    zero_model
  })
  
  stopCluster(cl)
  
  names(results) <- paste0("Best model for ", seq(firstStart, lastStart))
  bestModels <- results
  
  return(bestModels)
}
