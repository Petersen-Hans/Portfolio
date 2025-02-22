library(parallel)

zerosplineOptimized <- function(df) {
  df <- df[order(df$CommodityYear), ]
  firstStart <- min(df$CommodityYear)
  lastStart <- max(df$CommodityYear) - 31
  
  modelSize <- lastStart - firstStart + 1
  bestModels <- vector("list", modelSize)
  
  cl <- makeCluster(detectCores() - 1)

  clusterExport(cl, varlist = c("df", "firstStart", "lastStart"), envir = environment())
  
  clusterEvalQ(cl, library(stats))
  
  results <- parLapply(cl, firstStart:lastStart, function(j) {
    windowEnd <- j + 29
    
    dfWindow <- subset(df, CommodityYear >= j & CommodityYear <= windowEnd)
    zero_model <- lm(dryPlYldFinal ~ CommodityYear, data = dfWindow)
    zero_model$SSE <- sum(residuals(zero_model)^2)
    zero_model$AIC <- AIC(zero_model)
    forecastYear <- windowEnd + 2
    zero_model$forecastVal <- predict(zero_model, newdata = data.frame(CommodityYear = forecastYear))
    
    foreYear<-subset(df, df$CommodityYear == windowEnd+2)
    zero_model$forecastYear<-foreYear$CommodityYear
    zero_model$actualForecastYield<-foreYear$dryPlYldFinal
    zero_model$forecastError<-zero_model$forecastVal-zero_model$actualForecastYield
    zero_model
  })
  
  stopCluster(cl)
  
  names(results) <- paste0("Best model for ", seq(firstStart, lastStart))
  bestModels <- results
  
  return(bestModels)
}
