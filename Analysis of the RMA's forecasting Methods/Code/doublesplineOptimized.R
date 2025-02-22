library(parallel)

doublesplineOptimized = function(df, buffer) {
  df <- df[order(df$CommodityYear), ]
  firstStart<-min(df$CommodityYear)
  lastStart<-max(df$CommodityYear)-31
  modelSize<-lastStart - firstStart
  bestModels<-vector("list", modelSize)
  
  cl<-makeCluster(detectCores()-1)
  on.exit(stopCluster(cl))
  
  clusterExport(cl, varlist = c("df", "firstStart", "lastStart", "buffer"), envir = environment())
  clusterEvalQ(cl, library(stats))
  
  bestModels <- parLapply(cl, firstStart:lastStart, function(j) {
    bestModel<-NULL
    models<-list()
    windowEnd<-j+29
    dfWindow <- df[df$CommodityYear >= j & df$CommodityYear <= windowEnd, ]
    
    for (i in 6:(nrow(dfWindow)-buffer+1)) {
      breakpointYear <- dfWindow$CommodityYear[i]
      dfWindow$spline <- ifelse(dfWindow$CommodityYear > breakpointYear, dfWindow$CommodityYear - breakpointYear, 0)
      secondBreakpointModels<-list()
      for (k in (i+1):(nrow(dfWindow)-buffer)) {
        break2<-dfWindow$CommodityYear[k]
        dfWindow$spline2 <- ifelse(dfWindow$CommodityYear > break2, dfWindow$CommodityYear - break2, 0)
        lm_model <- lm(dryPlYldFinal ~ CommodityYear + spline + spline2, data = dfWindow)
        forecastYear<- windowEnd+2
        forecastBreak1<- forecastYear - breakpointYear
        forecastBreak2<- forecastYear - break2
        lm_model$forecastVal<-predict(lm_model,  data.frame(CommodityYear = forecastYear, spline = forecastBreak1, spline2 = forecastBreak2))
        lm_model$SSE<-sum((lm_model$residuals)^2)
        lm_model$AIC<-AIC(lm_model)
        
        foreYear<-subset(df, df$CommodityYear == windowEnd+2)
        lm_model$forecastYear<-foreYear$CommodityYear
        lm_model$actualForecastYield<-foreYear$dryPlYldFinal
        lm_model$forecastError<-lm_model$forecastVal-lm_model$actualForecastYield
        
        if(is.null(bestModel)){
          bestModel<-lm_model
          bestModel$breakpoint<-breakpointYear
        } else if(sum((bestModel$residuals)^2) > sum((lm_model$residuals)^2)){
          bestModel<-lm_model
          bestModel$breakpoint1<-breakpointYear
          bestModel$breakpoint2<-break2
        }
        
        
      }#end loop on second breakpoint
    }#end loop on first breakpoint
    
    bestModel
  })#end loop on all windows

  
  names(bestModels) <- paste0("Best model for ", firstStart:lastStart)
  
  return(bestModels)
}#end function

