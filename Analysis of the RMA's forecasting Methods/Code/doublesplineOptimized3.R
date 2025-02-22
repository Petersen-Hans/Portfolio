library(parallel)

doublesplineOptimized3 = function(df, buffer) {
  df <- df[order(df$yr), ]
  firstStart<-min(df$yr)
  lastStart<-max(df$yr)-31
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
    dfWindow <- df[df$yr >= j & df$yr <= windowEnd, ]
    
    for (i in 6:(nrow(dfWindow)-buffer+1)) {
      breakpointYear <- dfWindow$yr[i]
      dfWindow$spline <- ifelse(dfWindow$yr > breakpointYear, dfWindow$yr - breakpointYear, 0)
      secondBreakpointModels<-list()
      for (k in (i+1):(nrow(dfWindow)-buffer)) {
        break2<-dfWindow$yr[k]
        dfWindow$spline2 <- ifelse(dfWindow$yr > break2, dfWindow$yr - break2, 0)
        lm_model <- lm(ObservedValue ~ yr + spline + spline2, data = dfWindow)
        forecastYear<- windowEnd+2
        forecastBreak1<- forecastYear - breakpointYear
        forecastBreak2<- forecastYear - break2
        lm_model$forecastVal<-predict(lm_model,  data.frame(yr = forecastYear, spline = forecastBreak1, spline2 = forecastBreak2))
        lm_model$SSE<-sum((lm_model$residuals)^2)
        lm_model$AIC<-AIC(lm_model)
        
        foreYear<-subset(df, df$yr == windowEnd+2)
        lm_model$forecastYear<-foreYear$yr
        lm_model$actualForecastYield<-foreYear$ObservedValue
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

