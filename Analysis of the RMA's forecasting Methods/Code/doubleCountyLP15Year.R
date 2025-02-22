library(nloptr)
library(readxl)
library(parallel)
rm(list=ls())


df504 <- read_excel("countyData_CornSoy.xlsx", sheet = "ctyData corn rr504")
df508 <- read_excel("countyData_CornSoy.xlsx", sheet = "ctyData corn rr508")
df766 <- read_excel("countyData_CornSoy.xlsx", sheet = "ctyData soy rr766")
df660 <- read_excel("countyData_CornSoy.xlsx", sheet = "ctyData soy rr660")
df660<-subset(df660,df660$CommodityYear>1983)

dfbig<-rbind(df504,df508,df766,df660)

counties<-unique(dfbig$fullfips)

LPmodels<-list()
cl<-makeCluster(detectCores()-1)
clusterExport(cl, varlist = c("dfbig", "counties"), envir = .GlobalEnv)
clusterEvalQ(cl, library(nloptr))
LPmodels<-parLapply(cl, 1:length(counties), function(z){
  #for (z in 1:length(counties)) {
  
  
  dfcounty<-subset(dfbig, dfbig$fullfips==counties[z])
  dfcounty <- dfcounty[order(dfcounty$CommodityYear), ]
  firstStart<-min(dfcounty$CommodityYear)
  lastStart<-max(dfcounty$CommodityYear)-31
  modelSize<-lastStart - firstStart
  countyModels<-list()
  
  
  for (j in firstStart:lastStart) {
    bestModel<-NULL
    models<-list()
    windowEnd<-j+29
    dfWindow <- dfcounty[dfcounty$CommodityYear >= j & dfcounty$CommodityYear <= windowEnd, ]
    
    for (i in 6:(nrow(dfWindow)-16)) {
      breakpointYear <- dfWindow$CommodityYear[i]
      dfWindow$spline1 <- ifelse(dfWindow$CommodityYear > breakpointYear, dfWindow$CommodityYear - breakpointYear, 0)
      for(k in (i+1):(nrow(dfWindow)-15)){
        break2<-dfWindow$CommodityYear[k]
        dfWindow$spline2 <- ifelse(dfWindow$CommodityYear > break2, dfWindow$CommodityYear - break2, 0)
        
        objective_function <- function(params, data) {
          beta0 <- params[1] 
          beta1 <- params[2]  
          beta2 <- params[3]  
          beta3 <- params[4]
          predicted_yield <- beta0 + beta1 * data$CommodityYear + beta2 * data$spline1 + beta3 * data$spline2
          residuals <- data$dryPlYldFinal - predicted_yield
          SSE <- sum(residuals^2)
          
          return(SSE)
        }
        initial_params <- c(intercept = 0, slope1 = 1, slope2 = 1, slope3 = 1)
        constraints <- function(params) {
          -sum(params['slope1'], params['slope2'], params['slope3'])
        }
        opts <- list("algorithm"="NLOPT_LN_COBYLA", "xtol_rel"=1.0e-7, "minf_max"=1.0e-8, "maxeval"=250)
        
        result <- nloptr(x0 = initial_params,
                         eval_f = function(params) objective_function(params, dfWindow),
                         lb = c(-Inf, -10, -10, -10),
                         opts = opts,
                         eval_g_ineq = constraints)
        foreYear<-subset(dfcounty, dfcounty$CommodityYear == windowEnd+2)
        result$forecastYear<-foreYear$CommodityYear
        result$actualForecastYield<-foreYear$dryPlYldFinal
        
        if(is.null(bestModel)){
          bestModel<-result
          bestModel$breakpoint1<-breakpointYear
          bestModel$breakpoint2<-break2
          bestModel$startYear <- j
          bestModel$forecastVal<-bestModel$solution[1] + bestModel$solution[2] * (bestModel$startYear+31) + bestModel$solution[3] * (bestModel$startYear+31 - bestModel$breakpoint1)  + bestModel$solution[4] * (bestModel$startYear+31 - bestModel$breakpoint2)
          bestModel$forecastError<- bestModel$forecastError - bestModel$actualForecastYield
          
        } else if(bestModel$objective > result$objective){
          bestModel<-result
          bestModel$breakpoint1<-breakpointYear
          bestModel$breakpoint2<-break2
          bestModel$startYear <- j
          bestModel$forecastVal<-bestModel$solution[1] + bestModel$solution[2] * (bestModel$startYear+31) + bestModel$solution[3] * (bestModel$startYear+31 - bestModel$breakpoint1)  + bestModel$solution[4] * (bestModel$startYear+31 - bestModel$breakpoint2)
          bestModel$forecastError<- bestModel$forecastVal - bestModel$actualForecastYield
        }
        
      }
    }
    countyModels[[paste0("Best model for ", j)]] <- bestModel
  }
  countyModels
})

names(LPmodels)<-counties
doubleSplineRRLP15Year<-LPmodels
stopCluster(cl)
save(doubleSplineRRLP15Year, file = "LPDoubleSplineModels15Year.Rdata")
