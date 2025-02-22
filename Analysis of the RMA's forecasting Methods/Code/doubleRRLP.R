library(nloptr)
library(readxl)
library(parallel)
rm(list=ls())


df504<-read_xlsx("riskRegion_drylandCornSoy.xlsx")
df508<-read_xlsx("riskRegion_drylandCornSoy.xlsx", sheet = 2)
df766<-read_xlsx("riskRegion_drylandCornSoy.xlsx", sheet = 3)
df660<-read_xlsx("riskRegion_drylandCornSoy.xlsx", sheet = 4)
df660<-subset(df660, df660$yr >1983)
dfBig<-rbind(df504,df508,df766,df660)

RRlist<-list(df504, df508, df766, df660)

LPmodels<-list()

for (z in 1:length(RRlist)) {
  
  dfcounty<-RRlist[[z]]
  dfcounty <- dfcounty[order(dfcounty$yr), ]
  firstStart<-min(dfcounty$yr)
  lastStart<-max(dfcounty$yr)-31
  modelSize<-lastStart - firstStart
  countyModels<-list()
  
  for (j in firstStart:lastStart) {
    bestModel<-NULL
    models<-list()
    windowEnd<-j+29
    dfWindow <- dfcounty[dfcounty$yr >= j & dfcounty$yr <= windowEnd, ]
    
    for (i in 6:(nrow(dfWindow)-16)) {
      breakpointYear <- dfWindow$yr[i]
      dfWindow$spline1 <- ifelse(dfWindow$yr > breakpointYear, dfWindow$yr - breakpointYear, 0)
      for(k in (i+1):(nrow(dfWindow)-15)){
        break2<-dfWindow$yr[k]
        dfWindow$spline2 <- ifelse(dfWindow$yr > break2, dfWindow$yr - break2, 0)
        
        objective_function <- function(params, data) {
          # Extract parameters
          beta0 <- params[1]  # Intercept
          beta1 <- params[2]  # Slope before change
          beta2 <- params[3]  # Additional slope after change
          beta3 <- params[4]
          predicted_yield <- beta0 + beta1 * data$yr + beta2 * data$spline1 + beta3 * data$spline2
          
          residuals <- data$ObservedValue - predicted_yield
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
        
        foreYear<-subset(dfcounty, dfcounty$yr == windowEnd+2)
        result$forecastYear<-foreYear$yr
        result$actualForecastYield<-foreYear$ObservedValue
        
        if(is.null(bestModel)){
          bestModel<-result
          bestModel$breakpoint1<-breakpointYear
          bestModel$breakpoint2<-break2
          bestModel$startYear <- j
          bestModel$forecastVal<-bestModel$solution[1] + bestModel$solution[2] * (bestModel$startYear+31) + bestModel$solution[3] * (bestModel$startYear+31 - bestModel$breakpoint1)+ bestModel$solution[4] * (bestModel$startYear+31 - bestModel$breakpoint2)
          bestModel$forecastError<-bestModel$forecastVal-bestModel$actualForecastYield
          
        } else if(bestModel$objective > result$objective){
          bestModel<-result
          bestModel$breakpoint1<-breakpointYear
          bestModel$breakpoint2<-break2
          bestModel$startYear <- j
          bestModel$forecastVal<-bestModel$solution[1] + bestModel$solution[2] * (bestModel$startYear+31) + bestModel$solution[3] * (bestModel$startYear+31 - bestModel$breakpoint1) + bestModel$solution[4] * (bestModel$startYear+31 - bestModel$breakpoint2)
          bestModel$forecastError<-bestModel$forecastVal-bestModel$actualForecastYield
        }
        
      }
    }
    countyModels[[paste0("Best model for ", j)]] <- bestModel
  }
  LPmodels[[z]]<-countyModels
}
doubleSplineRRLP<-LPmodels
names(doubleSplineRRLP)<-c("504","508","776","660")
save(doubleSplineRRLP, file = "RiskRegionDoubleSplineLPModels.Rdata")
