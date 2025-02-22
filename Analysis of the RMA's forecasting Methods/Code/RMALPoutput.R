rm(list = ls())
load("LPDoubleSplineModels.Rdata")
load("LPDoubleSplineModels10Year.Rdata")
load("LPDoubleSplineModels15Year.Rdata")

df504 <- read_excel("countyData_CornSoy.xlsx", sheet = "ctyData corn rr504")
df508 <- read_excel("countyData_CornSoy.xlsx", sheet = "ctyData corn rr508")
df766 <- read_excel("countyData_CornSoy.xlsx", sheet = "ctyData soy rr766")
df660 <- read_excel("countyData_CornSoy.xlsx", sheet = "ctyData soy rr660")
df660<-subset(df660,df660$CommodityYear>1983)
dfbig<-rbind(df504,df508,df766,df660)

RMALPmodels15Year<-data.frame()
for (i in 1:length(doubleSplineRRLP15Year)) {
  ccc <- doubleSplineRRLP15Year[[i]]
  for (model in ccc) {
    newRow <- data.frame(County=as.double(names(doubleSplineRRLP15Year[i])),
                         ForecastYear=model$forecastYear,
                         ForecastedValue=model$forecastVal,
                         ObservedYield=model$actualForecastYield,
                         ForecastError=model$forecastVal - model$actualForecastYield)
    
    RMALPmodels15Year <- rbind(RMALPmodels15Year, newRow)
    
  }
}

save(RMALPmodels15Year, file = "RMALPmodels15Year.Rdata")

RMALPmodels10Year<-data.frame()
for (i in 1:length(doubleSplineRRLP10Year)) {
  ccc <- doubleSplineRRLP10Year[[i]]
  for (model in ccc) {
      newRow <- data.frame(County=as.double(names(doubleSplineRRLP10Year[i])),
                           ForecastYear=model$forecastYear,
                           ForecastedValue=model$forecastVal,
                           ObservedYield=model$actualForecastYield,
                           ForecastError=model$forecastVal - model$actualForecastYield)
      
      RMALPmodels10Year <- rbind(RMALPmodels10Year, newRow)
    
  }
}

save(RMALPmodels10Year, file = "RMALPmodels10Year.Rdata")
write.xlsx(RMALPmodels, file = "RMALPmodels.xlsx")

RMALPmodels<-data.frame()
for (i in 1:length(doubleSplineRRLP)) {
  ccc <- doubleSplineRRLP[[i]]
  for (model in ccc) {
    newRow <- data.frame(County=as.double(names(doubleSplineRRLP[i])),
                         ForecastYear=model$forecastYear,
                         ForecastedValue=model$forecastVal,
                         ObservedYield=model$actualForecastYield,
                         ForecastError=model$forecastVal - model$actualForecastYield)
    
    RMALPmodels <- rbind(RMALPmodels, newRow)
    
  }
}

save(RMALPmodels, file = "RMALPmodels.Rdata")

