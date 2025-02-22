
library(openxlsx)
load("phase3BestSplineModels.Rdata")

excel5Year <- data.frame(fullfips=character(),
                    commodityCode=character(),
                    ForecastYear=integer(),
                    forecast=numeric(),
                    dryPlYldFinal=numeric(),
                    forecastError=numeric(),
                    ModelType=factor())

countyCrop <- strsplit(names(bestModels5Year), "_")

for (i in 1:length(bestModels5Year)) {
  ccc <- bestModels5Year[[i]]
  for (model in ccc) {
    if(length(model$forecastYear) > 0 && length(model$forecastVal) > 0 && length(model$actualForecastYield) > 0 && length(model$forecastError) > 0 && length(model$forecastVal) > 0){
      newRow <- data.frame(County=countyCrop[[i]][1],
                         Crop=countyCrop[[i]][2],
                         ForecastYear=model$forecastYear,
                         ForecastedValue=model$forecastVal,
                         ObservedYield=model$actualForecastYield,
                         ForecastError=model$forecastError,
                         ModelType=model$type)
    
    excel5Year <- rbind(excel5Year, newRow)
   # }
    }
  }
}

listOfCounties <- split(excel5Year, excel5Year$County)
wb5Year <- createWorkbook() 

for (county in names(listOfCounties)) {
  addWorksheet(wb5Year, county) 
  writeData(wb5Year, county, listOfCounties[[county]]) 
}

saveWorkbook(wb5Year, "CountiesData5Year.xlsx", overwrite = TRUE)


excel10Year <- data.frame(fullfips=character(),
                    commodityCode=character(),
                    ForecastYear=integer(),
                    forecast=numeric(),
                    dryPlYldFinal=numeric(),
                    forecastError=numeric(),
                    ModelType=factor())

countyCrop <- strsplit(names(bestModels10Year), "_")

for (i in 1:length(bestModels10Year)) {
  ccc <- bestModels10Year[[i]]
  for (model in ccc) {
    if(length(model$forecastYear) > 0 && length(model$forecastVal) > 0 && length(model$actualForecastYield) > 0 && length(model$forecastError) > 0 && length(model$forecastVal) > 0){
      newRow <- data.frame(County=countyCrop[[i]][1],
                           Crop=countyCrop[[i]][2],
                           ForecastYear=model$forecastYear,
                           ForecastedValue=model$forecastVal,
                           ObservedYield=model$actualForecastYield,
                           ForecastError=model$forecastError,
                           ModelType=model$type)
      
      excel10Year <- rbind(excel10Year, newRow)
      # }
    }
  }
}

listOfCounties <- split(excel10Year, excel10Year$County)
wb10Year <- createWorkbook() 

for (county in names(listOfCounties)) {
  addWorksheet(wb10Year, county) 
  writeData(wb10Year, county, listOfCounties[[county]]) 
}

saveWorkbook(wb10Year, "CountiesData10Year.xlsx", overwrite = TRUE)

excel15Year <- data.frame(fullfips=character(),
                    commodityCode=character(),
                    ForecastYear=integer(),
                    forecast=numeric(),
                    dryPlYldFinal=numeric(),
                    forecastError=numeric(),
                    ModelType=factor())

countyCrop <- strsplit(names(bestModels15Year), "_")

for (i in 1:length(bestModels15Year)) {
  ccc <- bestModels15Year[[i]]
  for (model in ccc) {
    if(length(model$forecastYear) > 0 && length(model$forecastVal) > 0 && length(model$actualForecastYield) > 0 && length(model$forecastError) > 0 && length(model$forecastVal) > 0){
      newRow <- data.frame(County=countyCrop[[i]][1],
                           Crop=countyCrop[[i]][2],
                           ForecastYear=model$forecastYear,
                           ForecastedValue=model$forecastVal,
                           ObservedYield=model$actualForecastYield,
                           ForecastError=model$forecastError,
                           ModelType=model$type)
      
      excel15Year <- rbind(excel15Year, newRow)
      # }
    }
  }
}

listOfCounties <- split(excel15Year, excel15Year$County)
wb15Year <- createWorkbook() 

for (county in names(listOfCounties)) {
  addWorksheet(wb15Year, county) 
  writeData(wb15Year, county, listOfCounties[[county]]) 
}

RMAforecast5Year<-excel5Year
RMAforecast10Year<-excel10Year
RMAforecast15Year<-excel15Year

save(RMAforecast5Year, RMAforecast10Year, RMAforecast15Year, file = "RMAforecastP3.Rdata")
saveWorkbook(wb15Year, "CountiesData15Year.xlsx", overwrite = TRUE)
saveWorkbook(wb10Year, "CountiesData15Year.xlsx", overwrite = TRUE)
saveWorkbook(wb5Year, "CountiesData15Year.xlsx", overwrite = TRUE)
