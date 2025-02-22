###Compare models
rm(list=ls())
load("phase3SplineModels.Rdata")
library(readxl)

df1<-read_xlsx("countyData_dryrlandCornSoy.xlsx")
df2<-read_xlsx("countyData_dryrlandCornSoy.xlsx", sheet = 2)
df3<-read_xlsx("countyData_dryrlandCornSoy.xlsx", sheet = 3)
df4<-read_xlsx("countyData_dryrlandCornSoy.xlsx", sheet = 4)
dfBig<-rbind(df1,df2,df3,df4)

counties<-unique(dfBig$fullfips)
crops<-unique(dfBig$commodityCode)

bestModels5Year<-list()
bestModels10Year<-list()
bestModels15Year<-list()

cccorn<-paste(counties,"41", sep = "_")
ccsoy<-paste(counties,"81", sep = "_")
ccc<-c(cccorn, ccsoy)

countyGrouped <- lapply(ccc, function(pattern) {
  select <- grep(pattern, names(saveModels5Year))
  saveModels5Year[select]
})

for(i in 1:length(countyGrouped)){
  modelYears<-list()
  testCounty<-countyGrouped[[i]]

  if(length(testCounty)!=0){
     itterNames = names(testCounty[[1]])
     for(k in itterNames){
       zeroSplineModel <- testCounty[[1]][[k]]
       singleSplineModel <- testCounty[[2]][[k]]
       doubleSplineModel <- testCounty[[3]][[k]]
       AICcomp <- c(zeroSplineModel$AIC, singleSplineModel$AIC, doubleSplineModel$AIC)
       lowest <- min(AICcomp)
       best <- which(AICcomp == lowest)
       
       if (best == 1) {
         bestModel <- testCounty[[1]][[k]]
         bestModel$type<-"zeroSpline"
       } else if (best == 2) {
         bestModel <- testCounty[[2]][[k]]
         bestModel$type<-"singleSpline"
       } else {
         bestModel <- testCounty[[3]][[k]]
         bestModel$type<-"doubleSpline"
       }
       modelYears[[k]]<-bestModel
     }
     
  bestModels5Year[[i]]<-modelYears
  }
}
names(bestModels5Year)<-ccc
bestModels5Year <- Filter(Negate(is.null), bestModels5Year)
################
countyGrouped <- lapply(ccc, function(pattern) {
  select <- grep(pattern, names(saveModels10Year))
  saveModels10Year[select]
})

for(i in 1:length(countyGrouped)){
  modelYears<-list()
  testCounty<-countyGrouped[[i]]
  
  if(length(testCounty)!=0){
    itterNames = names(testCounty[[1]])
    for(k in itterNames){
      zeroSplineModel <- testCounty[[1]][[k]]
      singleSplineModel <- testCounty[[2]][[k]]
      doubleSplineModel <- testCounty[[3]][[k]]
      AICcomp <- c(zeroSplineModel$AIC, singleSplineModel$AIC, doubleSplineModel$AIC)
      lowest <- min(AICcomp)
      best <- which(AICcomp == lowest)
      
      if (best == 1) {
        bestModel <- testCounty[[1]][[k]]
        bestModel$type<-"zeroSpline"
      } else if (best == 2) {
        bestModel <- testCounty[[2]][[k]]
        bestModel$type<-"singleSpline"
      } else {
        bestModel <- testCounty[[3]][[k]]
        bestModel$type<-"doubleSpline"
      }
      modelYears[[k]]<-bestModel
    }
    
    bestModels10Year[[i]]<-modelYears
  }
}
names(bestModels10Year)<-ccc
bestModels10Year <- Filter(Negate(is.null), bestModels10Year)
###########################
countyGrouped <- lapply(ccc, function(pattern) {
  select <- grep(pattern, names(saveModels15Year))
  saveModels15Year[select]
})

for(i in 1:length(countyGrouped)){
  modelYears<-list()
  testCounty<-countyGrouped[[i]]
  
  if(length(testCounty)!=0){
    itterNames = names(testCounty[[1]])
    for(k in itterNames){
      zeroSplineModel <- testCounty[[1]][[k]]
      singleSplineModel <- testCounty[[2]][[k]]
      doubleSplineModel <- testCounty[[3]][[k]]
      AICcomp <- c(zeroSplineModel$AIC, singleSplineModel$AIC, doubleSplineModel$AIC)
      lowest <- min(AICcomp)
      best <- which(AICcomp == lowest)
      
      if (best == 1) {
        bestModel <- testCounty[[1]][[k]]
        bestModel$type<-"zeroSpline"
      } else if (best == 2) {
        bestModel <- testCounty[[2]][[k]]
        bestModel$type<-"singleSpline"
      } else {
        bestModel <- testCounty[[3]][[k]]
        bestModel$type<-"doubleSpline"
      }
      modelYears[[k]]<-bestModel
    }
    
    bestModels15Year[[i]]<-modelYears
  }
}
names(bestModels15Year)<-ccc
bestModels15Year <- Filter(Negate(is.null), bestModels15Year)

save(bestModels5Year, bestModels10Year, bestModels15Year, file = "phase3BestSplineModels.Rdata")
