## build county models 
library(readxl)
rm(list = ls())

source("singlesplineOptimized3.R")
source("doublesplineOptimized3.R")
source("zerosplineP2.R")

saveModels5Year<-list()

df1<-read_xlsx("riskRegion_drylandCornSoy.xlsx")
df2<-read_xlsx("riskRegion_drylandCornSoy.xlsx", sheet = 2)
df3<-read_xlsx("riskRegion_drylandCornSoy.xlsx", sheet = 3)
df4<-read_xlsx("riskRegion_drylandCornSoy.xlsx", sheet = 4)
df4<-subset(df4, df4$yr >1983)
dfBig<-rbind(df1,df2,df3,df4)

############################################################

zerospline504<-zerosplineOptimized(df1)
singlespline504_5Year<-singlesplineOptimized3(df1,5)
doublespline504_5Year<-doublesplineOptimized3(df1,5)
bestModels504_5Year<-list()

zerospline508<-zerosplineOptimized(df2)
singlespline508_5Year<-singlesplineOptimized3(df2,5)
doublespline508_5Year<-doublesplineOptimized3(df2,5)
bestModels508_5Year<-list()

zerospline660<-zerosplineOptimized(df4)
singlespline660_5Year<-singlesplineOptimized3(df4,5)
doublespline660_5Year<-doublesplineOptimized3(df4,5)
bestModels660_5Year<-list()

zerospline766<-zerosplineOptimized(df3)
singlespline766_5Year<-singlesplineOptimized3(df3,5)
doublespline766_5Year<-doublesplineOptimized3(df3,5)
bestModels766_5Year<-list()


singlespline504_10Year<-singlesplineOptimized3(df1,10)
doublespline504_10Year<-doublesplineOptimized3(df1,10)
bestModels504_10Year<-list()

singlespline508_10Year<-singlesplineOptimized3(df2,10)
doublespline508_10Year<-doublesplineOptimized3(df2,10)
bestModels508_10Year<-list()

singlespline660_10Year<-singlesplineOptimized3(df4,10)
doublespline660_10Year<-doublesplineOptimized3(df4,10)
bestModels660_10Year<-list()

singlespline766_10Year<-singlesplineOptimized3(df3,10)
doublespline766_10Year<-doublesplineOptimized3(df3,10)
bestModels766_10Year<-list()


singlespline504_15Year<-singlesplineOptimized3(df1,15)
doublespline504_15Year<-doublesplineOptimized3(df1,15)
bestModels504_15Year<-list()

singlespline508_15Year<-singlesplineOptimized3(df2,15)
doublespline508_15Year<-doublesplineOptimized3(df2,15)
bestModels508_15Year<-list()

singlespline660_15Year<-singlesplineOptimized3(df4,15)
doublespline660_15Year<-doublesplineOptimized3(df4,15)
bestModels660_15Year<-list()

singlespline766_15Year<-singlesplineOptimized3(df3,15)
doublespline766_15Year<-doublesplineOptimized3(df3,15)
bestModels766_15Year<-list()



####################################################
for (i in 1:length(zerospline504)) {
  zeroModel<-zerospline504[[i]]
  singleModel_5Year<-singlespline504_5Year[[i]]
  doubleModel_5Year<-doublespline504_5Year[[i]]
  
  singleModel_10Year<-singlespline504_10Year[[i]]
  doubleModel_10Year<-doublespline504_10Year[[i]]
  
  singleModel_15Year<-singlespline504_15Year[[i]]
  doubleModel_15Year<-doublespline504_15Year[[i]]

  
  AICcomp_5 <- c(zeroModel$AIC, singleModel_5Year$AIC, doubleModel_15Year$AIC)
  lowest_5 <- min(AICcomp_5)
  best_5 <- which(AICcomp_5 == lowest_5)
  
  if (best_5 == 1) {
    bestModel_5 <- zeroModel
    bestModel_5$type<-"zeroSpline"
  } else if (best_5 == 2) {
    bestModel_5 <- singleModel_5Year
    bestModel_5$type<-"singleSpline"
  } else {
    bestModel_5 <- doubleModel_5Year
    bestModel_5$type<-"doubleSpline"
  }
  bestModels504_5Year[[i]]<-bestModel_5
  
  AICcomp_10 <- c(zeroModel$AIC, singleModel_10Year$AIC, doubleModel_15Year$AIC)
  lowest_10 <- min(AICcomp_10)
  best_10 <- which(AICcomp_10 == lowest_10)
  
  if (best_10 == 1) {
    bestModel_10 <- zeroModel
    bestModel_10$type<-"zeroSpline"
  } else if (best_10== 2) {
    bestModel_10 <- singleModel_10Year
    bestModel_10$type<-"singleSpline"
  } else {
    bestModel_10 <- doubleModel_10Year
    bestModel_10$type<-"doubleSpline"
  }
  bestModels504_10Year[[i]]<-bestModel_10
  
  AICcomp_15 <- c(zeroModel$AIC, singleModel_15Year$AIC, doubleModel_15Year$AIC)
  lowest_15 <- min(AICcomp_15)
  best_15 <- which(AICcomp_15 == lowest_15)
  
  if (best_15 == 1) {
    bestModel_15 <- zeroModel
    bestModel_15$type<-"zeroSpline"
  } else if (best_15 == 2) {
    bestModel_15 <- singleModel_15Year
    bestModel_15$type<-"singleSpline"
  } else {
    bestModel_15 <- doubleModel_15Year
    bestModel_15$type<-"doubleSpline"
  }
  bestModels504_15Year[[i]]<-bestModel_15
}
listNames<-paste0("Best Model for ",df1$yr)[1:length(zerospline504)]
names(bestModels504_5Year)<-listNames
names(bestModels504_10Year)<-listNames
names(bestModels504_15Year)<-listNames
#######################################################################

for (i in 1:length(zerospline508)) {
  zeroModel<-zerospline508[[i]]
  singleModel_5Year<-singlespline508_5Year[[i]]
  doubleModel_5Year<-doublespline508_5Year[[i]]
  
  singleModel_10Year<-singlespline508_10Year[[i]]
  doubleModel_10Year<-doublespline508_10Year[[i]]
  
  singleModel_15Year<-singlespline508_15Year[[i]]
  doubleModel_15Year<-doublespline508_15Year[[i]]
  
  
  AICcomp_5 <- c(zeroModel$AIC, singleModel_5Year$AIC, doubleModel_15Year$AIC)
  lowest_5 <- min(AICcomp_5)
  best_5 <- which(AICcomp_5 == lowest_5)
  
  if (best_5 == 1) {
    bestModel_5 <- zeroModel
    bestModel_5$type<-"zeroSpline"
  } else if (best_5 == 2) {
    bestModel_5 <- singleModel_5Year
    bestModel_5$type<-"singleSpline"
  } else {
    bestModel_5 <- doubleModel_5Year
    bestModel_5$type<-"doubleSpline"
  }
  bestModels508_5Year[[i]]<-bestModel_5
  
  AICcomp_10 <- c(zeroModel$AIC, singleModel_10Year$AIC, doubleModel_15Year$AIC)
  lowest_10 <- min(AICcomp_10)
  best_10 <- which(AICcomp_10 == lowest_10)
  
  if (best_10 == 1) {
    bestModel_10 <- zeroModel
    bestModel_10$type<-"zeroSpline"
  } else if (best_10 == 2) {
    bestModel_10 <- singleModel_10Year
    bestModel_10$type<-"singleSpline"
  } else {
    bestModel_10 <- doubleModel_10Year
    bestModel_10$type<-"doubleSpline"
  }
  bestModels508_10Year[[i]]<-bestModel_10
  
  AICcomp_15 <- c(zeroModel$AIC, singleModel_15Year$AIC, doubleModel_15Year$AIC)
  lowest_15 <- min(AICcomp_15)
  best_15 <- which(AICcomp_15 == lowest_15)
  
  if (best_15 == 1) {
    bestModel_15 <- zeroModel
    bestModel_15$type<-"zeroSpline"
  } else if (best_15 == 2) {
    bestModel_15 <- singleModel_15Year
    bestModel_15$type<-"singleSpline"
  } else {
    bestModel_15 <- doubleModel_15Year
    bestModel_15$type<-"doubleSpline"
  }
  bestModels508_15Year[[i]]<-bestModel_15
}
listNames<-paste0("Best Model for ",df1$yr)[1:length(zerospline508)]
names(bestModels508_5Year)<-listNames
names(bestModels508_10Year)<-listNames
names(bestModels508_15Year)<-listNames
for (i in 1:length(zerospline660)) {
  zeroModel<-zerospline660[[i]]
  singleModel_5Year<-singlespline660_5Year[[i]]
  doubleModel_5Year<-doublespline660_5Year[[i]]
  
  singleModel_10Year<-singlespline660_10Year[[i]]
  doubleModel_10Year<-doublespline660_10Year[[i]]
  
  singleModel_15Year<-singlespline660_15Year[[i]]
  doubleModel_15Year<-doublespline660_15Year[[i]]
  
  
  AICcomp_5 <- c(zeroModel$AIC, singleModel_5Year$AIC, doubleModel_15Year$AIC)
  lowest_5 <- min(AICcomp_5)
  best_5 <- which(AICcomp_5 == lowest_5)
  
  if (best_5 == 1) {
    bestModel_5 <- zeroModel
    bestModel_5$type<-"zeroSpline"
  } else if (best_5 == 2) {
    bestModel_5 <- singleModel_5Year
    bestModel_5$type<-"singleSpline"
  } else {
    bestModel_5 <- doubleModel_5Year
    bestModel_5$type<-"doubleSpline"
  }
  bestModels660_5Year[[i]]<-bestModel_5
  
  AICcomp_10 <- c(zeroModel$AIC, singleModel_10Year$AIC, doubleModel_15Year$AIC)
  lowest_10 <- min(AICcomp_10)
  best_10 <- which(AICcomp_10 == lowest_10)
  
  if (best_10 == 1) {
    bestModel_10 <- zeroModel
    bestModel_10$type<-"zeroSpline"
  } else if (best_10 == 2) {
    bestModel_10 <- singleModel_10Year
    bestModel_10$type<-"singleSpline"
  } else {
    bestModel_10 <- doubleModel_10Year
    bestModel_10$type<-"doubleSpline"
  }
  bestModels660_10Year[[i]]<-bestModel_10
  
  AICcomp_15 <- c(zeroModel$AIC, singleModel_15Year$AIC, doubleModel_15Year$AIC)
  lowest_15 <- min(AICcomp_15)
  best_15 <- which(AICcomp_15 == lowest_15)
  
  if (best_15 == 1) {
    bestModel_15 <- zeroModel
    bestModel_15$type<-"zeroSpline"
  } else if (best_15 == 2) {
    bestModel_15 <- singleModel_15Year
    bestModel_15$type<-"singleSpline"
  } else {
    bestModel_15 <- doubleModel_15Year
    bestModel_15$type<-"doubleSpline"
  }
  bestModels660_15Year[[i]]<-bestModel_15
}
listNames<-paste0("Best Model for ",df1$yr)[1:length(zerospline660)]
names(bestModels660_5Year)<-listNames
names(bestModels660_10Year)<-listNames
names(bestModels660_15Year)<-listNames

for (i in 1:length(zerospline766)) {
  zeroModel<-zerospline766[[i]]
  singleModel_5Year<-singlespline766_5Year[[i]]
  doubleModel_5Year<-doublespline766_5Year[[i]]
  
  singleModel_10Year<-singlespline766_10Year[[i]]
  doubleModel_10Year<-doublespline766_10Year[[i]]
  
  singleModel_15Year<-singlespline766_15Year[[i]]
  doubleModel_15Year<-doublespline766_15Year[[i]]
  
  
  AICcomp_5 <- c(zeroModel$AIC, singleModel_5Year$AIC, doubleModel_15Year$AIC)
  lowest_5 <- min(AICcomp_5)
  best_5 <- which(AICcomp_5 == lowest_5)
  
  if (best_5 == 1) {
    bestModel_5 <- zeroModel
    bestModel_5$type<-"zeroSpline"
  } else if (best_5 == 2) {
    bestModel_5 <- singleModel_5Year
    bestModel_5$type<-"singleSpline"
  } else {
    bestModel_5 <- doubleModel_5Year
    bestModel_5$type<-"doubleSpline"
  }
  bestModels766_5Year[[i]]<-bestModel_5
  
  AICcomp_10 <- c(zeroModel$AIC, singleModel_10Year$AIC, doubleModel_15Year$AIC)
  lowest_10 <- min(AICcomp_10)
  best_10 <- which(AICcomp_10 == lowest_10)
  
  if (best_10 == 1) {
    bestModel_10 <- zeroModel
    bestModel_10$type<-"zeroSpline"
  } else if (best_10 == 2) {
    bestModel_10 <- singleModel_10Year
    bestModel_10$type<-"singleSpline"
  } else {
    bestModel_10 <- doubleModel_10Year
    bestModel_10$type<-"doubleSpline"
  }
  bestModels766_10Year[[i]]<-bestModel_10
  
  AICcomp_15 <- c(zeroModel$AIC, singleModel_15Year$AIC, doubleModel_15Year$AIC)
  lowest_15 <- min(AICcomp_15)
  best_15 <- which(AICcomp_15 == lowest_15)
  
  if (best_15 == 1) {
    bestModel_15 <- zeroModel
    bestModel_15$type<-"zeroSpline"
  } else if (best_15 == 2) {
    bestModel_15 <- singleModel_15Year
    bestModel_15$type<-"singleSpline"
  } else {
    bestModel_15 <- doubleModel_15Year
    bestModel_15$type<-"doubleSpline"
  }
  bestModels766_15Year[[i]]<-bestModel_15
}
listNames<-paste0("Best Model for ",df1$yr)[1:length(zerospline766)]
names(bestModels766_5Year)<-listNames
names(bestModels766_10Year)<-listNames
names(bestModels766_15Year)<-listNames

save(bestModels504_5Year, bestModels504_10Year, bestModels504_15Year,
     bestModels508_5Year, bestModels508_10Year, bestModels508_15Year,
     bestModels660_5Year, bestModels660_10Year, bestModels660_15Year,
     bestModels766_5Year,bestModels766_10Year, bestModels766_15Year, file = "RiskRegionModels.Rdata")
