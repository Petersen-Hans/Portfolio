library(readxl)
rm(list = ls())
source("singlesplineOptimized.R")
source("doublesplineOptimized.R")
source("zerosplineOptimized.R")

saveModels5Year<-list()

df1<-read_xlsx("countyData_dryrlandCornSoy.xlsx")
df2<-read_xlsx("countyData_dryrlandCornSoy.xlsx", sheet = 2)
df3<-read_xlsx("countyData_dryrlandCornSoy.xlsx", sheet = 3)
df4<-read_xlsx("countyData_dryrlandCornSoy.xlsx", sheet = 4)
df4<-subset(df4, df4$CommodityYear >1983)
dfBig<-rbind(df1,df2,df3,df4)


codes<-unique(dfBig$fullfips)
crops<-unique(dfBig$commodityCode)

buffer = 5

for (crop in crops){
  dfCrop<-subset(dfBig, dfBig$commodityCode == crop)
  for (county in codes) {
    print(paste(crop," in county ",county))
    temp<-subset(dfCrop, dfCrop$fullfips==county)
    if(nrow(temp)>0){
      print("zero spline")
      zeroSpineModel<- zerosplineOptimized(temp)
      print("single spline")
      singleSplineModel <- singlesplineOptimized(temp,buffer)
      print("double spline")
      doubleSplineModel <- doublesplineOptimized(temp,buffer)
      
      modelIdentifierZero <- paste(county, crop, "Zero spline model", sep = "_")
      modelIdentifierSingle <- paste(county, crop, "Single spline model", sep = "_")
      modelIdentifierDouble <- paste(county, crop, "Double spline model", sep = "_")
      
      saveModels5Year[[modelIdentifierZero]] <- zeroSpineModel
      saveModels5Year[[modelIdentifierSingle]] <- singleSplineModel
      saveModels5Year[[modelIdentifierDouble]] <- doubleSplineModel
    
    }
  }
}
saveModels10Year<-list()
buffer=10
for (crop in crops){
  dfCrop<-subset(dfBig, dfBig$commodityCode == crop)
  for (county in codes) {
    print(paste(crop," in county ",county))
    temp<-subset(dfCrop, dfCrop$fullfips==county)
    if(nrow(temp)>0){
      print("zero spline")
      zeroSpineModel<- zerosplineOptimized(temp)
      print("single spline")
      singleSplineModel <- singlesplineOptimized(temp,buffer)
      print("double spline")
      doubleSplineModel <- doublesplineOptimized(temp,buffer)
      
      modelIdentifierZero <- paste(county, crop, "Zero spline model", sep = "_")
      modelIdentifierSingle <- paste(county, crop, "Single spline model", sep = "_")
      modelIdentifierDouble <- paste(county, crop, "Double spline model", sep = "_")
      
      saveModels10Year[[modelIdentifierZero]] <- zeroSpineModel
      saveModels10Year[[modelIdentifierSingle]] <- singleSplineModel
      saveModels10Year[[modelIdentifierDouble]] <- doubleSplineModel
      
    }
  }
}
saveModels15Year<-list()
buffer=15
for (crop in crops){
  dfCrop<-subset(dfBig, dfBig$commodityCode == crop)
  for (county in codes) {
    print(paste(crop," in county ",county))
    temp<-subset(dfCrop, dfCrop$fullfips==county)
    if(nrow(temp)>0){
      print("zero spline")
      zeroSpineModel<- zerosplineOptimized(temp)
      print("single spline")
      singleSplineModel <- singlesplineOptimized(temp,buffer)
      print("double spline")
      doubleSplineModel <- doublesplineOptimized(temp,buffer)
      
      modelIdentifierZero <- paste(county, crop, "Zero spline model", sep = "_")
      modelIdentifierSingle <- paste(county, crop, "Single spline model", sep = "_")
      modelIdentifierDouble <- paste(county, crop, "Double spline model", sep = "_")
      
      saveModels15Year[[modelIdentifierZero]] <- zeroSpineModel
      saveModels15Year[[modelIdentifierSingle]] <- singleSplineModel
      saveModels15Year[[modelIdentifierDouble]] <- doubleSplineModel
      
    }
  }
}

save(saveModels5Year, saveModels10Year, saveModels15Year, file = "phase3SplineModels.Rdata")



