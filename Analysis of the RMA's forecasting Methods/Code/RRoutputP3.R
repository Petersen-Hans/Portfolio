rm(list = ls())

load("countyAdjustedPhase3Models.Rdata")
library(openxlsx)
library(readxl)

df504 <- read_excel("countyData_CornSoy.xlsx", sheet = "ctyData corn rr504")
df508 <- read_excel("countyData_CornSoy.xlsx", sheet = "ctyData corn rr508")
df766 <- read_excel("countyData_CornSoy.xlsx", sheet = "ctyData soy rr766")
df660 <- read_excel("countyData_CornSoy.xlsx", sheet = "ctyData soy rr660")
df660<-subset(df660,df660$CommodityYear>1983)

names504<-names(forecast504_5year)
fips504<- sapply(names504, function(name) {
  sub("County ", "", name)
})
fips504<-as.numeric(fips504)

output504_5Year<-data.frame()
output504_10Year<-data.frame()
output504_15Year<-data.frame()

for(i in 1:length(fips504)){
  
  dfcounty<-subset(df504, df504$fullfips==fips504[i])
  fore_5<-unlist(forecast504_5year[[i]])
  dfcounty$forecast<-c(rep(0,31),fore_5)
  dfcounty$forecastError<- dfcounty$forecast -dfcounty$dryPlYldFinal
  dfcounty=subset(dfcounty,  !dfcounty$forecast==0)
  output504_5Year<-rbind(output504_5Year, dfcounty)
  
  dfcounty<-subset(df504, df504$fullfips==fips504[i])
  fore_10<-unlist(forecast504_10year[[i]])
  dfcounty$forecast<-c(rep(0,31),fore_10)
  dfcounty$forecastError<- dfcounty$forecast -dfcounty$dryPlYldFinal
  dfcounty=subset(dfcounty,  !dfcounty$forecast==0)
  output504_10Year<-rbind(output504_10Year, dfcounty)
  
  dfcounty<-subset(df504, df504$fullfips==fips504[i])
  fore_15<-unlist(forecast504_15year[[i]])
  dfcounty$forecast<-c(rep(0,31),fore_15)
  dfcounty$forecastError<- dfcounty$forecast -dfcounty$dryPlYldFinal
  dfcounty=subset(dfcounty,  !dfcounty$forecast==0)
  output504_15Year<-rbind(output504_15Year, dfcounty)
}


names508<-names(forecast508_5year)
fips508<- sapply(names508, function(name) {
  sub("County ", "", name)
})
fips508<-as.numeric(fips508)

output508_5Year<-data.frame()
output508_10Year<-data.frame()
output508_15Year<-data.frame()

for(i in 1:length(fips508)){
  
  dfcounty<-subset(df508, df508$fullfips==fips508[i])
  fore_5<-unlist(forecast508_5year[[i]])
  dfcounty$forecast<-c(rep(0,31),fore_5)
  dfcounty$forecastError<- dfcounty$forecast -dfcounty$dryPlYldFinal
  dfcounty=subset(dfcounty,  !dfcounty$forecast==0)
  output508_5Year<-rbind(output508_5Year, dfcounty)
  
  dfcounty<-subset(df508, df508$fullfips==fips508[i])
  fore_10<-unlist(forecast508_10year[[i]])
  dfcounty$forecast<-c(rep(0,31),fore_10)
  dfcounty$forecastError<- dfcounty$forecast -dfcounty$dryPlYldFinal
  dfcounty=subset(dfcounty,  !dfcounty$forecast==0)
  output508_10Year<-rbind(output508_10Year, dfcounty)
  
  dfcounty<-subset(df508, df508$fullfips==fips508[i])
  fore_15<-unlist(forecast508_15year[[i]])
  dfcounty$forecast<-c(rep(0,31),fore_15)
  dfcounty$forecastError<- dfcounty$forecast -dfcounty$dryPlYldFinal
  dfcounty=subset(dfcounty,  !dfcounty$forecast==0)
  output508_15Year<-rbind(output508_15Year, dfcounty)
}


names660<-names(forecast660_5year)
fips660<- sapply(names660, function(name) {
  sub("County ", "", name)
})
fips660<-as.numeric(fips660)

output660_5Year<-data.frame()
output660_10Year<-data.frame()
output660_15Year<-data.frame()

for(i in 1:length(fips660)){
  
  dfcounty<-subset(df660, df660$fullfips==fips660[i])
  fore_5<-unlist(forecast660_5year[[i]])
  dfcounty$forecast<-c(rep(0,31),fore_5)
  dfcounty$forecastError<- dfcounty$forecast -dfcounty$dryPlYldFinal
  dfcounty=subset(dfcounty,  !dfcounty$forecast==0)
  output660_5Year<-rbind(output660_5Year, dfcounty)
  
  dfcounty<-subset(df660, df660$fullfips==fips660[i])
  fore_10<-unlist(forecast660_10year[[i]])
  dfcounty$forecast<-c(rep(0,31),fore_10)
  dfcounty$forecastError<- dfcounty$forecast -dfcounty$dryPlYldFinal
  dfcounty=subset(dfcounty,  !dfcounty$forecast==0)
  output660_10Year<-rbind(output660_10Year, dfcounty)
  
  dfcounty<-subset(df660, df660$fullfips==fips660[i])
  fore_15<-unlist(forecast660_15year[[i]])
  dfcounty$forecast<-c(rep(0,31),fore_15)
  dfcounty$forecastError<- dfcounty$forecast -dfcounty$dryPlYldFinal
  dfcounty=subset(dfcounty,  !dfcounty$forecast==0)
  output660_15Year<-rbind(output660_15Year, dfcounty)
}


names766<-names(forecast766_5year)
fips766<- sapply(names766, function(name) {
  sub("County ", "", name)
})
fips766<-as.numeric(fips766)

output766_5Year<-data.frame()
output766_10Year<-data.frame()
output766_15Year<-data.frame()

for(i in 1:length(fips766)){
  
  dfcounty<-subset(df766, df766$fullfips==fips766[i])
  fore_5<-unlist(forecast766_5year[[i]])
  dfcounty$forecast<-c(rep(0,31),fore_5)
  dfcounty$forecastError<- dfcounty$forecast -dfcounty$dryPlYldFinal
  dfcounty=subset(dfcounty,  !dfcounty$forecast==0)
  output766_5Year<-rbind(output766_5Year, dfcounty)
  
  dfcounty<-subset(df766, df766$fullfips==fips766[i])
  fore_10<-unlist(forecast766_10year[[i]])
  dfcounty$forecast<-c(rep(0,31),fore_10)
  dfcounty$forecastError<- dfcounty$forecast -dfcounty$dryPlYldFinal
  dfcounty=subset(dfcounty,  !dfcounty$forecast==0)
  output766_10Year<-rbind(output766_10Year, dfcounty)
  
  dfcounty<-subset(df766, df766$fullfips==fips766[i])
  fore_15<-unlist(forecast766_15year[[i]])
  dfcounty$forecast<-c(rep(0,31),fore_15)
  dfcounty$forecastError<- dfcounty$forecast -dfcounty$dryPlYldFinal
  dfcounty=subset(dfcounty,  !dfcounty$forecast==0)
  output766_15Year<-rbind(output766_15Year, dfcounty)
}

riskregiondf_5Year<-rbind(output504_5Year,output508_5Year,output660_5Year,output766_5Year)
riskregiondf_10Year<-rbind(output504_10Year,output508_10Year,output660_10Year,output766_10Year)
riskregiondf_15Year<-rbind(output504_15Year,output508_15Year,output660_15Year,output766_15Year)

save(riskregiondf_5Year, riskregiondf_10Year, riskregiondf_15Year, file = "W&AforecastP3.Rdata")
