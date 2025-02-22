library(readxl)

load("RiskRegionDoubleSplineLPModels15Year.Rdata")
load("RiskRegionDoubleSplineLPModels10Year.Rdata")
load("RiskRegionDoubleSplineLPModels.Rdata")
load("countyDifference.Rdata")

counties504<-difference_list[[1]]
counties508<-difference_list[[2]]
counties766<-difference_list[[3]]
counties660<-difference_list[[4]]

forecast504<-list()
RR504<-doubleSplineRRLP15Year[[1]]
j=0
for(county in counties504){
  j=j+1
  countyForecast<-list()
  startIndex <- max(1, length(RR504) - length(county)-30)
  for(i in startIndex:(length(county)-2)){
    countyAdjustment<-county[[i]]
    countyForecast[i]<-RR504[[i]]$forecastVal - countyAdjustment
  }
  names(countyForecast)<-names(county[1:(length(county)-2)])
  forecast504[[j]]<-countyForecast
}
names(forecast504)<-names(counties504)

forecast508<-list()
RR508<-doubleSplineRRLP15Year[[2]]
j=0
for(county in counties508){
  j=j+1
  countyForecast<-list()
  startIndex <- max(1, length(RR508) - length(county)-30)
  for(i in startIndex:(length(county)-2)){
    countyAdjustment<-county[[i]]
    countyForecast[i]<-RR508[[i]]$forecastVal - countyAdjustment
  }
  names(countyForecast)<-names(county[1:(length(county)-2)])
  forecast508[[j]]<-countyForecast
}
names(forecast508)<-names(counties508)

forecast766<-list()
RR766<-doubleSplineRRLP15Year[[3]]
j=0
for(county in counties766){
  j=j+1
  countyForecast<-list()
  startIndex <- max(1, length(RR766) - length(county)-30)
  for(i in startIndex:(length(county)-2)){
    countyAdjustment<-county[[i]]
    countyForecast[i]<-RR766[[i]]$forecastVal - countyAdjustment
  }
  names(countyForecast)<-names(county[1:(length(county)-2)])
  forecast766[[j]]<-countyForecast
}
names(forecast766)<-names(counties766)

forecast660<-list()
RR660<-doubleSplineRRLP15Year[[4]]
j=0
for(county in counties660){
  j=j+1
  countyForecast<-list()
  startIndex <- max(1, length(RR660) - length(county)-30)
  for(i in startIndex:(length(county)-2)){
    countyAdjustment<-county[[i]]
    countyForecast[i]<-RR660[[i]]$forecastVal - countyAdjustment
  }
  names(countyForecast)<-names(county[1:(length(county)-2)])
  forecast660[[j]]<-countyForecast
}
names(forecast660)<-names(counties660)
############################################################################
df504 <- read_excel("countyData_CornSoy.xlsx", sheet = "ctyData corn rr504")
df508 <- read_excel("countyData_CornSoy.xlsx", sheet = "ctyData corn rr508")
df766 <- read_excel("countyData_CornSoy.xlsx", sheet = "ctyData soy rr766")
df660 <- read_excel("countyData_CornSoy.xlsx", sheet = "ctyData soy rr660")
df660<-subset(df660,df660$CommodityYear>1983)

names504<-names(forecast504)
fips504<- sapply(names504, function(name) {
  sub("County ", "", name)
})
fips504<-as.numeric(fips504)

output504<-data.frame()

for(i in 1:length(fips504)){
  
  dfcounty<-subset(df504, df504$fullfips==fips504[i])
  tmp<-unlist(forecast504[[i]])
  
  dfcounty$forecast<-c(rep(0,31),tmp)
  dfcounty$forecastError<- dfcounty$forecast -dfcounty$dryPlYldFinal
  
  dfcounty=subset(dfcounty,  !dfcounty$forecast==0)
  
  output504<-rbind(output504, dfcounty)
}

names508<-names(forecast508)
fips508<- sapply(names508, function(name) {
  sub("County ", "", name)
})
fips508<-as.numeric(fips508)

output508<-data.frame()

for(i in 1:length(fips508)){
  
  dfcounty<-subset(df508, df508$fullfips==fips508[i])
  tmp<-unlist(forecast508[[i]])
  
  dfcounty$forecast<-c(rep(0,31),tmp)
  dfcounty$forecastError<- dfcounty$forecast - dfcounty$dryPlYldFinal
  
  dfcounty=subset(dfcounty,  !dfcounty$forecast==0)
  
  output508<-rbind(output508, dfcounty)
}

names660<-names(forecast660)
fips660<- sapply(names660, function(name) {
  sub("County ", "", name)
})
fips660<-as.numeric(fips660)

output660<-data.frame()
names(forecast660)

for(i in 1:length(fips660)){
  
  dfcounty<-subset(df660, df660$fullfips==fips660[i])
  tmp<-unlist(forecast660[[i]])
  
  dfcounty$forecast<-c(rep(0,31),tmp)
  dfcounty$forecastError<- dfcounty$forecast - dfcounty$dryPlYldFinal
  
  dfcounty=subset(dfcounty,  !dfcounty$forecast==0)
  
  output660<-rbind(output660, dfcounty)
}

names766<-names(forecast766)
fips766<- sapply(names766, function(name) {
  sub("County ", "", name)
})
fips766<-as.numeric(fips766)

output766<-data.frame()

for(i in 1:length(fips766)){
  
  dfcounty<-subset(df766, df766$fullfips==fips766[i])
  tmp<-unlist(forecast766[[i]])
  
  dfcounty$forecast<-c(rep(0,31),tmp)
  dfcounty$forecastError<- dfcounty$forecast - dfcounty$dryPlYldFinal
  
  dfcounty=subset(dfcounty,  !dfcounty$forecast==0)
  
  output766<-rbind(output766, dfcounty)
}

adjustedRRLPmodels15Year<-rbind(output504,output508,output660,output766)
save(adjustedRRLPmodels15Year, file = "adjustedRRLPmodels15Year.Rdata")

forecast504<-list()
RR504<-doubleSplineRRLP10Year[[1]]
j=0
for(county in counties504){
  j=j+1
  countyForecast<-list()
  startIndex <- max(1, length(RR504) - length(county)-30)
  for(i in startIndex:(length(county)-2)){
    countyAdjustment<-county[[i]]
    countyForecast[i]<-RR504[[i]]$forecastVal - countyAdjustment
  }
  names(countyForecast)<-names(county[1:(length(county)-2)])
  forecast504[[j]]<-countyForecast
}
names(forecast504)<-names(counties504)

forecast508<-list()
RR508<-doubleSplineRRLP10Year[[2]]
j=0
for(county in counties508){
  j=j+1
  countyForecast<-list()
  startIndex <- max(1, length(RR508) - length(county)-30)
  for(i in startIndex:(length(county)-2)){
    countyAdjustment<-county[[i]]
    countyForecast[i]<-RR508[[i]]$forecastVal - countyAdjustment
  }
  names(countyForecast)<-names(county[1:(length(county)-2)])
  forecast508[[j]]<-countyForecast
}
names(forecast508)<-names(counties508)

forecast766<-list()
RR766<-doubleSplineRRLP10Year[[3]]
j=0
for(county in counties766){
  j=j+1
  countyForecast<-list()
  startIndex <- max(1, length(RR766) - length(county)-30)
  for(i in startIndex:(length(county)-2)){
    countyAdjustment<-county[[i]]
    countyForecast[i]<-RR766[[i]]$forecastVal - countyAdjustment
  }
  names(countyForecast)<-names(county[1:(length(county)-2)])
  forecast766[[j]]<-countyForecast
}
names(forecast766)<-names(counties766)

forecast660<-list()
RR660<-doubleSplineRRLP10Year[[4]]
j=0
for(county in counties660){
  j=j+1
  countyForecast<-list()
  startIndex <- max(1, length(RR660) - length(county)-30)
  for(i in startIndex:(length(county)-2)){
    countyAdjustment<-county[[i]]
    countyForecast[i]<-RR660[[i]]$forecastVal - countyAdjustment
  }
  names(countyForecast)<-names(county[1:(length(county)-2)])
  forecast660[[j]]<-countyForecast
}
names(forecast660)<-names(counties660)
############################################################################
df504 <- read_excel("countyData_CornSoy.xlsx", sheet = "ctyData corn rr504")
df508 <- read_excel("countyData_CornSoy.xlsx", sheet = "ctyData corn rr508")
df766 <- read_excel("countyData_CornSoy.xlsx", sheet = "ctyData soy rr766")
df660 <- read_excel("countyData_CornSoy.xlsx", sheet = "ctyData soy rr660")
df660<-subset(df660,df660$CommodityYear>1983)

names504<-names(forecast504)
fips504<- sapply(names504, function(name) {
  sub("County ", "", name)
})
fips504<-as.numeric(fips504)

output504<-data.frame()

for(i in 1:length(fips504)){
  
  dfcounty<-subset(df504, df504$fullfips==fips504[i])
  tmp<-unlist(forecast504[[i]])
  
  dfcounty$forecast<-c(rep(0,31),tmp)
  dfcounty$forecastError<- dfcounty$forecast -dfcounty$dryPlYldFinal
  
  dfcounty=subset(dfcounty,  !dfcounty$forecast==0)
  
  output504<-rbind(output504, dfcounty)
}

names508<-names(forecast508)
fips508<- sapply(names508, function(name) {
  sub("County ", "", name)
})
fips508<-as.numeric(fips508)

output508<-data.frame()

for(i in 1:length(fips508)){
  
  dfcounty<-subset(df508, df508$fullfips==fips508[i])
  tmp<-unlist(forecast508[[i]])
  
  dfcounty$forecast<-c(rep(0,31),tmp)
  dfcounty$forecastError<- dfcounty$forecast - dfcounty$dryPlYldFinal
  
  dfcounty=subset(dfcounty,  !dfcounty$forecast==0)
  
  output508<-rbind(output508, dfcounty)
}

names660<-names(forecast660)
fips660<- sapply(names660, function(name) {
  sub("County ", "", name)
})
fips660<-as.numeric(fips660)

output660<-data.frame()
names(forecast660)

for(i in 1:length(fips660)){
  
  dfcounty<-subset(df660, df660$fullfips==fips660[i])
  tmp<-unlist(forecast660[[i]])
  
  dfcounty$forecast<-c(rep(0,31),tmp)
  dfcounty$forecastError<- dfcounty$forecast - dfcounty$dryPlYldFinal
  
  dfcounty=subset(dfcounty,  !dfcounty$forecast==0)
  
  output660<-rbind(output660, dfcounty)
}

names766<-names(forecast766)
fips766<- sapply(names766, function(name) {
  sub("County ", "", name)
})
fips766<-as.numeric(fips766)

output766<-data.frame()

for(i in 1:length(fips766)){
  
  dfcounty<-subset(df766, df766$fullfips==fips766[i])
  tmp<-unlist(forecast766[[i]])
  
  dfcounty$forecast<-c(rep(0,31),tmp)
  dfcounty$forecastError<- dfcounty$forecast - dfcounty$dryPlYldFinal
  
  dfcounty=subset(dfcounty,  !dfcounty$forecast==0)
  
  output766<-rbind(output766, dfcounty)
}

adjustedRRLPmodels10Year<-rbind(output504,output508,output660,output766)
save(adjustedRRLPmodels10Year, file = "adjustedRRLPmodels10Year.Rdata")

forecast504<-list()
RR504<-doubleSplineRRLP[[1]]
j=0
for(county in counties504){
  j=j+1
  countyForecast<-list()
  startIndex <- max(1, length(RR504) - length(county)-30)
  for(i in startIndex:(length(county)-2)){
    countyAdjustment<-county[[i]]
    countyForecast[i]<-RR504[[i]]$forecastVal - countyAdjustment
  }
  names(countyForecast)<-names(county[1:(length(county)-2)])
  forecast504[[j]]<-countyForecast
}
names(forecast504)<-names(counties504)

forecast508<-list()
RR508<-doubleSplineRRLP[[2]]
j=0
for(county in counties508){
  j=j+1
  countyForecast<-list()
  startIndex <- max(1, length(RR508) - length(county)-30)
  for(i in startIndex:(length(county)-2)){
    countyAdjustment<-county[[i]]
    countyForecast[i]<-RR508[[i]]$forecastVal - countyAdjustment
  }
  names(countyForecast)<-names(county[1:(length(county)-2)])
  forecast508[[j]]<-countyForecast
}
names(forecast508)<-names(counties508)

forecast766<-list()
RR766<-doubleSplineRRLP[[3]]
j=0
for(county in counties766){
  j=j+1
  countyForecast<-list()
  startIndex <- max(1, length(RR766) - length(county)-30)
  for(i in startIndex:(length(county)-2)){
    countyAdjustment<-county[[i]]
    countyForecast[i]<-RR766[[i]]$forecastVal - countyAdjustment
  }
  names(countyForecast)<-names(county[1:(length(county)-2)])
  forecast766[[j]]<-countyForecast
}
names(forecast766)<-names(counties766)

forecast660<-list()
RR660<-doubleSplineRRLP[[4]]
j=0
for(county in counties660){
  j=j+1
  countyForecast<-list()
  startIndex <- max(1, length(RR660) - length(county)-30)
  for(i in startIndex:(length(county)-2)){
    countyAdjustment<-county[[i]]
    countyForecast[i]<-RR660[[i]]$forecastVal - countyAdjustment
  }
  names(countyForecast)<-names(county[1:(length(county)-2)])
  forecast660[[j]]<-countyForecast
}
names(forecast660)<-names(counties660)
############################################################################
df504 <- read_excel("countyData_CornSoy.xlsx", sheet = "ctyData corn rr504")
df508 <- read_excel("countyData_CornSoy.xlsx", sheet = "ctyData corn rr508")
df766 <- read_excel("countyData_CornSoy.xlsx", sheet = "ctyData soy rr766")
df660 <- read_excel("countyData_CornSoy.xlsx", sheet = "ctyData soy rr660")
df660<-subset(df660,df660$CommodityYear>1983)

names504<-names(forecast504)
fips504<- sapply(names504, function(name) {
  sub("County ", "", name)
})
fips504<-as.numeric(fips504)

output504<-data.frame()

for(i in 1:length(fips504)){
  
  dfcounty<-subset(df504, df504$fullfips==fips504[i])
  tmp<-unlist(forecast504[[i]])
  
  dfcounty$forecast<-c(rep(0,31),tmp)
  dfcounty$forecastError<- dfcounty$forecast -dfcounty$dryPlYldFinal
  
  dfcounty=subset(dfcounty,  !dfcounty$forecast==0)
  
  output504<-rbind(output504, dfcounty)
}

names508<-names(forecast508)
fips508<- sapply(names508, function(name) {
  sub("County ", "", name)
})
fips508<-as.numeric(fips508)

output508<-data.frame()

for(i in 1:length(fips508)){
  
  dfcounty<-subset(df508, df508$fullfips==fips508[i])
  tmp<-unlist(forecast508[[i]])
  
  dfcounty$forecast<-c(rep(0,31),tmp)
  dfcounty$forecastError<- dfcounty$forecast - dfcounty$dryPlYldFinal
  
  dfcounty=subset(dfcounty,  !dfcounty$forecast==0)
  
  output508<-rbind(output508, dfcounty)
}

names660<-names(forecast660)
fips660<- sapply(names660, function(name) {
  sub("County ", "", name)
})
fips660<-as.numeric(fips660)

output660<-data.frame()
names(forecast660)

for(i in 1:length(fips660)){
  
  dfcounty<-subset(df660, df660$fullfips==fips660[i])
  tmp<-unlist(forecast660[[i]])
  
  dfcounty$forecast<-c(rep(0,31),tmp)
  dfcounty$forecastError<- dfcounty$forecast - dfcounty$dryPlYldFinal
  
  dfcounty=subset(dfcounty,  !dfcounty$forecast==0)
  
  output660<-rbind(output660, dfcounty)
}

names766<-names(forecast766)
fips766<- sapply(names766, function(name) {
  sub("County ", "", name)
})
fips766<-as.numeric(fips766)

output766<-data.frame()

for(i in 1:length(fips766)){
  
  dfcounty<-subset(df766, df766$fullfips==fips766[i])
  tmp<-unlist(forecast766[[i]])
  
  dfcounty$forecast<-c(rep(0,31),tmp)
  dfcounty$forecastError<- dfcounty$forecast - dfcounty$dryPlYldFinal
  
  dfcounty=subset(dfcounty,  !dfcounty$forecast==0)
  
  output766<-rbind(output766, dfcounty)
}

adjustedRRLPmodels<-rbind(output504,output508,output660,output766)
save(adjustedRRLPmodels, file = "adjustedRRLPmodels.Rdata")
