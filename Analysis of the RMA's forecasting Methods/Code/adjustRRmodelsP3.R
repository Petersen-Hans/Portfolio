load("RiskRegionModels.Rdata")
load("countyDifference.Rdata")

counties504<-difference_list[[1]]
counties508<-difference_list[[2]]
counties766<-difference_list[[3]]
counties660<-difference_list[[4]]

forecast504_5year<-list()
forecast504_10year<-list()
forecast504_15year<-list()
j=0
for(county in counties504){
  j=j+1
  countyForecast_5Year<-list()
  countyForecast_10Year<-list()
  countyForecast_15Year<-list()
  for(i in 1:(length(county)-2)){
    countyAdjustment<-county[[i]]
    countyForecast_5Year[i]<-bestModels504_5Year[[i]]$forecastVal + countyAdjustment
    countyForecast_10Year[i]<-bestModels504_10Year[[i]]$forecastVal + countyAdjustment
    countyForecast_15Year[i]<-bestModels504_15Year[[i]]$forecastVal + countyAdjustment
  }
  forecast504_5year[[j]]<-countyForecast_5Year
  forecast504_10year[[j]]<-countyForecast_10Year
  forecast504_15year[[j]]<-countyForecast_15Year
}

names(forecast504_5year)<-names(counties504)
names(forecast504_10year)<-names(counties504)
names(forecast504_15year)<-names(counties504)

forecast508_5year<-list()
forecast508_10year<-list()
forecast508_15year<-list()
j=0
for(county in counties508){
  j=j+1
  countyForecast_5Year<-list()
  countyForecast_10Year<-list()
  countyForecast_15Year<-list()
  for(i in 1:(length(county)-2)){
    countyAdjustment<-county[[i]]
    countyForecast_5Year[i]<-bestModels508_5Year[[i]]$forecastVal + countyAdjustment
    countyForecast_10Year[i]<-bestModels508_10Year[[i]]$forecastVal + countyAdjustment
    countyForecast_15Year[i]<-bestModels508_15Year[[i]]$forecastVal + countyAdjustment
  }
  forecast508_5year[[j]]<-countyForecast_5Year
  forecast508_10year[[j]]<-countyForecast_10Year
  forecast508_15year[[j]]<-countyForecast_15Year
}

names(forecast508_5year)<-names(counties508)
names(forecast508_10year)<-names(counties508)
names(forecast508_15year)<-names(counties508)

forecast660_5year<-list()
forecast660_10year<-list()
forecast660_15year<-list()
j=0
for(county in counties660){
  j=j+1
  countyForecast_5Year<-list()
  countyForecast_10Year<-list()
  countyForecast_15Year<-list()
  for(i in 1:(length(county)-2)){
    countyAdjustment<-county[[i]]
    countyForecast_5Year[i]<-bestModels660_5Year[[i]]$forecastVal + countyAdjustment
    countyForecast_10Year[i]<-bestModels660_10Year[[i]]$forecastVal + countyAdjustment
    countyForecast_15Year[i]<-bestModels660_15Year[[i]]$forecastVal + countyAdjustment
  }
  forecast660_5year[[j]]<-countyForecast_5Year
  forecast660_10year[[j]]<-countyForecast_10Year
  forecast660_15year[[j]]<-countyForecast_15Year
}

names(forecast660_5year)<-names(counties660)
names(forecast660_10year)<-names(counties660)
names(forecast660_15year)<-names(counties660)

forecast766_5year<-list()
forecast766_10year<-list()
forecast766_15year<-list()
j=0
for(county in counties766){
  j=j+1
  countyForecast_5Year<-list()
  countyForecast_10Year<-list()
  countyForecast_15Year<-list()
  for(i in 1:(length(county)-2)){
    countyAdjustment<-county[[i]]
    countyForecast_5Year[i]<-bestModels766_5Year[[i]]$forecastVal + countyAdjustment
    countyForecast_10Year[i]<-bestModels766_10Year[[i]]$forecastVal + countyAdjustment
    countyForecast_15Year[i]<-bestModels766_15Year[[i]]$forecastVal + countyAdjustment
  }
  forecast766_5year[[j]]<-countyForecast_5Year
  forecast766_10year[[j]]<-countyForecast_10Year
  forecast766_15year[[j]]<-countyForecast_15Year
}

names(forecast766_5year)<-names(counties766)
names(forecast766_10year)<-names(counties766)
names(forecast766_15year)<-names(counties766)

save(forecast766_5year,forecast766_10year,forecast766_15year,forecast660_15year,forecast660_10year,forecast660_5year,forecast508_15year,forecast508_10year,forecast508_5year,forecast504_5year,forecast504_10year,forecast504_15year, file = "countyAdjustedPhase3Models.Rdata")
