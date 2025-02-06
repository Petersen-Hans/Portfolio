rm(list = ls())
#setwd("C:/Users/17203/OneDrive - Montana State University/Documents/403/research project")
library(tidyverse)
library(stringr)
library(fastDummies)
library(rvest)
load("aggregatedCrashData.Rdata")
load("GLClocationCodes.Rdata")


# boroughcodes<-c(005,047,061,081,085)#fips code for counties of interest
# #apply treated variable to variable of interest
# crash$TREATED<-ifelse(crash$STATE == 36 & crash$COUNTY %in% boroughcodes,1,0)

crash$post<-ifelse(crash$YEAR>=2014, 1,0)
#create weather dummy variables
crash<-dummy_cols(crash, select_columns = "WEATHER", remove_first_dummy = F)
#I will be adding the unreported columns to the clear column because severe weather conditions that would have influenced crashes is more likely to be reported than weather that would have little effect.
crash$WEATHER_1<-ifelse(crash$WEATHER_1+crash$WEATHER_98+crash$WEATHER_98+crash$WEATHER_9>=1, 1,0)
crash<-select(crash, -c("WEATHER_9","WEATHER_98","WEATHER_99"))

weatherNums<-colnames(crash)[45:55]
weatherNames<-c("CLEAR","RAIN","HAIL","SNOW","FOG","HIGHWINDS","BLOWINGSEDIMENT","OTHER","CLOUDY","BLOWINGSNOW","FREEZINGRAIN")
crash<-crash|>rename_at(vars(weatherNums),~weatherNames)
#########################
#reference table for weathers
# 1 Clear
# 2 Rain
# 3 Sleet or Hail
# 4 Snow
# 5 Fog, Smog, Smoke
# 6 Severe Crosswinds
# 7 Blowing Sand, Soil, Dirt
# 8 Other
# 10 Cloudy
# 11 Blowing Snow
# 12 Freezing Rain or Drizzle
##################

summarizedCrash<- crash|>
  group_by(STATE, COUNTY, YEAR)|>
  summarise(
    totalCrashes = n(),#number of crashes
    fatalities = sum(FATALS),#number of fatalities
    people = sum(PERSONS),#total people involved in accident
    #Percentage of crashes that had certain weather condition
    clear = sum(CLEAR)/n(),
    rain = sum(RAIN)/n(),
    hail = sum(HAIL)/n(),
    snow = sum(SNOW)/n(),
    fog = sum(FOG)/n(),
    highWinds = sum(HIGHWINDS)/n(),
    blowingSediment = sum(BLOWINGSEDIMENT)/n(),
    other = sum(OTHER)/n(),
    cloudy = sum(CLOUDY)/n(),
    blowingSnow = sum(BLOWINGSNOW)/n(),
    freezingrain = sum(FREEZINGRAIN)/n(),
    
    drunkDriver = sum(DRUNK_DR)/sum(VE_TOTAL)#Percent of drivers in crashes under the influence
     )
#clean GLC Codes in order to merge names of the counties to data
locationCodes$`State Code`<-str_pad(locationCodes$`State Code`, width = 2, pad = "0")
locationCodes$`County Code`<-str_pad(locationCodes$`County Code`, width = 3, pad = "0")
stateCounty<-as.data.frame(cbind("code"=paste0(locationCodes$`State Code`,locationCodes$`County Code`), "county"=paste(locationCodes$`County Name`, locationCodes$`State Name`)))
stateCounty<-unique(stateCounty)

summarizedCrash$STATE<-str_pad(summarizedCrash$STATE, width = 2, pad = "0")
summarizedCrash$COUNTY<-str_pad(summarizedCrash$COUNTY, width = 3, pad = "0")
summarizedCrash$code<-paste0(summarizedCrash$STATE, summarizedCrash$COUNTY)

summarizedCrash<-summarizedCrash|>left_join(stateCounty, by = "code")
summarizedCrash<-summarizedCrash|>rename_at("county",~"location")

boroughsTemp<-unique(subset(locationCodes, locationCodes$`County Name` %in% c("BRONX", "KINGS", "NEW YORK", "QUEENS", "RICHMOND") & locationCodes$`State Name`=="NEW YORK"))
boroughs<-as.data.frame(cbind("code"=paste0(boroughsTemp$`State Code`,boroughsTemp$`County Code`), "county"=paste(boroughsTemp$`County Name`, boroughsTemp$`State Name`)))

summarizedCrash$treated<-ifelse(summarizedCrash$code %in% boroughs$code,1,0)

x11()
ggplot(summarizedCrash, aes(x=YEAR, y = (fatalities), group = location))+geom_line(aes(color = treated))+theme(legend.position = "none")

summarizedCrash<-dummy_cols(summarizedCrash, select_columns = "YEAR")
# summarizedCrash$YEAR_2009<- summarizedCrash$treated * summarizedCrash$YEAR_2009
# summarizedCrash$YEAR_2010<- summarizedCrash$treated * summarizedCrash$YEAR_2010
# summarizedCrash$YEAR_2011<- summarizedCrash$treated * summarizedCrash$YEAR_2011
# summarizedCrash$YEAR_2012<- summarizedCrash$treated * summarizedCrash$YEAR_2012
# summarizedCrash$YEAR_2013<- summarizedCrash$treated * summarizedCrash$YEAR_2013
# summarizedCrash$YEAR_2014<- summarizedCrash$treated * summarizedCrash$YEAR_2014
# summarizedCrash$YEAR_2015<- summarizedCrash$treated * summarizedCrash$YEAR_2015
# summarizedCrash$YEAR_2016<- summarizedCrash$treated * summarizedCrash$YEAR_2016
# summarizedCrash$YEAR_2017<- summarizedCrash$treated * summarizedCrash$YEAR_2017
# summarizedCrash$YEAR_2018<- summarizedCrash$treated * summarizedCrash$YEAR_2018
# summarizedCrash$YEAR_2019<- summarizedCrash$treated * summarizedCrash$YEAR_2019


url = "https://en.wikipedia.org/wiki/List_of_the_most_populous_counties_in_the_United_States"
page = read_html(url)

tables = page |> html_elements("table")
tables

LargestCounties <- (tables[[1]] |> html_table())
LargestCounties <- LargestCounties[-1,]
countySet<-as.data.frame(paste(toupper(LargestCounties$County), toupper(LargestCounties$State)))
names(countySet)<-"name"
countySet<-rbind(countySet, "RICHMOND NEW YORK")

#remove upper outlier
outlier<-summarizedCrash$location[which.max(summarizedCrash$fatalities)]
summarizedCrash<-summarizedCrash|>filter(location!=outlier)
x11()
ggplot(summarizedCrash, aes(x=YEAR, y = (fatalities), group = location))+geom_line(aes(color = treated))+geom_smooth(aes(group = treated))


sampleOfCrash<-subset(summarizedCrash, summarizedCrash$location %in% countySet$name)

finalData<-sampleOfCrash
save(finalData, file = "finalData.Rdata")
