rm(list = ls())
library(tidyverse)

crash2009<-read_csv("rawData/FARS2009NationalCSV/ACCIDENT.csv")
crash2010<-read_csv("rawData/FARS2010NationalCSV/ACCIDENT.csv")
crash2011<-read_csv("rawData/FARS2011NationalCSV/ACCIDENT.csv")
crash2012<-read_csv("rawData/FARS2012NationalCSV/ACCIDENT.csv")
crash2013<-read_csv("rawData/FARS2013NationalCSV/ACCIDENT.csv")
crash2014<-read_csv("rawData/FARS2014NationalCSV/ACCIDENT.csv")
crash2015<-read_csv("rawData/FARS2015NationalCSV/ACCIDENT.csv")
crash2016<-read_csv("rawData/FARS2016NationalCSV/ACCIDENT.csv")
crash2017<-read_csv("rawData/FARS2017NationalCSV/ACCIDENT.csv")
crash2018<-read_csv("rawData/FARS2018NationalCSV/ACCIDENT.csv")
crash2019<-read_csv("rawData/FARS2019NationalCSV/ACCIDENT.csv")

colCleaning<-cbind(ncol(crash2009), ncol(crash2010), ncol(crash2011), ncol(crash2012), ncol(crash2013), ncol(crash2014), ncol(crash2015), ncol(crash2016), ncol(crash2017), ncol(crash2018), ncol(crash2019))
which.min(colCleaning) #2010 has the fewest columns, we need all columns to be the same in all datasets so this will be our base

keepCols<-colnames(crash2010)
crashSet<-list(crash2009, crash2010, crash2011, crash2012, crash2013, crash2014, crash2015, crash2016, crash2017, crash2018, crash2019)

dropExtraCols<-function(df, columns){
  commonCols <- columns[columns %in% names(df)]
  df[, commonCols, drop=TRUE]
}#end function

crashSet<-lapply(crashSet, dropExtraCols, columns = keepCols)

#we have to do this again for 2009 which is now missing a couple columns
crashSet<-lapply(crashSet, dropExtraCols, columns = colnames(crashSet[[1]]))
#one more time for one column not collected after 2015
crashSet<-lapply(crashSet, dropExtraCols, columns = colnames(crashSet[[9]]))

crash<-bind_rows(crashSet)#bind final dataset

save(crash, file = "aggregatedCrashData.Rdata")

locationCodes<-readxl::read_xlsx("rawData/FRPP_GLC_-_United_States_November_02_2023.xlsx")

locationCodes<-select(locationCodes, c("State Name","State Code","County Name", "County Code"))

save(locationCodes, file="GLClocationCodes.Rdata")


#######################DEPRICATED###############################
# accident<-read_csv("FARS2009NationalCSV/ACCIDENT.csv")
# MIACC<-read_csv("FARS2009NationalCSV/MIACC.csv")
# #MIDRVACC<-read_csv("FARS2009NationalCSV/MIDRVACC.csv")
# MIPER<-read_csv("FARS2009NationalCSV/MIPER.csv")
# PERSON<-read_csv("FARS2009NationalCSV/PERSON.csv")
# VEHICLE<-read_csv("FARS2009NationalCSV/VEHICLE.csv")
# #VEHNIT<-read_csv("FARS2009NationalCSV/VEHNIT.csv")
# #vpicdecode<-read_csv("FARS2009NationalCSV/vpicdecode.csv")

# t<-cbind(c(colnames(crashSet[[1]])), c(colnames(crashSet[[9]])))
# t<-as.data.frame(t)
# t$bool<-ifelse(t$V1 %in% t$V2, TRUE, FALSE)
# 
# 
# dropped<-(colnames(crash2019) %in% colnames(crashSet[[9]]))
# tmp<-subset(colnames(crash2019), !dropped)
# tmp