rm(list = ls())
library("tidyverse")
library("ggplot2")
library("tseries")
library(dplyr)
library("plm")

setwd("C:/Users/17203/Documents/hedgeProject")

df1<-read.csv(file = "Barclay_Return_Vector.csv")
df1$Dates<-seq(as.Date("1997-01-01"), as.Date("2022-12-01"), by="months")
v<-read.csv("S&p500_Return_Vector.csv")
df1$SnP500<-v$S.P500
###########################################################################
summary(df1$Barclay)
summary(df1$SnP500)

nova<-c(df1$Barclay)
label<-rep("Berclay",length(nova))
#nova<-cbind(nova, label)
nova2<-c(df1$SnP500)
label2<-rep("SnP500",length(nova2))
#nova2<-cbind(nova2,label)
Returns<-c(nova,nova2)
Fund<-c(label,label2)
dfnova<-cbind.data.frame(Returns,Fund)
ggplot(data = dfnova, mapping = aes(x=Returns, y= Fund))+geom_boxplot()

summary(aov(dfnova$Returns~factor(dfnova$Fund)))

ts.plot(df1$Barclay)
adf.test(df1$Barclay)

ts.plot(df1$SnP500)
adf.test(df1$SnP500)


var(df1$Barclay)
var(df1$SnP500)

df1$BarclayDollar<-100*cumprod(1+df1$Barclay)
df1$SnPDollar<-100*cumprod(1+df1$SnP500)

df1$BarclayDollar[length(df1$BarclayDollar)]
df1$SnPDollar[length(df1$SnPDollar)]

ts.plot(df1$BarclayDollar)
ts.plot(df1$SnPDollar)


df1$greatRecession<- ifelse(df1$Dates > "2007-12-01" & df1$Dates <= "2009-06-01",1,0)
df1$Recession2000<- ifelse(df1$Dates > "2001-03-01" & df1$Dates <= "2001-11-01",1,0)
df1$covidRecession<- ifelse(df1$Dates > "2020-02-01" & df1$Dates <= "2020-04-01",1,0)


#############################################################################
lag <- function(x, k) {
  if (k>0) {
    return (c(rep(NA, k), x)[1 : length(x)] );
  }
  else {
    return (c(x[(-k+1) : length(x)], rep(NA, -k)));
  }
}
#############################################################################

adf.test(df1$BarclayDollar)
barclaylag<-lag(df1$BarclayDollar, 1)
SnPlag<-lag(df1$SnPDollar,1)


barclayFD<-df1$BarclayDollar-barclaylag
summary(lm(df1$BarclayDollar~barclaylag))
fe<-plm(df1$BarclayDollar~barclaylag,index() )

summary(lm(df1$SnPDollar~SnPlag))
summary(lm(df1$BarclayDollar~barclaylag+df1$covidRecession*barclaylag+df1$Recession2000*barclaylag+df1$greatRecession*barclaylag))
summary(lm(df1$SnPDollar~SnPlag+df1$covidRecession*SnPlag))

ts.plot(barclayFD)


spFD<-df1$SnP500-SnPlag
ts.plot(spFD)

summary(lm(df1$Barclay~barclayFD))
summary(lm())
lm(df1$SnP500~spFD)
