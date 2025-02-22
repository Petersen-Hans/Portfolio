#############################################################################
#Prep workspace
rm(list = ls())
library("tidyverse")
library("ggplot2")
library("tseries")
library("dplyr")
library("psych")
library(lmtest)
setwd("C:/Users/17203/Documents/hedgeProject")
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

#Pull data
df1<-read.csv(file = "Barclay_Return_Vector.csv")
df1$Dates<-seq(as.Date("1997-01-01"), as.Date("2022-12-01"), by="months")
v<-read.csv("S&p500_Return_Vector.csv")
df1$SnP500<-v$S.P500
v<-read.csv("VDIGX.csv")
df1$VDIGX<-v$VDIGX
v<-read.csv("PGSGX.csv")
df1$PGSGX<-v$PGSGX
v<-read.csv("FDVLX.csv")
df1$FDVLX<-v$FDVLX





# v<-read.csv(("FRED10.csv"))
# df1$RF<-v$RF
# 
# dfRF<-cbind.data.frame(df1$Barclay-df1$RF,df1$VDIGX-df1$RF,df1$PGSGX-df1$RF,df1$FDVLX-df1$RF,df1$SnP500-df1$RF)
# mean(df1$Barclay-df1$RF)

#Create recession variables
df1$greatRecession<- ifelse(df1$Dates > "2007-12-01" & df1$Dates <= "2009-06-01",1,0)
df1$Recession2000<- ifelse(df1$Dates > "2001-03-01" & df1$Dates <= "2001-11-01",1,0)
df1$covidRecession<- ifelse(df1$Dates > "2020-01-01" & df1$Dates <= "2020-03-01",1,0)
#############################################################################
#Calulate dollar value for HPR
df1$BarclayDollar<-1*cumprod(1+df1$Barclay)
df1$SnPDollar<-1*cumprod(1+df1$SnP500)
df1$VDIGXDollar<-1*cumprod(1+df1$VDIGX)
df1$PGSGXDollar<-1*cumprod(1+df1$PGSGX)
df1$FDVLXDollar<-1*cumprod(1+df1$FDVLX)



#############################################################################
#Statistical prep
BarclayStat<-c(mean(df1$Barclay),geometric.mean(df1$Barclay+1)-1, var(df1$Barclay),(geometric.mean(df1$Barclay+1)-1)/var(df1$Barclay)^.5, df1$BarclayDollar[312]-1)
VDIGXStat<-c(mean(df1$VDIGX),geometric.mean(df1$VDIGX+1)-1, var(df1$VDIGX),(geometric.mean(df1$VDIGX+1)-1)/var(df1$VDIGX)^.5,df1$VDIGXDollar[312]-1)
PGSGXStat<-c(mean(df1$PGSGX),geometric.mean(df1$PGSGX+1)-1, var(df1$PGSGX),(geometric.mean(df1$PGSGX+1)-1)/var(df1$PGSGX)^.5,df1$PGSGXDollar[312]-1)
FDVLXStat<-c(mean(df1$FDVLX),geometric.mean(df1$FDVLX+1)-1, var(df1$FDVLX),(geometric.mean(df1$FDVLX+1)-1)/var(df1$FDVLX)^.5,df1$FDVLXDollar[312]-1)
SnPStat<-c(mean(df1$SnP500),geometric.mean(df1$SnP500+1)-1, var(df1$SnP500),(geometric.mean(df1$SnP500+1)-1)/var(df1$SnP500)^.5,df1$SnPDollar[312]-1)
type<-c("mean", "geomean", "var", "sharpe", "HPR")

dfStat<-cbind.data.frame(type,BarclayStat,VDIGXStat,PGSGXStat, FDVLXStat, SnPStat  )


#############################################################################
#ANOVA TEST
Returns<-c(df1$Barclay, df1$VDIGX, df1$PGSGX, df1$FDVLX, df1$SnP500)
label<-rep("Berclay",312)
label2<-rep("VDIGX",312)
label3<-rep("PGSGX",312)
label4<-rep("FDVLX",312)
label5<-rep("SnP500",312)
Fund<-c(label,label2,label3,label4,label5)
df2<-cbind.data.frame(Returns, Fund)

ggplot(data = df2, mapping = aes(x=Returns, y= Fund, fill=Fund))+geom_boxplot()

summary(aov(df2$Returns~df2$Fund))


mean(df1$Barclay)
mean(df1$VDIGX)
mean(df1$PGSGX)
mean(df1$FDVLX)
mean(df1$SnP500)

geometric.mean(df1$Barclay+1)-1
geometric.mean(df1$VDIGX+1)-1
geometric.mean(df1$PGSGX+1)-1
geometric.mean(df1$FDVLX+1)-1
geometric.mean(df1$SnP500+1)-1
#############################################################################

Returns<-c(df1$BarclayDollar, df1$VDIGXDollar, df1$PGSGXDollar, df1$FDVLXDollar, df1$SnPDollar)
label<-rep("Berclay",312)
label2<-rep("VDIGX",312)
label3<-rep("PGSGX",312)
label4<-rep("FDVLX",312)
label5<-rep("SnP500",312)
Fund<-c(label,label2,label3,label4,label5)
d<-df1$Dates
df7<-cbind.data.frame(Returns, Fund, d)


growthplot<-ggplot(df7, aes(x=d, y=Returns, color=Fund))+geom_line()+xlab("")
growthplot




BarclayTS <- ggplot(df1, aes(x=Dates, y=Barclay)) +
  geom_line() + 
  xlab("")
BarclayTS

SnPTS <- ggplot(df1, aes(x=Dates, y=SnP500)) +
  geom_line() + 
  xlab("")
SnPTS

VDIGXTS <- ggplot(df1, aes(x=Dates, y=VDIGX)) +
  geom_line() + 
  xlab("")
VDIGXTS

PGSGXTS <- ggplot(df1, aes(x=Dates, y=PGSGX)) +
  geom_line() + 
  xlab("")
PGSGXTS

FDVLXTS <- ggplot(df1, aes(x=Dates, y=FDVLX)) +
  geom_line() + 
  xlab("")
FDVLXTS


Barclayreg<-(lm(Barclay~SnP500+SnP500*greatRecession+SnP500*Recession2000+SnP500*covidRecession-1, data = df1))
VDIGXreg<-(lm(VDIGX~SnP500+SnP500*greatRecession+SnP500*Recession2000+SnP500*covidRecession-1, data = df1))
FDVLXreg<-(lm(FDVLX~SnP500+SnP500*greatRecession+SnP500*Recession2000+SnP500*covidRecession-1, data = df1))
PGSGXreg<-(lm(PGSGX~SnP500+SnP500*greatRecession+SnP500*Recession2000+SnP500*covidRecession-1, data = df1))

summary(Barclayreg)
summary(VDIGXreg)
summary(FDVLXreg)
summary(PGSGXreg)

vars<-c("Market Return","Great Recession", "2000's Recession","Covid Recession","Market X Great Recession","Market X 2000's Recession","Market X Covid Recession")
df6<-cbind.data.frame(vars, Barclayreg$coefficients, VDIGXreg$coefficients, FDVLXreg$coefficients, PGSGXreg$coefficients)

bptest(Barclayreg)
bptest(VDIGXreg)
bptest(Barclayreg)
bptest(FDVLXreg)

#end
#########################################################################################
#All code after this point in deprecated 


# BarclayLag<-lag(df1$BarclayDollar,1)
# SnPLag<-lag(df1$SnPDollar,1)
# # VDIGXLag<-lag(df1$VDIGXDollar,1)
# # PGSGXLag<-lag(df1$PGSGXDollar,1)
# # FDVLXLag<-lag(df1$FDVLXDollar,1)
# BarclayFD<-df1$BarclayDollar-BarclayLag
# SnPFD<-df1$SnPDollar-SnPLag
# # VDIGXFD<-df1$VDIGXDollar-VDIGXLag
# # PGSGXFD<-df1$PGSGXDollar-PGSGXLag
# # FDVLXFD<-df1$FDVLXDollar-FDVLXLag
# 
# summary(lm(BarclayFD~SnPFD+SnPFD*df1$Recession2000+SnPFD*df1$greatRecession+SnPFD*df1$covidRecession))
# 
# 
# 
# 
# 
# #First difference returns
# BarclayFD<-df1$Barclay-lag(df1$Barclay,1)
# SnPFD<-df1$SnP500-lag(df1$SnP500,1)
# VDIGXFD<-df1$VDIGX-lag(df1$VDIGX,1)
# PGSGXFD<-df1$PGSGX-lag(df1$PGSGX,1)
# FDVLXFD<-df1$FDVLX-lag(df1$FDVLX,1)
# 
# 
# df3<-cbind.data.frame(BarclayFD, SnPFD, VDIGXFD, PGSGXFD, FDVLXFD)
# df3<-na.omit(df3)
# df3$greatRecession<-df1$greatRecession[2:312]
# df3$covidRecession<-df1$covidRecession[2:312]
# df3$Recession2000<-df1$Recession2000[2:312]
# 
# 
# # summary(lm(BarclayFD~SnPFD+SnPFD*Recession2000+SnPFD*greatRecession+SnPFD*covidRecession, data = df3))
# # summary(lm(VDIGXFD~SnPFD+SnPFD*Recession2000+SnPFD*greatRecession+SnPFD*covidRecession, data = df3))
# # summary(lm(PGSGXFD~SnPFD+SnPFD*Recession2000+SnPFD*greatRecession+SnPFD*covidRecession, data = df3))
# # summary(lm(FDVLXFD~SnPFD+SnPFD*Recession2000+SnPFD*greatRecession+SnPFD*covidRecession, data = df3))
# 
# 
# 
# summary(lm(df1$Barclay[2:312]~SnPFD+SnPFD*greatRecession+SnPFD*Recession2000+SnPFD*covidRecession-1, data = df3))
# summary(lm(df1$VDIGX[2:312]~SnPFD+SnPFD*greatRecession+SnPFD*Recession2000+SnPFD*covidRecession-1, data = df3))
# summary(lm(df1$PGSGX[2:312]~SnPFD+SnPFD*greatRecession+SnPFD*Recession2000+SnPFD*covidRecession-1, data = df3))
# summary(lm(df1$FDVLX[2:312]~SnPFD+SnPFD*greatRecession+SnPFD*Recession2000+SnPFD*covidRecession-1, data = df3))
# 
# 
# regplot<-ggplot(lm(BarclayFD~SnPFD+SnPFD*Recession2000+SnPFD*greatRecession+SnPFD*covidRecession, data = df3))
# 
# 
# summary(lm(VDIGXFD~SnPFD, data = df3))
# 
# 
# 
# df3<-cbind.data.frame(BarclayLag, SnPLag, VDIGXLag, PGSGXLag, FDVLXLag)
# 
# 
# summary(lm(Barclay~SnP500+SnP500*greatRecession+SnP500*Recession2000+SnP500*covidRecession-1, data = df1))
# summary(lm(VDIGX~SnP500+SnP500*Recession2000+SnP500*greatRecession+SnP500*covidRecession-1, data = df1))
# summary(lm(FDVLX~SnP500+SnP500*greatRecession+SnP500*Recession2000+SnP500*covidRecession-1, data = df1))
# summary(lm(PGSGX~SnP500+SnP500*greatRecession+SnP500*Recession2000+SnP500*covidRecession-1, data = df1))
# 
# 
# summary(lm(df3$BarclayFD~df3$SnPFD))
# summary(lm(df3$VDIGXFD~df3$SnPFD))
# summary(lm(df3$FDVLXFD~df3$SnPFD))
# summary(lm(df3$PGSGX~df3$SnPFD))
# 
# Returns<-c(df1$Barclay, df1$VDIGX, df1$PGSGX, df1$FDVLX, df1$SnP)
# label<-rep("Berclay",311)
# label2<-rep("VDIGX",311)
# label3<-rep("PGSGX",311)
# label4<-rep("FDVLX",311)
# label5<-rep("SnP500",311)
# Fund<-c(label,label2,label3,label4,label5)
# df4<-cbind.data.frame(Returns, Fund)
# summary(aov(df4$Returns~ factor(df4$Fund)))
# 
# df5<-df1
# df5$covidRecession=df5$covidRecession*df5$SnP500
# df5$greatRecession=df5$greatRecession*df5$SnP500
# df5$Recession2000=df5$Recession2000*df5$SnP500
# summary(lm(Barclay~SnP500+greatRecession+Recession2000+covidRecession-1, data = df5))
# summary(lm(Barclay~SnP500+greatRecession+Recession2000+covidRecession-1, data = df5))
# 
