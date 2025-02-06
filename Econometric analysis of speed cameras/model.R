rm(list = ls())
library(tidyverse)
library(fixest)
library(modelsummary)
load("finalData.Rdata")


noGroup<-finalData|>
  group_by(YEAR)|>
  summarise(mean(fatalities),sd(fatalities))

write.csv(noGroup, file = "summaryStats.csv",row.names = F)

x11()
ggplot(finalData, aes(x=YEAR, y = (fatalities), group = location))+geom_line(aes(color = treated))+geom_smooth(aes(group = treated))

parallelTrendCheck<-finalData|>
  group_by(treated,YEAR)|>
  summarise("fatalities" =mean(fatalities))
x11()
ggplot(parallelTrendCheck, aes(x=YEAR, y = fatalities, group = factor(treated)))+geom_line(aes(color = factor(treated)), size=1)+geom_point()+geom_vline(xintercept = 2013) + scale_color_manual(values = c("red", "blue"), labels = c("control", "treated"))+ theme(legend.title = element_blank())+ggtitle("Mean Fatalities by Year")+theme(plot.title = element_text(hjust = 0.5))

eventTime = c("YEAR_2009","YEAR_2010","YEAR_2011","YEAR_2012","YEAR_2013","YEAR_2014","YEAR_2015","YEAR_2016","YEAR_2017","YEAR_2018","YEAR_2019")

m2<-feols(finalData, fatalities~YEAR_2009+YEAR_2010+YEAR_2011+YEAR_2012+YEAR_2014+YEAR_2015+YEAR_2016+YEAR_2017+YEAR_2018+YEAR_2019
                    +YEAR_2009*treated+YEAR_2010*treated+YEAR_2011*treated+YEAR_2012*treated+YEAR_2014*treated+YEAR_2015*treated+YEAR_2016*treated+
                      YEAR_2017*treated+YEAR_2018*treated+YEAR_2019*treated+treated|COUNTY)
summary(m2)

m1<-feols(finalData, fatalities~treated+YEAR_2009+YEAR_2010+YEAR_2011+YEAR_2012+YEAR_2014+YEAR_2015+YEAR_2016+YEAR_2017+YEAR_2018+YEAR_2019
          +YEAR_2009*treated+YEAR_2010*treated+YEAR_2011*treated+YEAR_2012*treated+YEAR_2014*treated+YEAR_2015*treated+YEAR_2016*treated+YEAR_2017*treated+YEAR_2018*treated+YEAR_2019*treated
          +drunkDriver+rain+hail+snow+fog+highWinds+blowingSediment+other+cloudy+blowingSnow+freezingrain| COUNTY)
summary(m1)

View(m1$coeftable)

modelsummary(m2, stars = c("*" = .1, "**" = .05, "***" = .01), notes = "Signif. codes: *** 0.01 ** 0.05 * 0.1", split = "treated:YEAR_2009", title = "Regression Results", gof_omit = "Observations",output = "model1.docx")
