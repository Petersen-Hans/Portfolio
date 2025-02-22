####################
library(ggplot2)
#library(tidyverse)
library(readxl)
rm(list = ls())

load("adjustedRRLPmodels.Rdata")
load("LPDoubleSplineModels.Rdata")
load("RiskRegionModels.Rdata")
load("phase3SplineModels.Rdata")
load("countyDifference.Rdata")

df<-read_xlsx("countyData_dryrlandCornSoy.xlsx")
##########################################################
df19083<-subset(df, df$fullfips==19083)
df87<-subset(df19083, df19083$CommodityYear<=1980)

zero_5<-saveModels5Year$`19083_41_Zero spline model`$`Best model for 1947`
single_5<-saveModels5Year$`19083_41_Single spline model`$`Best model for 1947`
double_5<-saveModels5Year$`19083_41_Double spline model`$`Best model for 1947`

dfPlot_5<-subset(df87, df87$CommodityYear<=1978)

dfPlot_5<- dfPlot_5[order(dfPlot_5$CommodityYear), ]

dfPlot_5$double_5<-c(double_5$fitted.values, mean(c(double_5$fitted.values[length(double_5$fitted.values)], double_5$forecastVal)), double_5$forecastVal)
dfPlot_5$single_5<-c(single_5$fitted.values, mean(c(single_5$fitted.values[length(single_5$fitted.values)], single_5$forecastVal)), single_5$forecastVal)
dfPlot_5$zero_5<-c(zero_5$fitted.values, mean(c(zero_5$fitted.values[length(zero_5$fitted.values)], zero_5$forecastVal)), zero_5$forecastVal)
library(ggplot2)

library(ggplot2)

rmaPlot_5 <- ggplot(data = dfPlot_5) +
  geom_line(aes(x = CommodityYear, y = dryPlYldFinal), linewidth = 1.2) +  # Assuming you want this line without a legend entry
  geom_line(aes(x = CommodityYear, y = double_5, color = "Double Spline"), linewidth = 1) +
  geom_line(aes(x = CommodityYear, y = single_5, color = "Single Spline"), linewidth = 1) +
  geom_line(aes(x = CommodityYear, y = zero_5, color = "Zero Spline"), linewidth = 1) +
  geom_vline(xintercept = 1976, linetype = "dashed") +
  theme_minimal() +
  ggtitle("Phase 1\n Forecast of Corn Yield in Harden, Iowa 1947-1976") + 
  xlab("Year") + 
  ylab("Yield per Acre") +
  theme(plot.title = element_text(hjust = 0.5, size = 19),
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)) +
  scale_color_manual(name = "Model Type",
    values = c("Double Spline" = "red", "Single Spline" = "blue", "Zero Spline" = "green"),
    labels = c("Double Spline", "Single Spline", "Zero Spline") )

rmaPlot_5  # Display the plot


rmaPlot_5  # Display the plot

    

rmaPlot_5
################################################
df19083<-subset(df, df$fullfips==19083)
df87<-subset(df19083, df19083$CommodityYear<=1980)

zero_10<-saveModels10Year$`19083_41_Zero spline model`$`Best model for 1947`
single_10<-saveModels10Year$`19083_41_Single spline model`$`Best model for 1947`
double_10<-saveModels10Year$`19083_41_Double spline model`$`Best model for 1947`

dfPlot_10<-subset(df87, df87$CommodityYear<=1978)

dfPlot_10 <- dfPlot_10[order(dfPlot_10$CommodityYear), ]

dfPlot_10$double_10<-c(double_10$fitted.values, mean(c(double_10$fitted.values[length(double_10$fitted.values)], double_10$forecastVal)), double_10$forecastVal)
dfPlot_10$single_10<-c(single_10$fitted.values, mean(c(single_10$fitted.values[length(single_10$fitted.values)], single_10$forecastVal)), single_10$forecastVal)
dfPlot_10$zero_10<-c(zero_10$fitted.values, mean(c(zero_10$fitted.values[length(zero_10$fitted.values)], zero_10$forecastVal)), zero_10$forecastVal)

rmaPlot_10<-ggplot(data = dfPlot_10) + geom_line( aes(x = CommodityYear, y = dryPlYldFinal)) + 
  geom_line( aes(x = CommodityYear, y = double_10),color = "red", linewidth = 1)+
  geom_line(aes(x = CommodityYear, y = single_10), color = "cyan", linewidth = 1)+
  geom_line(aes(x = CommodityYear, y = zero_10), color = "pink", linewidth = 1)+
  geom_vline(xintercept = 1976, linetype = "dashed")+
  theme_minimal()+
  ggtitle("RMA Forecast of Corn Yield in Harden, Iowa 1947-1976") + 
  xlab("Year") + 
  ylab("Yield per Acre")+
  theme(plot.title = element_text(hjust = 0.5))

rmaPlot_10
#######################################################################
df19083<-subset(df, df$fullfips==19083)
df87<-subset(df19083, df19083$CommodityYear<=1980)

zero_15<-saveModels15Year$`19083_41_Zero spline model`$`Best model for 1947`
single_15<-saveModels15Year$`19083_41_Single spline model`$`Best model for 1947`
double_15<-saveModels15Year$`19083_41_Double spline model`$`Best model for 1947`

dfPlot_15<-subset(df87, df87$CommodityYear<=1978)

dfPlot_15 <- dfPlot_15[order(dfPlot_15$CommodityYear), ]

dfPlot_15$double_15<-c(double_15$fitted.values, mean(c(double_15$fitted.values[length(double_15$fitted.values)], double_15$forecastVal)), double_15$forecastVal)
dfPlot_15$single_15<-c(single_15$fitted.values, mean(c(single_15$fitted.values[length(single_15$fitted.values)], single_15$forecastVal)), single_15$forecastVal)
dfPlot_15$zero_15<-c(zero_15$fitted.values, mean(c(zero_15$fitted.values[length(zero_15$fitted.values)], zero_15$forecastVal)), zero_15$forecastVal)

rmaPlot_15<-ggplot(data = dfPlot_15) + geom_line( aes(x = CommodityYear, y = dryPlYldFinal)) + 
  geom_line( aes(x = CommodityYear, y = double_15),color = "red", linewidth = 1)+
  geom_line(aes(x = CommodityYear, y = single_15), color = "cyan", linewidth = 1)+
  geom_line(aes(x = CommodityYear, y = zero_15), color = "pink", linewidth = 1)+
  geom_vline(xintercept = 1976, linetype = "dashed")+
  theme_minimal()+
  ggtitle("RMA Forecast of Corn Yield in Harden, Iowa 1947-1976") + 
  xlab("Year") + 
  ylab("Yield per Acre")+
  theme(plot.title = element_text(hjust = 0.5))

rmaPlot_15
#############################################################
# Adding an identifier column to each dataframe
dfPlot_15$Group <- "15"
dfPlot_10$Group <- "10"
dfPlot_5$Group <- "5"

names(dfPlot_15)[names(dfPlot_15) == "double_15"] <- "double"
names(dfPlot_15)[names(dfPlot_15) == "single_15"] <- "single"
names(dfPlot_15)[names(dfPlot_15) == "zero_15"] <- "zero"

names(dfPlot_10)[names(dfPlot_10) == "double_10"] <- "double"
names(dfPlot_10)[names(dfPlot_10) == "single_10"] <- "single"
names(dfPlot_10)[names(dfPlot_10) == "zero_10"] <- "zero"

names(dfPlot_5)[names(dfPlot_5) == "double_5"] <- "double"
names(dfPlot_5)[names(dfPlot_5) == "single_5"] <- "single"
names(dfPlot_5)[names(dfPlot_5) == "zero_5"] <- "zero"

# Combining the data frames
combined_df <- rbind(dfPlot_15, dfPlot_10, dfPlot_5)

combined_plot <- ggplot(data = combined_df, aes(x = CommodityYear, group = Group)) + 
  geom_line(aes(y = dryPlYldFinal), color = "black", size = 1) +  # Black line for dryPlYldFinal
  geom_line(aes(y = double, color = Group), linetype = "dashed", size = 0.8) +
  geom_line(aes(y = single, color = Group), linetype = "dotdash", size = 0.8) +
  geom_line(aes(y = zero, color = Group), linetype = "twodash", size = 0.8) +
  geom_vline(xintercept = 1976, linetype = "dashed") +
  scale_color_manual(values = c("15" = "red", "10" = "blue", "5" = "green")) +  # Custom colors for each group
  theme_minimal() +
  ggtitle("RMA Forecast of Corn Yield in Harden, Iowa 1947-1976") +
  xlab("Year") +
  ylab("Yield per Acre") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank())


# Printing the plot
x11();print(combined_plot)
#####################################
doublePlot <- ggplot(data = combined_df, aes(x = CommodityYear, group = Group)) + 
  geom_line(aes(y = dryPlYldFinal), color = "black", size = 1.25) +  # Black line for dryPlYldFinal
  geom_line(aes(y = double, color = Group),  size = 0.8) +
  geom_vline(xintercept = 1976, linetype = "dashed") +
  geom_vline(xintercept = 1976-5-3, linetype = "longdash",color = "gold") +
  geom_vline(xintercept = 1976-10-3, linetype = "longdash",color = "purple") +
  geom_vline(xintercept = 1976-4, linetype = "longdash", color = "chartreuse3") +
  theme_minimal() +
  ggtitle("Phase 3 pt. 1\nForecast of Corn Yield in Harden, Iowa 1947-1976") +
  xlab("Year") +
  ylab("Yield per Acre") +
  scale_color_manual(
    name = "Breakpoint Buffer", values = c( "gold", "purple","chartreuse3"), labels = c("15", "10", "5"))+
  theme(plot.title = element_text(hjust = 0.5, size = 19),
        legend.position = "bottom",
        legend.title = element_text(size = 14), 
        legend.text = element_text(size = 12))


# Printing the plot
x11();print(doublePlot)

###########################
load("fittedResults.Rdata")
#####
load("RiskRegionDoubleSplineLPModels15Year.Rdata")
load("countyDifference.Rdata")
model<-doubleSplineRRLP15Year$`504`$`Best model for 1947`
yearRange <- seq(model$startYear, model$startYear + 31)
RRfittedValues <- model$solution[1] + model$solution[2] * yearRange + 
  ifelse(yearRange > model$breakpoint1, model$solution[3] * (yearRange - model$breakpoint1), 0) +
  ifelse(yearRange > model$breakpoint2, model$solution[4] * (yearRange - model$breakpoint2), 0)
#####

df19083<-subset(df, df$fullfips==19083)
df87<-subset(df19083, df19083$CommodityYear<=1980)
RMAmodel <- fittedResults$`19083_Best model for 1947`
dfPlot_LP<-subset(df87, df87$CommodityYear<=1978)
dfPlot_LP<- dfPlot_LP[order(dfPlot_LP$CommodityYear), ]
dfPlot_LP<-cbind(dfPlot_LP, RMAmodel,RRfittedValues)

library(ggplot2)

LPPlot <- ggplot(data = dfPlot_LP, aes(x = CommodityYear)) +
  geom_line(aes(y = dryPlYldFinal), color = "black", size = 1.25) +
  geom_line(aes(y = FittedValues, color = "red"),  size = 0.8) +
  geom_line(aes(y = RRfittedValues, color = "blue"), size = 0.8) +
  geom_vline(xintercept = 1976, linetype = "dashed") +
  theme_minimal() +
  ggtitle("Phase 3 pt. 2\nForecast of Corn Yield in Harden, Iowa 1947-1976") +
  xlab("Year") +
  ylab("Yield per Acre") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  scale_color_manual(
    name = "Model Type", values = c( "red", "blue"), labels = c("County", "Risk Region"))+
  theme(plot.title = element_text(hjust = 0.5, size = 19),
        legend.position = "bottom",
        legend.title = element_text(size = 14), 
        legend.text = element_text(size = 12))

x11(); LPPlot
