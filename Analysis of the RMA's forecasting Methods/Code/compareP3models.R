rm(list = ls())
load("W&AforecastP3.Rdata")
load("RMAforecastP3.Rdata")
load("adjustedRRLPmodels.Rdata")
load("adjustedRRLPmodels15Year.Rdata")
load("adjustedRRLPmodels10Year.Rdata")
load("RMALPmodels.Rdata")
load("RMALPmodels15Year.Rdata")
load("RMALPmodels10Year.Rdata")


dataframe_names <- c("adjustedRRLPmodels", "adjustedRRLPmodels10Year", "adjustedRRLPmodels15Year",
                     "riskregiondf_10Year", "riskregiondf_15Year", "riskregiondf_5Year",
                     "RMAforecast10Year", "RMAforecast15Year", "RMAforecast5Year",
                     "RMALPmodels", "RMALPmodels10Year", "RMALPmodels15Year")

county_41<-unique(subset(adjustedRRLPmodels, adjustedRRLPmodels$commodityCode==41)$fullfips)
county_81<-unique(subset(adjustedRRLPmodels, adjustedRRLPmodels$commodityCode==81)$fullfips)

for (df_name in dataframe_names) {
  print(df_name)
    df <- get(df_name)
    
    column_name <- if ("County" %in% names(df)) "County" else if ("fullfips" %in% names(df)) "fullfips" else NA
    if (!is.na(column_name)) {
      assign(paste0(df_name, "_41"), df[df[[column_name]] %in% county_41, ], envir = .GlobalEnv)
      assign(paste0(df_name, "_81"), df[df[[column_name]] %in% county_81, ], envir = .GlobalEnv)
    }
    rm(list = df_name, envir = .GlobalEnv)
  
}

mean(riskregiondf_5Year_41$forecastError^2)^.5
mean(riskregiondf_10Year_41$forecastError^2)^.5
mean(riskregiondf_15Year_41$forecastError^2)^.5

mean(RMAforecast5Year_41$ForecastError^2)^.5
mean(RMAforecast10Year_41$ForecastError^2)^.5
mean(RMAforecast15Year_41$ForecastError^2)^.5

mean(adjustedRRLPmodels_41$forecastError^2)^.5
mean(adjustedRRLPmodels10Year_41$forecastError^2)^.5
mean(adjustedRRLPmodels15Year_41$forecastError^2)^.5

mean(RMALPmodels_41$ForecastError^2)^.5
mean(RMALPmodels10Year_41$ForecastError^2)^.5
mean(RMALPmodels15Year_41$ForecastError^2)^.5

mean(riskregiondf_5Year_81$forecastError^2)^.5
mean(riskregiondf_10Year_81$forecastError^2)^.5
mean(riskregiondf_15Year_81$forecastError^2)^.5

mean(RMAforecast5Year_81$ForecastError^2)^.5
mean(RMAforecast10Year_81$ForecastError^2)^.5
mean(RMAforecast15Year_81$ForecastError^2)^.5

mean(adjustedRRLPmodels_81$forecastError^2)^.5
mean(adjustedRRLPmodels10Year_81$forecastError^2)^.5
mean(adjustedRRLPmodels15Year_81$forecastError^2)^.5

mean(RMALPmodels_81$ForecastError^2)^.5
mean(RMALPmodels10Year_81$ForecastError^2)^.5
mean(RMALPmodels15Year_81$ForecastError^2)^.5