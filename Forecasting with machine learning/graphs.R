library(readxl)
library(dplyr)
Fmonth<-read.csv("month_forecast.csv")
Fweek<- read.csv("week_forecast.csv")
Fday<-read.csv("day_forecast.csv")

Dmonth<-read_xlsx("DJIAmonthly.xlsx")
Dweek<-read_xlsx("DJIAweekly.xlsx")
Dday<-read_xlsx("DJIAdaily.xlsx")


n = nrow(Fmonth)
Fmonthdates<- Dmonth %>% 
  slice(1:n) %>%
  pull(Date)
Fmonth<-cbind.data.frame(Fmonth, Fmonthdates)
Fmonth$month <- rev(Fmonth$month)
png("monthFore.png")
plot(y = Dmonth$`Adj Close**`, x = Dmonth$Date, type = "l", col = "red", xlab = "", ylab = "Dow Jones Price", main = "Monthly Forecast");lines(y=Fmonth$month, x = Fmonthdates, col="blue")
dev.off()
n = nrow(Fweek)
Fweekdates<- Dweek %>% 
  slice(1:n) %>%
  pull(Date)
Fweek<-cbind.data.frame(Fweek, Fweekdates)
Fmonth$month <- rev(Fmonth$month)
png("weekFore.png")
plot(y = Dweek$`Adj Close**`, x = Dweek$Date, type = "l", col = "red", xlab = "", ylab = "Dow Jones Price", main = "Weekly Forecast");lines(y=Fweek$week, x = Fweekdates, col="blue")
dev.off()
n = nrow(Fday)
Fdaydates<- Dday %>% 
  slice(1:n) %>%
  pull(Date)
Fday<-cbind.data.frame(Fday, Fdaydates)
Fday$day <- rev(Fday$day)
png("dayFore.png")
plot(y = Dday$`Adj Close**`, x = Dday$Date, type = "l", col = "red", xlab = "", ylab = "Dow Jones Price", main = "Daily Forecast");lines(y=Fday$day, x = Fdaydates, col = "blue")
dev.off()
