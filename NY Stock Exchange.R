library(knitr)
library(forecast)
library(dplyr)
library(ggplot2)
library(caret)
library(zoo)
library(xts)
setwd("D:/Geetha/Examples/Kaggle/NY Stock Exchange")

prices <- read.csv(file.path(getwd(), "prices.csv"))
price_split <-read.csv(file.path(getwd(), "prices-split-adjusted.csv"))
securities <- read.csv(file.path(getwd(), "securities.csv"))

#Combine the securities details with the price data.

colnames(securities)[1] <- "symbol"
prices_sec <- merge(price_split, securities, by = "symbol")
prices_sec <- subset(prices_sec, select = -c(Address.of.Headquarters, Date.first.added, CIK, SEC.filings))
prices_sec$Month_year <- format(as.Date(prices_sec$date), "%Y-%m")


#Analayse the data by selecting the symbol and plot the data

plot_data <- function(A) {

  data_1 <- subset(prices_sec, symbol == "A")
  data_1$Month_year <- format(as.Date(data_1$date), "%Y-%m")

  plot <- ggplot(data_1, aes(Month_year, close))+geom_point()+theme(axis.text.x = element_text(angle = 90))
  plot
}

plot_data(AAPL)



#Fit the arima model

data_1 <- subset(prices_sec, symbol == "A")
data_2 <- subset(data_1, select = c("date", "close"))
data_2 <- ts(data_2)

plot.ts(data_2)
ARIMAFIT <- auto.arima(data_2)
summary(ARIMAFIT)

pred_2_p <- predict(ARIMAFIT, n.ahead = 30)
pred_2_p

par(mfrow = c(1,1))
plot(data_2, xlab = "Years", ylab = "Rate", type = "l", main = "AUTO ARIMA")
lines(pred_2_p$pred,col='blue')

#analyse the data with prophet
data_3 <- data_2
colnames(data_3) <- c("ds", "y")
p <- prophet(data_3, daily.seasonality = TRUE)
future <- make_future_dataframe(p, periods = 30)
forecast <- predict(p, future)
plot(p,forecast)

prophet_plot_components(p, forecast)




