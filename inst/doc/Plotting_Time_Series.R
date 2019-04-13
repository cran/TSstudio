## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      fig.width=8, fig.height=5)
options(list(menu.graphics = FALSE, scipen=99, digits= 3))
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)

## ------------------------------------------------------------------------
# install.packages("TSstudio")
library(TSstudio)
packageVersion("TSstudio")
# Function documentation
?ts_plot

## ------------------------------------------------------------------------
# Loading the USgas dataset
data("USgas")

ts_info(USgas)

## ------------------------------------------------------------------------
ts_plot(USgas)

## ------------------------------------------------------------------------
# Setting the plot titles
ts_plot(USgas,
        title = "US Natural Gas Consumption",
        Xtitle = "Year",
        Ytitle = "Billion Cubic Feet")

## ------------------------------------------------------------------------
ts_plot(USgas,
        title = "US Natural Gas Consumption",
        Xtitle = "Year",
        Ytitle = "Billion Cubic Feet",
        line.mode = "lines+markers",
        width = 3,
        color = "green")

## ------------------------------------------------------------------------
ts_plot(USgas,
        title = "US Natural Gas Consumption",
        Xtitle = "Year",
        Ytitle = "Billion Cubic Feet",
        line.mode = "lines+markers",
        width = 1,
        color = "#66C2A5",
        Xgrid = TRUE,
        Ygrid = TRUE,
        slider = TRUE)

## ----message=FALSE, warning=FALSE----------------------------------------
library(TSstudio)
library(xts)
library(zoo)
library(quantmod)
# Loading the stock price of key technology companies:
tckrs <- c("GOOGL", "FB", "AAPL", "MSFT")
getSymbols(tckrs, 
           from = "2013-01-01",
           src = "yahoo"
           )

# Creating a multiple time series object
closing <- cbind(AAPL$AAPL.Close, FB$FB.Close, GOOGL$GOOGL.Close, MSFT$MSFT.Close)
names(closing) <- c("Apple", "Facebook", "Google", "Microsoft")

ts_info(closing)

## ------------------------------------------------------------------------
# Plot each series in a sepreate (default option)
ts_plot(closing,
        title = "Top Technology Companies Stocks Prices Since 2013", 
        type = "multiple")


# All the series in one plot
ts_plot(closing, 
        title = "Top Technology Companies Stocks Prices Since 2013",
        type = "single")



## ------------------------------------------------------------------------
data(US_indicators)
str(US_indicators)

## ------------------------------------------------------------------------
ts_plot(US_indicators)

## ----message=FALSE, warning=FALSE----------------------------------------
USgas_partition <- ts_split(ts.obj = USgas, sample.out = 12)

train <- USgas_partition$train
test <- USgas_partition$test

library(forecast)

md1 <- auto.arima(train)
fc1 <- forecast(md1, h = 12)

test_forecast(actual = USgas, forecast.obj = fc1, test = test)

## ------------------------------------------------------------------------
md2 <- auto.arima(USgas)
fc2 <- forecast(md2, h = 60)

plot_forecast(fc2)

## ------------------------------------------------------------------------
md2_sim <- forecast_sim(model = md2, n = 200, h = 60)

md2_sim$plot

