## ----message=FALSE, warning=FALSE, eval=FALSE, echo=TRUE-----------------
#  install.packages("TSstudio")

## ----message=FALSE, warning=FALSE, eval=FALSE, echo=TRUE-----------------
#  # install.packages("devtools")
#  devtools::install_github("RamiKrispin/TSstudio")

## ----fig.height=5, fig.width=8 , message=FALSE, warning=FALSE------------
library(TSstudio)
library(xts)
library(zoo)
library(quantmod)
# Loading the stock price of key technology companies:
tckrs <- c("GOOGL", "FB", "AAPL", "MSFT")
getSymbols(tckrs, 
           from = "2013-01-01",
           src = "yahoo")

# Visual Google closing price since 2013
Google <- GOOGL$GOOGL.Close
class(Google)

# Basic plot
ts_plot(Google)


# Adding titles and slider
ts_plot(Google, 
        title = "Google Stock Prices Since 2013",
        Xtitle = "Sourch: Yahoo Finance", 
        Ytitle = "Closing Price in USD",
        slider = TRUE
        )



## ----fig.height=5, fig.width= 8, message=FALSE, warning=FALSE------------
closing <- cbind(GOOGL$GOOGL.Close, FB$FB.Close, AAPL$AAPL.Close, MSFT$MSFT.Close)
names(closing) <- c("Google", "Facebook", "Apple", "Microsoft")

class(closing)
dim(closing)

# You can plot all the series in one plot using the type option:
ts_plot(closing, 
        title = "Top Technology Companies Stocks Prices Since 2013",
        type = "single")

# or use the default option - "multiple" and plot series on separate plots:
ts_plot(closing,
        title = "Top Technology Companies Stocks Prices Since 2013")


## ----fig.height=5, fig.width= 8, message=FALSE, warning=FALSE------------
# Load the US monthly natural gas consumption
data("USgas")

class(USgas)

ts_plot(USgas,
        title = "US Natural Gas Consumption",
        Xtitle = "Year",
        Ytitle = "Billion Cubic Feet"
        )


## ----fig.height=5, fig.width= 8, message=FALSE, warning=FALSE------------
ts_seasonal(USgas, type = "normal")


## ----fig.height=5, fig.width= 8, message=FALSE, warning=FALSE------------
ts_seasonal(USgas, type = "cycle")

## ----fig.height=5, fig.width= 8, message=FALSE, warning=FALSE------------
ts_seasonal(USgas, type = "box")

## ----fig.height=5, fig.width= 8, message=FALSE, warning=FALSE------------
ts_seasonal(USgas, type = "all")

## ----fig.height=5, fig.width= 8, message=FALSE, warning=FALSE------------
ts_heatmap(USgas)

## ----fig.height=5, fig.width= 8, message=FALSE, warning=FALSE------------
ts_surface(USgas)

## ----fig.height=5, fig.width= 8, message=FALSE, warning=FALSE------------
ts_polar(USgas)

## ----fig.height=5, fig.width= 8, message=FALSE, warning=FALSE------------
ts_decompose(USgas, type = "both")

## ---- fig.height=5, fig.width= 8, message=FALSE, warning=FALSE-----------
ts_acf(USgas, lag.max = 36)
ts_pacf(USgas, lag.max = 36)


## ----fig.height=5, fig.width= 8, message=FALSE, warning=FALSE------------
ts_lags(USgas)

## ----fig.height=5, fig.width= 8, message=FALSE, warning=FALSE------------
# set the forecast horizon for 12 months
h <- 12

# Split the data into training and testing sets (leaving the last 12 months for testing)
split_USgas <- ts_split(USgas, sample.out = h)

train <- split_USgas$train
test <- split_USgas$test


head(train, 5)
head(test, 5)


## ----fig.height=5, fig.width= 8, message=FALSE, warning=FALSE------------
library(forecast)
# Building a model on the training set
fit <- auto.arima(train, lambda = BoxCox.lambda(train))

# Checking the residuals
check_res(fit)


## ----fig.height=5, fig.width= 8, message=FALSE, warning=FALSE------------
fc <- forecast(fit, h = h)

test_forecast(actual = USgas, forecast.obj = fc, test = test)


## ----fig.height=5, fig.width= 8, message=FALSE, warning=FALSE------------
# Loading the Total US Vehicle Sales data 
data("USVSales")

ts_plot(USVSales, title = "Total US Vehicle Sales",
        Ytitle = "Thousands of Units",
        Xtitle = "Source: U.S. Bureau of Economic Analysis")

head(ts_plot)

USVSales_df <- ts_reshape(USVSales)

library(DT)

datatable(USVSales_df, filter = 'top', options = list(
  pageLength = nrow(USVSales_df), autoWidth = TRUE
))

## ----fig.height=5, fig.width= 8, message=FALSE, warning=FALSE------------
# Loading the University of Michigan Consumer Sentiment 
data("Michigan_CS")

Michigan_CS_ts <- xts_to_ts(Michigan_CS)

ts_plot(Michigan_CS, title = "University of Michigan Consumer Sentiment - 'xts' format")
ts_plot(Michigan_CS_ts, title = "University of Michigan Consumer Sentiment - 'ts' format")

class(Michigan_CS)
class(Michigan_CS_ts)

head(Michigan_CS)
head(Michigan_CS_ts)

head(index(Michigan_CS))
head(time(Michigan_CS_ts))

periodicity(Michigan_CS)
frequency(Michigan_CS_ts)


cycle(Michigan_CS_ts)

