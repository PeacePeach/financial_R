library(tidyverse)
library(tidyquant)
library(Hmisc)

# Import Amazon stock price data from Yahoo Finance from December 31, 2010 to December 31, 2013
amazon <- tq_get("AMZN", get = "stock.prices", from = "2010-12-31", to = "2014-01-01")
amazon

# Renaming our columns
amazon <- amazon %>% 
  rename(AMZN.Open = open,
         AMZN.High = high,
         AMZN.Low = low,
         AMZN.Close = close,
         AMZN.Volume = volume, 
         AMZN.Adjusted = adjusted)
amazon

# Convert our data into xts object
amazon <- xts(amazon[,2:7], order.by = amazon$date)
amazon %>% head(15)
amazon %>% tail(15)
class(amazon)

# -------------------------
# Checking the Data
# -------------------------
# Verify that the data is complete by plotting a chart
plot(amazon$AMZN.Close)

# And by Checking the summary
summary(amazon)


# -------------------------
# Basic Data Manipulation
# -------------------------
# a) Subsetting using dates
# --> Data is an xts object
xts.2012 <- subset(amazon$AMZN.Close, index(amazon) >= "2012-01-01" & index(amazon) <= "2012-12-31")
xts.2012[c(1:3, nrow(xts.2012)), ]

#--> Data is a data.frame object
# First we need to change our data into a dataframe
amazon.2012 <- cbind(date = index(amazon), data.frame(amazon[, "AMZN.Close"]))
amazon.2012[c(1:3, nrow(amazon.2012)), ]
class(amazon.2012)

# Change the index 
rownames(amazon.2012) <- seq(1, nrow(amazon.2012))
amazon.2012[c(1:3, nrow(amazon.2012)), ]

# Subset the data
amazon.2012 <- subset(amazon.2012, amazon.2012$date >= "2012-01-01" & amazon.2012$date <= "2012-12-31")
amazon.2012[c(1:3, nrow(amazon.2012)), ]


# b) Converting daily prices to weekly and monthly prices
# xts object enables us to convert data very easily. 
# Weekly
wk <- amazon
amzn_weekly <- to.weekly(wk)
amzn_weekly[c(1:3, nrow(amzn_weekly)), ]

# Monthly
mo <- amazon
amzn_monthly <- to.monthly(mo)
amzn_monthly[c(1:3, nrow(amzn_monthly)), ]

# Candlestick Chart with quantmod
myOHLC <- amzn_monthly[-1, -6]
myOHLC
amzn_OHLC <- as.quantmod.OHLC(myOHLC, col.names = c("Open", "High", "Low", "Close", "Volume"))
class(amzn_OHLC)
amzn_OHLC[c(1:3, nrow(amzn_OHLC)), ]

chartSeries(amzn_OHLC, theme = "white.mono", name = "AMZN OHLC")


# -------------------------
# Comparing Capital Gains
# -------------------------
# Download 4 new data set of semiconductor companies
INTC <- tq_get("INTC", get = "stock.prices", from = "2014-12-31", to = "2018-01-01")
QCOM <- tq_get("QCOM", get = "stock.prices", from = "2014-12-31", to = "2018-01-01")
AVGO <- tq_get("AVGO", get = "stock.prices", from = "2014-12-31", to = "2018-01-01")
TXN <- tq_get("TXN", get = "stock.prices", from = "2014-12-31", to = "2018-01-01")


# A function that will rename our columns and turn it into a xts object
ren_xts <- function(myData, stockName, stockColNames) {
  myCol <-  str_c(stockName, capitalize(stockColNames), sep = ".")
  colnames(myData) <- myCol
  myData <- xts(myData[, 2:7], order.by = myData[[1]])
}

INTC <- ren_xts(INTC, "INTC", colnames(INTC))
INTC[c(1:3, nrow(INTC)), ]

QCOM <- ren_xts(QCOM, "QCOM", colnames(QCOM))
QCOM[c(1:3, nrow(QCOM)), ]

AVGO <- ren_xts(AVGO, "AVGO", colnames(AVGO))
AVGO[c(1:3, nrow(INTC)), ]

TXN <- ren_xts(TXN, "TXN", colnames(TXN))
TXN[c(1:3, nrow(INTC)), ]











