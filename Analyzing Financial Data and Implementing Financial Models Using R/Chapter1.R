library(tidyquant)
library(tidyverse)
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

# -----------------------------
# Checking the Data
# -----------------------------

# Verify that the data is complete by plotting a chart
plot(amazon$AMZN.Close)

# And by Checking the summary
summary(amazon)


# -----------------------------
# Basic Data Manipulation
# -----------------------------

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


# -----------------------------
# Comparing Capital Gains
# -----------------------------

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

# ---TBD---
# Selecting columns with the same name from multiple dataframes 
# lapply(list(INTC, QCOM), function(x) grepl("Close", names(x)))
# lapply(lapply(ls(pattern="scp"), get), function(x) x[grepl("Close$", colnames(x))])

# Select the Close price only
closePrices <- cbind(INTC$INTC.Close, QCOM$QCOM.Close, AVGO$AVGO.Close, TXN$TXN.Close)
closePrices[c(1:3, nrow(closePrices)), ]
class(closePrices)


# Convert the data into dataframe
closeDataframe <- cbind(Date = index(closePrices), data.frame(closePrices))
rownames(closeDataframe) <- seq(1, nrow(closeDataframe), 1)
names(closeDataframe) <- c("Date", "INTC", "QCOM", "AVGO", "TXN")
closeDataframe %>% head(10)

# Normalize the data
myData <- closeDataframe %>% 
  mutate_at(vars(matches("[[:upper:]]$", ignore.case = FALSE)), funs(idx = ./.[1]))
myData[c(1:3, nrow(myData)), ]

# Plot the data with base graphing
plot(x = myData$Date, 
     y = myData$INTC_idx, 
     type = "l", 
     xlab = "Date", 
     ylab = "Value of Investment ($)", 
     col = "black",
     lty = 1,
     lwd = 2, 
     main = "Value of $1 Investment in Intel, Qualcomm, Broadcom, Texas Instruments",
     ylim = range(c(myData$INTC_idx, myData$QCOM_idx, myData$AVGO_idx)))

lines(x = myData$Date, 
      y = myData$QCOM_idx,
      col = "dodgerblue",
      lty = 2,
      lwd = 1)

lines(x = myData$Date,
      y = myData$AVGO_idx,
      col = "#DC143C",
      lty = 1, 
      lwd = 2)

lines(x = myData$Date,
      y = myData$TXN_idx,
      col = "coral",
      lty = 2, 
      lwd = 1)

abline(h = 1, lty = 1, col = "black")

legend("topleft", 
       c("INTC", "QCOM", "AVGO", "TXN"), 
       col = c("black", "dodgerblue", "#DC143C", "coral"), 
       lty = c(1, 2, 1, 2),
       lwd = c(2, 1, 2, 1),
       cex = 0.8)


# Plot the data with ggplot2
myData %>% ggplot(aes(Date)) +
  geom_line(aes(y = INTC_idx, color = "var0"), size = 1) +
  geom_line(aes(y = QCOM_idx, color = "var1"), size = 1) +
  geom_line(aes(y = AVGO_idx, color = "var2"), size = 1) +
  geom_line(aes(y = TXN_idx, color = "var3"), size = 1) +
  geom_hline(yintercept = 1.0, color = "dimgrey") +
  ggtitle("Value of $1 Investment in Intel, Qualcomm, Broadcom, Texas Instruments") +
  labs(x = "Date", y = "Value of Investment ($)") +
  theme(axis.title = element_text(face = "bold", size = 13),
        plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(),
        axis.text = element_text(size = 11),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.key = element_blank(),
        legend.position = c(0.055, 0.85)) +
  scale_color_manual(values = c("black", "dodgerblue", "#DC143C", "coral"), 
                     labels = c("INTC", "QCOM", "AVGO", "TXN"))
  
# Alternative presentation 
par(oma = c(0, 0, 3, 0))
par(mfrow = c(2, 2))
y.range <- range(myData[,6:9])
plot(x = myData$Date, 
     xlab = "", 
     y = myData$QCOM_idx,
     ylim = y.range,
     ylab = "",
     type = "l",
     col = "gray",
     main = "Intel Stock")
lines(x = myData$Date, y = myData$AVGO_idx, col = "gray")
lines(x = myData$Date, y = myData$TXN_idx, col = "gray")
lines(x = myData$Date, y = myData$INTC_idx, col = "black", lwd = 2)
abline(h=1)

plot(x = myData$Date,
     y = myData$INTC_idx,
     xlab = "",
     ylab = "",
     ylim = y.range,
     type = "l",
     col = "gray",
     main = "Qualcomm Stock")
lines(x = myData$Date, y = myData$AVGO_idx, col = "gray")
lines(x = myData$Date, y = myData$TXN_idx, col = "gray")
lines(x = myData$Date, y = myData$QCOM_idx, col = "dodgerblue", lwd = 2)
abline(h=1)

plot(x = myData$Date, 
     xlab = "", 
     y = myData$QCOM_idx,
     ylim = y.range,
     ylab = "",
     type = "l",
     col = "gray",
     main = "Broadcom Stock")
lines(x = myData$Date, y = myData$INTC_idx, col = "gray")
lines(x = myData$Date, y = myData$TXN_idx, col = "gray")
lines(x = myData$Date, y = myData$AVGO_idx, col = "#DC143C", lwd = 2)
abline(h=1)

plot(x = myData$Date, 
     xlab = "", 
     y = myData$QCOM_idx,
     ylim = y.range,
     ylab = "",
     type = "l",
     col = "gray",
     main = "Texas Instruments Stock")
lines(x = myData$Date, y = myData$AVGO_idx, col = "gray")
lines(x = myData$Date, y = myData$INTC_idx, col = "gray")
lines(x = myData$Date, y = myData$TXN_idx, col = "coral", lwd = 2)
abline(h=1)

title(main = "Value of $1 Invested in Top Semiconductor Companies\nDecember 31, 2014 - December 31, 2017", 
      outer = TRUE)

# Alternative presentation with ggplot
tidyData <- myData %>% 
  select(Date, contains("_idx")) %>% 
  gather(INTC_idx, QCOM_idx, AVGO_idx, TXN_idx, key = "Symbol", value = "Index")

tidyData %>% 
  ggplot(aes(x = Date, y = Index)) +
  geom_line() +
  facet_wrap(~ Symbol)


# -----------------------------
# Technical Analysis Examples
# -----------------------------

# Technical analysis is the use of charts to study stock price and volume data for the purpose of forecasting trends.
# 3 groups of technical indicators:
#   (a) trend indicator      --> moving average
#   (b) volatility indicator --> Bollinger Bands
#   (c) momentum indicattor  --> relative strength index

# ----Simple Moving Average Crossover----
# Also known as SMA crossover.
# "simple" because we treat all days equally, regardless of how near or far those days are from the present. 
# "crossover" because we will use two SMA lines, a shorter-term and a longer-term, and make trade decisions when
# the line crosses. 

# Download Maybank stock price from 2017 to 2018
MYB <- tq_get("1155.KL", get = "stock.prices", from = "2016-01-01", to = "2017-12-31")
MYB %>% head()

# We'll use only the closing price and use that to calculate rolling 50-day and 200-day moving average
MYB.sma <- MYB %>% 
  select(date, close) %>% 
  filter(!is.na(close)) %>% 
  mutate(sma50  = rollapply(close, FUN = mean, fill = NA, width = 50,  align = "right"),
         sma200 = rollapply(close, FUN = mean, fill = NA, width = 200, align = "right"))
MYB.sma %>% slice(c(45:51, 195:201))

# Convert our data into an xts object
MYB.xts <- xts(MYB.sma[,2:4], order.by = MYB.sma$date)

myb.range <- range(MYB.xts, na.rm = TRUE)
myb.range
MYB.xts %>% summary()
par(mfrow = c(1,1))
plot(x = index(MYB.xts), 
     y = MYB.xts$close, 
     ylim = myb.range, 
     ylab = "Price ($)",
     xlab = "Date",
     type = 'l',
     main = "Maybank - Simple Moving Average\nJanuary 1, 2017 - December 31, 2017")
lines(x = index(MYB.xts), y = MYB.xts$sma50)
lines(x = index(MYB.xts), y = MYB.xts$sma200, lty = 2)
legend("topleft", c("Maybank Price", "50-day Moving Average", "200-day Moving Average"), lty = c(1, 1, 2))










