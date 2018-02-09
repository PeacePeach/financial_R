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
      col = "blue",
      lty = 2,
      lwd = 1)

lines(x = myData$Date,
      y = myData$AVGO_idx,
      col = "red",
      lty = 1, 
      lwd = 2)

lines(x = myData$Date,
      y = myData$TXN_idx,
      col = "orange",
      lty = 2, 
      lwd = 1)

abline(h = 1, lty = 1, col = "black")

legend("topleft", 
       c("INTC", "QCOM", "AVGO", "TXN"), 
       col = c("black", "blue", "red", "orange"), 
       lty = c(1, 2, 1, 2),
       lwd = c(2, 1, 2, 1),
       cex = 0.8)


# Plot the data with ggplot2
myData %>% ggplot(aes(Date, INTC_idx)) +
  geom_line() +
  labs(x = "Date", y = "Value of Investment ($)") +
  theme(axis.title = element_text(face = "bold", size = 13),
        plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray"),
        axis.line.x = element_blank())
  
  






