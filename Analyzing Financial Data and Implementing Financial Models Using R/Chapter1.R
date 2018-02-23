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
       cex = 0.75)


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
legend("bottomright", 
       c("Maybank Price", "50-day Moving Average", "200-day Moving Average"), 
       lty = c(1, 1, 2), 
       cex = 0.75)


# ----Volatility: Bollinger Bands----
# A frequently used technical anlaysis volatility indicators are Bollinger Bands. 
# 3 components of the Bollinger Bands:
# (1) 20-day SMA
# (2) An upper band - 2 std dev above the 20-day SMA
# (3) A lower band - 2 std dve below the 20-day SMA
# Bollinger Bands widen (narrow) with more (less) volatility in the stock.
# When the bands narrow, it may be used as an indication that volatility is about to rise.

# Use our Amazon data
amazon %>% head()
amazon.bb <- amazon[, 4]
amazon.bb %>% head()

# Calculate 20-day SMA and the standarad deviations
amazon.bb$avg <- rollmeanr(amazon.bb$AMZN.Close, k = 20)
amazon.bb$sd <- rollapply(amazon.bb$AMZN.Close, width = 20, FUN = sd, fill = NA)
amazon.bb[c(1:3, nrow(amazon.bb))]
amazon.bb[18:22,]

# Subset to only show 2013 data
amazon.bb2013 <- subset(amazon.bb, index(amazon.bb) >= "2013-01-01")
amazon.bb2013[c(1:3, nrow(amazon.bb2013)), ]

# Create the upper and lower Bollinger Band
amazon.bb2013$sd2up <- amazon.bb2013$avg + 2 * amazon.bb2013$sd
amazon.bb2013$sd2dn <- amazon.bb2013$avg - 2 * amazon.bb2013$sd
amazon.bb2013[c(1:3, nrow(amazon.bb2013)), ]

# Plot the Bollinger Bands
y.bb.range <- range(amazon.bb2013[,-3], na.rm = TRUE)
y.bb.range

plot(x = index(amazon.bb2013),
     y = amazon.bb2013$AMZN.Close,
     xlab = "Date",
     ylab = "Price ($)",
     type = "l",
     lwd = 3, 
     main = "Amazon - Bollinger Bands (20-days sma, 2 deviations)\nJanuary 1 - December 31, 2013"
     )
lines(x = index(amazon.bb2013), y = amazon.bb2013$avg, lty = 2)
lines(x = index(amazon.bb2013), y = amazon.bb2013$sd2up, col = "gray40")
lines(x = index(amazon.bb2013), y = amazon.bb2013$sd2dn, col = "gray40")
legend("topleft", 
       c("Amazon Price", "20-Day Moving Average", "Upper Band", "Lower Band"),
       lty = c(1, 2, 1, 1),
       lwd = c(3, 1, 1, 1),
       col = c("black", "black", "gray40", "gray40"),
       cex = 0.75)

# Now we test it out with Nestle (Malaysia) Bhd stock.
# Download ytd data
nestle <- tq_get("4707.KL", get = "stock.prices", from = "2016-11-01", to = "2018-01-01")
nestle.sma <- nestle %>% 
  filter(!is.na(close)) %>%
  mutate(sma20 = rollapply(close, FUN = mean, width = 20, fill = NA, align = "right"),
         sd = rollapply(close, FUN = sd, width = 20, fill = NA, align = "right"), 
         sd2up = sma20 + 2 * sd, 
         sd2dn = sma20 - 2 * sd)
nestle.sma %>% slice(18:20)
nestle.sma2017 <- nestle.sma %>% 
  filter(date >= "2017-01-01", date < "2018-01-01")
nestle.over <- nestle.sma2017 %>% 
  filter(close - sd2up >= 0.7) %>% 
  mutate(category = "overbought")

nestle.under <- nestle.sma2017 %>% 
  filter(close - sd2dn < - 0.5)%>% 
  mutate(category = "oversold")

nestle.sma2017 %>% 
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = sd2dn, ymax = sd2up), alpha = 0.3, fill = "khaki") +
  geom_line(aes(y = close, color = "colB"), size = 1.2) + 
  geom_line(aes(y = sma20, color = "colA"), size = 0.75) +
  geom_line(aes(y = sd2up), linetype = "dotted", size = 0.7) +
  geom_line(aes(y = sd2dn), linetype = "dotted", size = 0.7, color = "dimgrey") +
  geom_text(data = nestle.under, aes(x = date + 8, y = close - 0.5, label = category), color = "mediumpurple") +
  geom_point(data = nestle.under, aes(x = date, y = close), color = "orchid") +
  geom_text(data = nestle.over, aes(x = date - 10, y = close + 0.5, label = category), color = "firebrick") +
  geom_point(data = nestle.over, aes(x = date, y = close), color = "darkred") +
  annotate(geom = "text", x = date("2017-01-01"), y = 81, label = "Upper\nBand") +
  annotate(geom = "text", x = date("2017-01-01"), y = 74, label = "Lower\nBand") +
  ggtitle("Bollinger Bands on Nestle (Malaysia) Bhd Stock Prices", "January 1 - December 31, 2017") +
  labs(x ="Date", y = "Price ($)") +
  scale_color_manual(values = c("colA" = "tomato", "colB" = "black"), labels = (c("SMA-20", "Closing Price"))) +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(face = "bold", size = 15, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 13),
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 10)),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.line = element_line(),
        axis.text = element_text(size = 13),
        legend.title = element_blank(),
        legend.background = element_rect(color = "black", fill = "white"),
        legend.key = element_rect(fill = "white"),
        legend.text = element_text(size = 12),
        legend.position = c(0.1, 0.9))











