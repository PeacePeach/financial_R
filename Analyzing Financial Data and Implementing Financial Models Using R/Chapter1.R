library(tidyverse)
library(tidyquant)

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















