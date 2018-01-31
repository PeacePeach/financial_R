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

