library(tidyverse)
library(tidyquant)

# Import Amazon stock price data from Yahoo Finance from December 31, 2010 to December 31, 2013
amazon <- tq_get("AMZN", get = "stock.prices", from = "2010-12-31", to = "2013-12-31")
amazon
