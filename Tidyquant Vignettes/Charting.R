library(tidyquant)

# Get the stock prices
MSFT <- tq_get("MSFT", get = "stock.prices", from = "2014-12-31", to = "2018-01-01")
GOOG <- tq_get("GOOG", get = "stock.prices", from = "2014-12-12", to = "2018-01-01")

# Our end date
end <- as_date("2018-01-01")

# Plot a line chart with tidyquant theme
MSFT %>% 
  ggplot(aes(date, close)) +
  geom_line() +
  ggtitle("MSFT Line Chart", "2015 - 2017") +
  labs(x = "", y = "Closing Price") +
  theme_tq()

# Candlestick chart
GOOG %>% 
  ggplot(aes(x = date, y = close)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
  ggtitle("GOOGLE Candlestick Chart", "2015 - 2017") +
  labs(x = "", y = "Closing Price") +
  theme_tq()

# Using coord_x_date to zoom in to specific time
GOOG %>% 
  ggplot(aes(x = date, y = close)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close),
                   color_up = "Dark Green",
                   color_down = "Fire Brick",
                   fill_up = "Lime Green",
                   fill_down = "#DC143C",
                   color = "black") +
  ggtitle("GOOGLE Candlestick Chart", "Q4'2017") +
  labs(x = "", y = "Closing Price") +
  coord_x_date(xlim = c(as_date("2017-10-01"), end), ylim = c(900, 1100)) +
  theme_tq()
  
# Get data for 2018
AMZN <- tq_get("AMZN", get = "stock.prices", from = "2018-01-01", to = "2018-02-14")
AAPL <- tq_get("AAPL", get = "stock.prices", from = "2018-01-01", to = "2018-02-14")
MSFT <- tq_get("MSFT", get = "stock.prices", from = "2018-01-01", to = "2018-02-14")
GOOG <- tq_get("GOOG", get = "stock.prices", from = "2018-01-01", to = "2018-02-14")

# Function to add stock name into the dataset
add_name <- function(myStock, myName) {
  myStock <- myStock %>% mutate(symbol = myName) %>% select(symbol, everything())
}

AMZN <- AMZN %>% add_name("AMZN")
AAPL <- AAPL %>% add_name("AAPL")
MSFT <- MSFT %>% add_name("MSFT")
GOOG <- GOOG %>% add_name("GOOG")
AAMG <- bind_rows(AMZN, AAPL, MSFT, GOOG)
AAMG %>% count(symbol)


AAMG %>% 
  ggplot(aes(x = date, y = close, group = symbol)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close),
                   color_up = "Dark Green",
                   color_down = "Fire Brick",
                   fill_up = "Lime Green",
                   fill_down = "#DC143C",
                   color = "black") +
  ggtitle("Silicon Valley Top Tech Companies Candlestick Chart", "Year: 2018") +
  labs(x = "", y = "Closing Price") +
  facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
  theme_tq()
