# financial_R

My R codes on financial topics and materials based on books and online tutorials.

## Awesome R Plots

* Bollinger Bands

![bollingerbands_nestle](https://user-images.githubusercontent.com/24283367/36609465-d98f0d84-1907-11e8-907e-e200c6c9b945.png "Bollinger Bands on Nestle (Malaysia) Bhd Stock Prices")

* Four mini line charts using base graph

![four_plots](https://user-images.githubusercontent.com/24283367/36215764-f220989e-11e7-11e8-965b-5cb32c28c0d9.png "Top Semiconductor Companies")


* Candlestick chart with tidyquant theme

![top4_candlestick](https://user-images.githubusercontent.com/24283367/36164660-f9a46782-1127-11e8-899d-276e14a53de1.png "Candlestick with tidyquant")

## KLSE Stock Symbols Cheat Sheet
KLSE stocks symbols are not maintained as they are in Yahoo! Finance. Hence, you need to find the right symbol for the company you are interested in if you want to use the `tq_get()` function or the `getSymbols()` function. Google can easily help you with this. 

Example:
```R
nestle <- tq_get("4707.KL", get = "stock.prices", from = "2017-01-01", to = "2018-01-01")
```

Here is a mini cheat-sheet to get you started:

### Top Banks in Malaysia
| KLSE    | Yahoo! Finance Symbol| Company Name               |
| ------- |:--------------------:| -------------------------- |
| CIMB    | 1023.KL              | CIMB Group Holdings Berhad |
| MAYBANK | 1155.KL              | Malayan Banking Berhad     |
| PBBANK  | 1295.KL              | Public Bank Berhad         |
| RHBBANK | 1066.KL              | RHB Capital Berhad         |
| HLBANK  | 5819.KL              | Hong Leong Bank Berhad     |
| AMBANK  | 1015.KL              | AMMB Holdings Berhad       |


## Sources
* Analyzing Financial Data and Implementing Financial Models using R - [Clifford S. Ang, CFA](http://www.cliffordang.com/)
* [Tidyquant Vignettes](https://business-science.github.io/tidyquant/index.html) 
* [Yahoo! Finance](https://finance.yahoo.com/)
