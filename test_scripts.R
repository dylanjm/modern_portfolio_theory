library(tidyverse)
library(tidyquant)
library(ggcorrplot)

stocks <- tq_get(c("AAPL","AMZN","MSFT","TSLA"), 
                 get = "stock.prices", 
                 from = "2010-01-01", 
                 to = "2017-01-01") %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "daily", type = "log")

index <- tq_get("^GSPC", 
                get = "stock.prices", 
                from = "2016-10-30", 
                to = "2017-10-30")

stocks %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted, 
               mutate_fun = periodReturn, 
               period = "daily", type = "log") %>%
  spread(key = symbol, value = daily.returns) %>%
  dplyr::select(-date) %>%
  cor() %>%
  ggcorrplot(method = "circle",
             hc.order = TRUE,
             outline.col = "gray",
             ggtheme = ggplot2::theme_bw,
             colors = c("#e42f25", "white","#6D9EC1")) +
  labs(title="Basket of Equities Correlation Matrix")
  
# The symbols vector holds our tickers. 
symbols <- c("SPY","EFA", "IJS", "EEM","AGG")

# The prices object will hold our raw price data throughout this book.
prices <- 
  getSymbols(symbols, src = 'yahoo', from = "2005-01-01", 
             auto.assign = TRUE, warnings = FALSE) %>% 
  map(~Ad(get(.))) %>% 
  reduce(merge) %>%
  `colnames<-`(symbols)
