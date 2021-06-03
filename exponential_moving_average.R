library(tidyquant) # To download the data
library(tidyr)
library(timetk)
#library(naniar)
tiingo_api_key('b254e84a270da1878f6bcc8e46ac44c9e56b176a')
tick = c("GIS", "SPGI", "WMT", "GE", "PFE")

moving_average <- function(tick, n, steps = 1){
  obj <- NULL
  data <- tq_get(tick,
                 from = '2015-01-01',
                 to = '2020-12-31',
                 get = 'stock.prices') #Get stock price from yahoo finance
  
  # data2 <- tq_get(tick,
  #                get = "tiingo.iex",
  #                from = '2015-01-01',
  #                to = '2020-12-31',
  #                resample_frequency = "5min") #Tiingo Prices, free alternative to
                                #Yahoo finance. Take close instead of adjusted
  ########### Ist close eine sinnvolle Alternative zu adjusted?
  
  ##Laden der Daten von Yahoo Finance
  price_data <- data[c("symbol","date", "adjusted")] %>%
    spread(symbol, value = adjusted) %>%
    tk_xts()
  obj$price_data <- price_data
  
  #Initialisieren von Matrizen
  ema_data <- price_data
  ema_data[is.na(ema_data)==FALSE]<- NA
  diff_data <- ema_data
  warn_data <- ema_data[steps:nrow(ema_data),]
  
  ##Moving average
  for (i in 1:ncol(price_data)){
    ema_data[,i] <- EMA(price_data[,i], n)
    diff_data[,i] <- sign(price_data[,i] - ema_data[,i])
    
  }
  diff_data <- diff_data[(n):nrow(diff_data), ]
  obj$ema_data <- ema_data
  obj$diff_data <- diff_data
  
  ##Warnmeldungen bei Vorzeichenwechsel
  for (i in 1:ncol(diff_data)){
    for (j in (steps+1):length(diff_data[,i])){
      warn_data[j,i] <- (diff_data[[j,i]]!=diff_data[[(j-1),i]])*diff_data[[j,i]]
    }
  }
  obj$warn_data <- warn_data
  return(obj)
}

test <- moving_average(tick = tick, n = 10)