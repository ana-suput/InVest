library(tidyquant) # To download the data
library(plotly) # To create interactive charts
library(timetk) # To manipulate the data series
library(tidyr)
library(dplyr)

#tick <- c('AMZN', 'AAPL', 'NFLX', 'XOM', 'T')
#tick = c("MMM", "AI.PA", "GIS", "PEP", "SPGI", "UPS", "WMT", "GE", "PFE", "MNST")
#tick = c("MMM", "AI.PA", "GIS", "PEP", "SPGI")
#tick = c("GE", "PFE", "MNST")
tick = c("GIS", "SPGI", "WMT", "GE", "PFE")
# tick_vgl = c("OMV", "F", "WMT")
# price = c(37,12,145)
# amount = c(50,45,18)
tick_vgl = c("OMV", "DTE.DE", "PEP", "UPS", "WMT", "PFE", "MNST")
price = c(37,15,143,165,145,35,92)
amount = c(14,17,7,5,9,10,8)

wt = matrix(price*amount/sum(price*amount), nrow =1)
#Get quantitative data in tibble format
efficienc_frontier <- function(tick, wts = NULL){
  obj <- NULL
price_data <- tq_get(tick,
                     from = '2015-01-01',
                     to = '2020-12-31',
                     get = 'stock.prices') #Get stock price from yahoo finance

#
price_data_year <-price_data %>%
  mutate(year = substring(date, 1,4))

#aggregate(price_data_year[["adjusted"]], list(log_ret_tidy_year[["symbol"]]), mean)
means_year <- price_data_year %>%
  group_by(symbol,year) %>%
  summarise_at(vars(adjusted), funs(mean(.)))#, na.rm=TRUE)))
#Stocks mit fehlenden Werten werden entfernt
stocks_na <-means_year%>%
  group_by(symbol)%>%
  summarise_at(vars(adjusted), funs(any(is.na(.))))
means_year = means_year[means_year[["symbol"]] %in% stocks_na[["symbol"]][!stocks_na[["adjusted"]]],]

if (!is.null(wts)){
wts = matrix(wts[!stocks_na[["adjusted"]]], nrow = 1)
}

##Stocks mit geringerem Wert im letzten als im ersten Jahr werden entfernt
means_year_min <- means_year[means_year[["year"]]==min(means_year[["year"]]),]
means_year_max <- means_year[means_year[["year"]]==max(means_year[["year"]]),]
selected_vars <- means_year_max[["symbol"]][means_year_max[["adjusted"]] > means_year_min[["adjusted"]]]

if(length(selected_vars)<=1){
  obj = (selected_vars)
}else{
price_data_selected = price_data[price_data[["symbol"]] %in% selected_vars,]
#head(price_data)
#Group price_data by symbol (=tick)
log_ret_tidy <- price_data_selected %>%
  group_by(symbol) %>%
  #Calculates daily logarithmic periodic return
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'ret',
               type = 'log')
#head(log_ret_tidy)


#Stocks as colnames and date as rownames
log_ret_xts <- log_ret_tidy %>%
  spread(symbol, value = ret) %>%
  tk_xts()
#head(log_ret_xts)

#Calculate daily mean
mean_ret <- colMeans(log_ret_xts)
print(round(mean_ret, 5))

#Calculate covariance matrix and anualize it
cov_mat <- cov(log_ret_xts) * 252
print(round(cov_mat,4))

#Calculate weights
if (is.null(wts)){
n = ncol(cov_mat)
wts = matrix(NA, ncol = n, nrow = 11^n)
for (i in 1:n){
  wts[,i] <- rep(seq(0,1, 0.1), each = 11^(n-i), times = 11^(i-1))
}
wts <- wts[rowSums(wts)==1,]
wts
  
}

port_returns <- vector(length = dim(wts)[1])
port_risk <- vector(length = dim(wts)[1])
sharpe_ratio <- vector(length = dim(wts)[1])
# if ((dim(wts)[1])>1)  {
for (i in 1:dim(wts)[1]) {
  
   # Portfolio returns
  
  port_ret <- sum(wts[i,] * mean_ret)
  port_ret <- ((port_ret + 1)^252) - 1
  # Storing Portfolio Returns values
  port_returns[i] <- port_ret
  
  
  # Creating and storing portfolio risk
  port_sd <- sqrt(t(wts[i,]) %*% (cov_mat  %*% (wts[i,])))
  port_risk[i] <- port_sd
  
  # Creating and storing Portfolio Sharpe Ratios
  # Assuming 0% Risk free rate
  
  sr <- port_ret/port_sd
  sharpe_ratio[i] <- sr
  
}
# }else{
#   port_ret <- sum(wts * mean_ret)
#   port_ret <- ((port_ret + 1)^252) - 1
#   # Storing Portfolio Returns values
#   port_returns <- port_ret
# 
# 
#   # Creating and storing portfolio risk
#   cov_mat
#   wts
#   port_sd <- sqrt((cov_mat%*%matrix(wts, nrow = 1)) %*% (matrix(wts, ncol = 1)))
#   port_risk <- port_sd
# 
#   # Creating and storing Portfolio Sharpe Ratios
#   # Assuming 0% Risk free rate
# 
#   sr <- port_ret/port_sd
#   sharpe_ratio <- sr
# }

# Storing the values in the table
portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           SharpeRatio = sharpe_ratio)

#Converting matrix to tibble
all_wts <- tk_tbl(wts)
colnames(all_wts) <- colnames(log_ret_xts)

# Combing all the values together
portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))

#Minimum variance portfolio
min_var <- portfolio_values[which.min(portfolio_values$Risk),]
obj$min_var = min_var

#Plot weights
vars <- names(min_var)[names(min_var) %in% c("Return", "Risk", "SharpeRatio") == FALSE]
obj$vars = vars
p <- min_var %>%
  gather(vars, key = Asset, ##sollte hier alles nehmen, aber es nimmt nur GIS und PFE
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = Asset, y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Minimum Variance Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 

#ggplotly(p)
obj$plot_minvar = p

#tangency portfolio (the portfolio with highest sharpe ratio)
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]
obj$max_sr = max_sr

p <- max_sr %>%
  gather(vars, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = Asset, y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Tangency Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 

#ggplotly(p)
obj$plot_maxsr = p

#Efficient frontier
p <- portfolio_values %>%
  ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Annualized Risk',
       y = 'Annualized Returns',
       title = "Portfolio Optimization & Efficient Frontier") +
  geom_point(aes(x = Risk, 
                 y = Return), data = min_var, color = 'orange') +
  geom_point(aes(x = Risk,
                 y = Return), data = max_sr, color = 'red') +
  annotate('text', x = 0.22, y = 0.42, label = "Tangency Portfolio") +
  annotate('text', x = 0.32, y = 0.01, label = "Minimum variance portfolio") #+
  #annotate(geom = 'segment', x = 0.14, xend = 0.135,  y = 0.01, 
  #         yend = 0.06, color = 'red', arrow = arrow(type = "open")) +
  #annotate(geom = 'segment', x = 0.22, xend = 0.2275,  y = 0.405, 
  #         yend = 0.365, color = 'red', arrow = arrow(type = "open"))


#ggplotly(p)
obj$plot_efficient_frontier = p
}
return(obj)

}

res = efficienc_frontier(tick = tick)
res2 = efficienc_frontier(tick = tick_vgl, wts = wt)

##Vergleich des Risikos und Returns der beiden Portfolios
res2$min_var$Risk/res$min_var$Risk
res2$min_var$Return/res$min_var$Return
res2$max_sr$Return/res$max_sr$Return
res2$max_sr$Risk/res$max_sr$Risk
