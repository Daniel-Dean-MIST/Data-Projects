#install.packages('tidyquant')
library(tidyquant)
library(ggplot2)
library(dplyr)
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
# Downloading Apple price using quantmod

#Analaysis from 2015-start of 2021 S&P 500 Companies
tickers = c('AAPL', 'MSFT', 'AMZN', 'FB', 'GOOGL', 'GOOG', 'BRK.B', 'TSLA', 'NVDA', 
            'JPM', 'JNJ', 'V', 'UNH', 'PYPL', 'HD', 'PG', 'MA', 'DIS', 'BAC', 'ADBE',
            'CMCSA', 'XOM', 'NFLX', 'VZ', 'KO', 'PFE', 'PEP', 'NKE', 'CVX', 'WMT')

#takes a ticker symbol and returns a stock object from beginning 2015-2021
get_info <- function(ticker_symbol){
  stock <- tq_get(ticker_symbol,
                from = "2015-01-01",
                to = "2021-01-20",
                get = "stock.prices")
  
  return(stock)
}#get_info

stock <- get_info('qqq')
View(stock)

#takes in stock variable and does calculations to see if setting a lower than open 
#buy limit is worth it,
# and if so, how low should it be as a percentage?
augment_data <- function(stock){
  stock <- stock %>%
    summarize(stock, low_open=100-(low/open*100),
              stock, low_close=100-(low/close*100),
              stock, mean_open_low=(mean(low) / mean(open)-1)*-100)
  
  return(stock)
}#augment_data

#graphs the opening price, low price, and close price
graph_open_low_close <- function(stock){
  ggplot(data=stock)+
    geom_smooth(mapping=aes(x=date,y=open),color="yellow") + 
    geom_smooth(mapping=aes(x=date,y=close),color="green") +
    geom_smooth(mapping=aes(x=date,y=low),color="red")
  #facet_wrap(~symbol)
}#graph_open_low_close

#pulls stock info from ticker.
#adds low_open, low_close, and mean_low_open variables to dataframe
#returns the formulated dataframe as an object
summarize_stock <- function(stock){
  stock <- get_info(stock)
  stock <- augment_data(stock)
  
  return(stock)
}#summarize_stock

#tech heavy market ETF
qqq <- get_info('QQQ')
qqq <- augment_data(qqq)
graph_open_low_close(qqq)
View(qqq)

qqq[1, 1:11]

#s&p 500 market ETF
spy <- summarize_stock('SPY')
graph_open_low_close(spy)
View(spy)

#total stock market ETF
vti <- summarize_stock('VTI')
graph_open_low_close(vti)
view(vti)

#gets mean_discount_price for each of our 3 ETFs
vti_discount <- vti %>%
  select(mean_open_low) %>%
  distinct(mean_open_low)

spy_discount <- spy %>%
  select(mean_open_low) %>%
  distinct(mean_open_low)

qqq_discount <- qqq %>%
  select(mean_open_low) %>%
  distinct(mean_open_low)

qqq_discount <- as.double(qqq_discount)
spy_discount <- as.double(spy_discount)
vti_discount <- as.double(vti_discount)
#as.double only gets the value! wohoo!
as.double(qqq_discount)

#puts the value of each discount as a double value into a vector
x = c(as.double(qqq_discount), as.double(spy_discount), 
      as.double(vti_discount))

#mean_open_low synopsis for each ETF
market_df <- data.frame(symbol = c('QQQ', 'SPY', 'VTI'),
                stock_discount_percent = x)
                        
View(market_df)

#shows the average daily discount for each of our 3 ETFs
ggplot(data=market_df, aes(x=symbol, y=stock_discount_percent, fill=symbol)) +
  geom_bar(stat="identity")

#putting it all together, you would make significantly more
#year over year buy using limit buy orders around these
#daily volatility discount rates

#assuming a 7% compounding interest rate on the stock market,
#these discounts used at their maximum potential would increase
#the compounding interest rate by their stock_discount_percent

compound_interest_rate <- 7

discount_compound_interest_rate <- c(qqq_discount + compound_interest_rate,
                                     spy_discount + compound_interest_rate,
                                     vti_discount + compound_interest_rate)

discount_compound_interest_rate

#percent_increase_in_compound_interest_rate

compound_the_stock <- function(stock_df, interest_rate, num_periods){
  
  stock_df <- stock_df[1, 1:11]
  print(stock_df)
  stock_df <- stock_df %>%
    mutate(year_1_discount_compounded = open*(1+ mean_open_low/100 + interest_rate/100)^num_periods)
  return(stock_df)
}#compound_the_stock

#compounds a present value by the base interest rate + discount rate for as many periods as desired
compound_the_pv <- function(stock_df, pv, interest_rate, num_periods){
  
  stock_df <- stock_df[1, 1:11]
  
  stock_df <- stock_df %>%
    mutate(discount_compounded = pv*(1+ mean_open_low/100 + interest_rate/100)^num_periods)
  
  return(stock_df)
}#compound_the_pv

qqq_clone <- qqq
qqq_clone <- compound_the_stock(qqq, 7, 6)

View(qqq_clone)

qqq_pv_clone <- qqq
qqq_pv_clone <- compound_the_pv(qqq_clone, 6000, 7-.747, 40)

View(qqq_pv_clone)

#3 data frames, 1 for each ETF. 1 row discount+interest rate, 1 row base interest rate
print(1:100)

#compounds a present value by the base interest rate + discount rate for as many periods as desired
compound_the_pv_2 <- function(stock_df, pv){
  
  stock_df <- stock_df %>%
    summarize(year=year,
              discounted_current_price = pv*(1+ interest_rate)^year,
              discounted_interest_rate = interest_rate,
              previous_price = current_price/(1+interest_rate),
              base_interest_rate = 7/100,
              base_current_price = pv*(1+ base_interest_rate)^year,
              percent_gained_value = (base_current_price / discounted_current_price-1)*-100)
  
  return(stock_df)
}#compound_the_pv

#new dataframe for each ETF to track growth over 45 years
qqq_df = data.frame(year=c(0:45), current_price = c(100), interest_rate=c((qqq_discount+7)/100), previous_price=c(100))
spy_df = data.frame(year=c(0:45), current_price = c(100), interest_rate=c((spy_discount+7)/100), previous_price=c(100))
vti_df = data.frame(year=c(0:45), current_price = c(100), interest_rate=c((vti_discount+7)/100), previous_price=c(100))

qqq_df <- compound_the_pv_2(qqq_df, 6000)
spy_df <- compound_the_pv_2(spy_df, 6000)
vti_df <- compound_the_pv_2(vti_df, 6000)

#plotting discounted stock growth vs base stock growth
ggplot(data=qqq_df)+
  geom_smooth(mapping=aes(x=year,y=discounted_current_price, color="Limit Order")) + 
  geom_smooth(mapping=aes(x=year,y=base_current_price, color="Market Order")) +
  scale_color_manual(values=c('Green', 'Orange')) +
  labs(x = "Years of Compounding Interest",
       y = "Value of Investment $",
       title = "QQQ Limit Order vs Market Order Value Over Time")

#plotting discounted stock growth vs base stock growth
ggplot(data=spy_df)+
  geom_smooth(mapping=aes(x=year,y=discounted_current_price, color="Limit Order")) + 
  geom_smooth(mapping=aes(x=year,y=base_current_price, color="Market Order")) +
  scale_color_manual(values=c('Green', 'Orange')) +
  labs(x = "Years of Compounding Interest",
       y = "Value of Investment $",
       title = "SPY Limit Order vs Market Order Value Over Time")

#plotting discounted stock growth vs base stock growth
ggplot(data=vti_df)+
  geom_smooth(mapping=aes(x=year,y=discounted_current_price, color="Limit Order")) + 
  geom_smooth(mapping=aes(x=year,y=base_current_price, color="Market Order")) +
  scale_color_manual(values=c('Green', 'Orange')) +
  labs(x = "Years of Compounding Interest",
       y = "Value of Investment $",
       title = "VTI Limit Order vs Market Order Value Over Time")
