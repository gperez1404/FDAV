

#  This scrip has two parts :

# Part 1: extraction of historical sell/buy prices of VANGUARD products 
# domiciled in Australia 

# Part 2: extractrion and  anlysis of ETFs funds VANGUARD US

#----------------------------------------------------------------------------------------
# part 1

# Scraping and Analyzing Vangaurd ETFs :

# This aprt of the script is to downlaod historical data of buy/sell prices for a especific VANGUARD product

# References:

# Scraping Javascript websites in R

# https://blog.brooke.science/posts/scraping-javascript-websites-in-r/


library(httr)
library(xml2)


link <- "https://www.vanguard.com.au/personal/products/en/detail/8101/prices-and-distributions"
r <- GET(link)
doc <- read_html(content(r, "text"))



#  dunno how to proceed  ?....

#------------------------------------------------------------------------------------------
# part 2 
# this has been modified from original reference to run in windows

# References:

# Scraping and Analyzing Vangaurd ETFs with R (This is for US domiciled Vanguard ETFs)

# https://rstudio-pubs-static.s3.amazonaws.com/392483_2e403b6b7f4e473b8e87461aa9674599.html

#  libraries for scraping JavaScrip: 

library(purrr)
library(here)
library(beepr)
library(DT)

# libraries  for handling dataframes:

library(dplyr)
library(rvest)
library(stringr)
library(tidyr)
library(quantmod)
library(lubridate)

# libraries  for plotting and analysing data:
library(readr)
library(ggplot2)
require("ggrepel")

# This is also useful if you want to resize your plots or copy them into the clipboard 
options(device = "windows")
cat("\014")


#-------------------------------------------------------------------------------------------
# file paths and folder locations:

base_directory<-"C:/00-C-GPM_INFO/04-C-RDM/04-C-01-R/04-C-01-08-FINANCIAL_DATA_EXPLORER"
setwd(paste(base_directory,"/FDAV",sep=""))
main_directory<- getwd()

# Folder locations for results:
results_fp<-paste(base_directory,"02-Results-files",sep="/")

#------------------------------------------------------------------------------------------

# Here you create a Javascript file to downlaod the content of the website : scrape.js
writeLines("var url = NULL;
           var page = new WebPage();
           var fs = require('fs');
           page.open(url, function (status) {
           just_wait();
           });
           function just_wait() {
           setTimeout(function() {
           fs.write('1.html', page.content, 'w');
           phantom.exit();
           }, 2500);
           }
           ", 
           con = "scrape.js")

# Function for scraping from a URL
js_scrape <- function(url,js_path,phantompath){
  
  # Replace url in scrape.js
  lines <- readLines(js_path)
  lines[1] <- paste0("var url ='", url ,"';")
  writeLines(lines, js_path)

  # Run from command line
  command <- paste(phantompath, js_path, sep = " ")
  print("downloading webpage, please wait....")
  system(command)
  print("html object created successfully !")
}

#---------------------------------------------------------------------------------------------------
# Scrape the website:

js_file_path<-paste(main_directory,"scrape.js",sep="/")

# you need to dowlad and extract PhantomJS from this website: https://phantomjs.org/download.html
js_phantom_path<-paste(base_directory,"/00-packages/phantomjs-2.1.1-windows/bin/phantomjs",sep="")

js_scrape(url = "https://investor.vanguard.com/etf/list#/etf/asset-class/month-end-returns",
          js_path= js_file_path,
          phantompath = js_phantom_path)

#--------------------------------------------------------------------------------------------------

# load the html object to start extrating data
html <- read_html("1.html")

# Extract fund names
fund_names <- html %>%
  html_nodes(".productName a") %>%
  html_text()

# Extract tickers
ticker <- html %>%
  html_nodes(".fixedCol+ .ng-binding") %>%
  html_text()

# Extract asset classes
asset_class <- html %>%
  html_nodes(".assetClass") %>%
  html_text() %>%
  sapply(., function(x) x[!str_detect(x, "\t")]) %>%
  unlist()

# Extract expense ratios
expense_ratio <- html %>%
  html_nodes(".expenseRatio") %>%
  html_text()  %>%
  sub("%", "", .) %>%
  as.numeric() %>%
  sapply(., '/', 100) %>%
  sapply(., function(x) x[!is.na(x)]) %>%
  unlist()

# Extract price
price <- html %>%
  html_nodes(".expenseRatio+ .ng-binding") %>%
  html_text()  %>%
  sub("[$]", "", .) %>%
  as.numeric() 

# Extract sec yield
sec_yield <- html %>%
  html_nodes(".secYield") %>%
  html_text()
sec_yield_clean <- sec_yield[!str_detect(sec_yield, "SEC")] %>%
  str_replace_all(., "\n", "") %>%
  str_replace_all(., "\t", "") %>%
  str_replace_all(., "â\u0080\u0094", NA_character_) 

# Extract ytd returns
ytd <- html %>%
  html_nodes(".secYield+ .ng-binding") %>%
  html_text() %>%
  sub("%", "", .) %>%
  as.numeric() %>%
  sapply(., '/', 100)

# Extract one yr returns
one_yr <- html %>%
  html_nodes(".ng-binding:nth-child(11)") %>%
  html_text() %>%
  str_replace_all(., "â\u0080\u0094", NA_character_) %>%
  sub("%", "", .) %>%
  as.numeric() %>%
  sapply(., '/', 100)

# Extract five yr returns
five_yr <- html %>%
  html_nodes(".ng-binding:nth-child(12)") %>%
  html_text() %>%
  str_replace_all(., "â\u0080\u0094", NA_character_) %>%
  sub("%", "", .) %>%
  as.numeric() %>%
  sapply(., '/', 100)

# Extract ten yr yields
ten_yr <- html %>%
  html_nodes(".ng-binding:nth-child(13)") %>%
  html_text() %>%
  str_replace_all(., "â\u0080\u0094", NA_character_) %>%
  sub("%", "", .) %>%
  as.numeric() %>%
  sapply(., '/', 100)

# Extract yield since inception
since <- html %>%
  html_nodes(".right:nth-child(14)") %>%
  html_text() %>%
  str_replace_all(., "\n", "") %>%
  str_replace_all(., "\t", "") %>%
  str_split(., "[(]") %>%
  lapply(., head, 1) %>%
  unlist() %>%
  sub("%", "", .) %>%
  as.numeric() %>%
  sapply(., '/', 100)

# Extract date of inception
inception <-  html %>%
  html_nodes(".right:nth-child(14)") %>%
  html_text() %>%
  str_replace_all(., "\n", "") %>%
  str_replace_all(., "\t", "") %>%
  str_split(., "[(]") %>%
  lapply(., tail, 1) %>%
  str_replace_all(., "[)]", "") %>%
  unlist() %>%
  as.Date(., "%m/%d/%Y")

# Combine into one data frame
fund_data.df <- data.frame(fund_names, ticker, asset_class,expense_ratio, price, sec_yield_clean, ytd, one_yr, five_yr, ten_yr, since, inception,stringsAsFactors = FALSE) 

# Drop duplicate rows
fund_data.df <- fund_data.df %>%distinct()

# Here you get the historical data for all funds
all_funds <- list()
number_of_funds<-length(fund_data.df$ticker)
for(i in seq_along(fund_data.df$ticker)) {
  
  print(paste("Extracting time series for fund: ", as.character(fund_data.df$ticker[i]),"....",as.character(i)," out of ",as.character(number_of_funds),sep=""))
  # Download data indto df
  all_funds[[i]] <- getSymbols(fund_data.df$ticker[i], src = "yahoo",
                               auto.assign = FALSE) %>%
    as.data.frame()
  # Add ticker column
  all_funds[[i]]$ticker <- fund_data.df$ticker[i]
  # Add date column
  all_funds[[i]]$date <- date(row.names(all_funds[[i]]))
  # Standardize names
  names(all_funds[[i]]) <- c("open", "high", "low", "close",
                             "volume", "adjusted", "ticker", "date")
}

# Here you combien the historical data into a Combine into a single  df
fund_history.df <- bind_rows(all_funds)

#------------------------------------------------------------------

# here you export the data to  excel files

file_name<-paste("VANG-fund-data-",Sys.Date(),".csv",sep="")
file_path<-paste(results_fp,file_name,sep="/")
write.csv(fund_data.df,file_path,row.names = FALSE)

file_name<-paste("VANG-funds-historical-data-",Sys.Date(),".csv",sep="")
file_path<-paste(results_fp,file_name,sep="/")
write.csv(fund_history.df,file_path,row.names = FALSE)


#-----------------------------------------------------------------

# here you plot the Year-to-date returns of all the funds:

# This allows you to see which funds have lost money this year (left bars)

fund_data.df %>%
  ggplot() +
  aes(x = reorder(fund_names, ytd), y = ytd) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("") +
  ggtitle("Year-to-date returns") +
  theme(axis.text = element_text(size = 8))

# here you crete a scatter plot with annual return Vs year of inception:

# Note: There is greater variance in the average annual returns of newer 
# funds than newer funds.

# it is worth noting that the odler funds are not neccesary the more profitable

ggplot(data=fund_data.df, aes(x=inception, y=since)) + 
  geom_point(color = "red")+
  geom_text_repel(aes(label = ticker),col = 'blue',size = 5) +
  geom_hline(yintercept = 0,col = 'red',lwd = 1.5,lty = 2)+
  ylab("Average Annual Returns") +
  scale_x_date("Year of Inception",date_breaks = "1 year", date_labels = "%Y") +
  ggtitle("Average Annual Returns for Vangaurd ETFs") +
  theme(axis.text.x = element_text(size = 12))  

# here you plot the funds return expense ratios

# Note: This plot help you to identify the most "economic funds" 

#       Funds that have high returns but high expense ratios are "expensive"
#       and can leave you with less money in the long run when compared to funds with 
#       lower returns and lower expense ratios

fund_data.df %>%
  select(fund_names, expense_ratio, asset_class) %>%
  mutate(`Asset Class` = sapply(str_split(asset_class, " "), head, 1)) %>%
  ggplot() +
  aes(x = reorder(fund_names, expense_ratio), y = expense_ratio, fill = `Asset Class`) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("") +
  ggtitle("International funds have higher expense ratios") +
  theme(axis.text.y = element_text(size = 8)) +
  scale_fill_hue(c = 56, l = 56) 

# Here you look at the historical performance of all funds to see if any stands out.

# Warning:
# This plot contains more than 70  time series and there is no legend (hard to read)
#  You need to manually edit the limit of the Y scale (for 2021 8% is good) 
fund_history.df %>% group_by(ticker) %>% mutate(scaled_close = close/close[date == min(date)] - 1) %>% ungroup() %>%
  ggplot() +
  aes(x = date, y = scaled_close, col = ticker) +
  geom_line(lwd = 0.5) +
  scale_y_continuous("Percent of initial",limits = c(-1, 8),breaks = seq(-1, 8, 1)) +
  scale_x_date("Date",date_breaks = "1 year", date_labels = "%Y") +
  ggtitle("Historical performance of Vangaurd ETFs (Us domiciled only)") +
  theme(axis.text.x = element_text(size = 10),panel.grid = element_blank()) +
  scale_color_discrete(guide = FALSE)


# The VGT fund (Vanguard Information Technology ETF) seems to have broken away 
# from the pack recently, and has been around since 2007. However it’s very volatile

# The typical benchamrk is : VOO = S&P 500 ETF tracker

ticker_interest_ETF<-"VGT"
list_of_interest_ETFs<-c("VOO",ticker_interest_ETF)

# here you filter out the interest funds and then plot them with legend
fund_history.df %>%filter(ticker %in% list_of_interest_ETFs) %>% group_by(ticker) %>% mutate(close = close/close[date == min(date)] - 1) %>% ungroup() %>%
  ggplot() +
  aes(x = date, y = close, col = ticker) +
  geom_line() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  ggtitle(paste("Historical performance of ",ticker_interest_ETF," Vs S&P500 ETF (VOO)",sep="")) +
  theme(axis.text.x = element_text(size = 8))


# here you look  at the overall correlation between a specific ETFand all the other funds.
# this is useful if you want to hedge the risk by investign in products ataht go in the opposite directaion than stokcs (e.i bonds)

interest_ETF_data <- fund_history.df %>% filter(ticker == ticker_interest_ETF)

cor_with_interest_ETF <- fund_history.df %>% group_by(ticker) %>% 
  summarise(correlation = cor(close,interest_ETF_data$close[interest_ETF_data$date >= min(date) &interest_ETF_data$date <= max(date)],use = "complete.obs"))

# check the list of the Top 10 ETFs in therms of negative correlations
cor_with_interest_ETF %>%arrange(correlation) %>% head(10)

