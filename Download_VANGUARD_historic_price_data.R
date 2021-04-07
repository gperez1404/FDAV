
# Scraping and Analyzing Vangaurd ETFs :

# this scrip downlaod historical data of buy/sell prices for a especific product

# references:

# https://rstudio-pubs-static.s3.amazonaws.com/392483_2e403b6b7f4e473b8e87461aa9674599.html



library(httr)
library(xml2)

#  packages for scraping JAvaScrip libraries: 
library(rvest)
library(stringr)
library(tidyverse)
library(purrr)
library(here)
library(beepr)
library(DT)


target_link <- "https://www.vanguard.com.au/personal/products/en/detail/8101/prices-and-distributions"
url <- GET(target_link)


# Function for scraping from a URL
js_scrape <- function(url, 
                      js_path = "scrape.js", 
                      phantompath = "/usr/local/Cellar/phantomjs/2.1.1/bin/phantomjs"){
  
  # Replace url in scrape.js
  lines <- readLines(js_path)
  lines[1] <- paste0("var url ='", url ,"';")
  writeLines(lines, js_path)
  
  # Run from command line
  command <- paste(phantompath, js_path, sep = " ")
  system(command)
  
}

# Scrape it :

js_scrape(url = target_link)

