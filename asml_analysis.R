#!/usr/bin/env Rscript

# Project: ASML News  Analysis (Hard vs. Soft News)
# Path: 00-asml-project/code/asml_analysis.R

rm(list = ls()) # Clear the environment
setwd("00-asml-project")

# 1 - Load necessary libraries
if (!require("pacman")) install.packages("pacman"); library ("pacman")
install.packages("tidyverse")
pacman::p_load(
  quantmod,   # For stock price data
  httr,       # For API calls
  jsonlite,   # For parsing JSON
  dplyr,      # For data manipulation
  ggplot2,    # For plotting
  lubridate,  # For date handling
  ellmer,     # For the LLM part
  tidyverse,  # For tidy data manipulation
  lubridate   # For date handling
)

# 2 - Define relative paths for the project structure
input_dir  = "input"
temp_dir   = "temp"
output_dir = "output"

# 3 - Set the ticker
ticker  = "ASML"
start_date <- ymd("2026-01-31") 
end_date   <- ymd("2026-03-03") # 30 days later

# 4 - Fetch historical stock price data from Yahoo Finance
# subtract 1 from the start date to have the previous day's price for return calculations
getSymbols(ticker, src = "yahoo", from = start_date - 1, to = end_date)

# 4.1 - Create data frame from the fetched data
price_data <- data.frame(
  date     = index(get(ticker)),
  open     = as.numeric(Op(get(ticker))),
  high     = as.numeric(Hi(get(ticker))),
  low      = as.numeric(Lo(get(ticker))),
  close    = as.numeric(Cl(get(ticker))),
  volume   = as.numeric(Vo(get(ticker))),
  adjusted = as.numeric(Ad(get(ticker)))
)

# 4.2 - Calculate daily returns and remove the first row (since it will have NA for return)
price_data <- price_data %>%
  mutate(daily_return = (adjusted / lag(adjusted) - 1)) %>%
  filter(date >= start_date)

# 4.3 - Inspect the price data
print(head(price_data))

# 4.4 - Save the price data to the 'input' folder
write_csv(price_data, file.path(input_dir, "asml_prices.csv"))


# 5 - News Data: Fetch recent company news headlines
finnhub_key <- "d6k8lu1r01qko8c3sr60d6k8lu1r01qko8c3sr6g" 

# Construct the API URL
url <- paste0("https://finnhub.io/api/v1/company-news?symbol=", ticker, 
              "&from=", start_date, "&to=", end_date, "&token=", finnhub_key)

# Make the API call
response <- GET(url)

# Error handling for the API response
if (status_code(response) != 200) {
  stop("API Error: Check your Finnhub Key or network connection.")
}

# Parse and create dataframe in one pipeline
news_data <- fromJSON(content(response, as = "text", encoding = "UTF-8")) %>%
  as.data.frame() %>%
  mutate(date = as.Date(as.POSIXct(as.numeric(datetime), origin = "1970-01-01"))) %>%
  select(date, datetime, headline, summary)

# Check if data exists
if (nrow(news_data) == 0) stop("No news found for this timeframe.")

# HEAD: View the first news item as requested
cat("\n--- Preview of First 5 Headlines ---\n")
news_data %>% 
  mutate(short_headline = paste0(substr(headline, 1, 50), "...")) %>% 
  select(date, short_headline) %>% 
  head(5) %>% 
  print(row.names = FALSE)
cat("------------------------------------\n")

# SAVE: Store to input
write_csv(news_data, file.path(input_dir, "asml_news_headlines.csv"))

# 4. VISUALIZATION
p <- news_data %>%
  count(date, name = "num_headlines") %>%
  ggplot(aes(x = date, y = num_headlines)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  labs(title = paste("News Density for", ticker),
       subtitle = paste("Period:", start_date, "to", end_date),
       x = "Date", y = "Number of Headlines")

ggsave(file.path(output_dir, "news_density_plot.png"), p, width = 8, height = 5)

plot(p) 
