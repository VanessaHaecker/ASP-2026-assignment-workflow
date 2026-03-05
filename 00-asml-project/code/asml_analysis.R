#!/usr/bin/env Rscript

# Project: ASML News  Analysis (Hard vs. Soft News)
# Path: 00-asml-project/code/asml_analysis.R

rm(list = ls()) # Clear the environment
setwd("00-asml-project")

# 1 - Load necessary libraries
if (!require("pacman")) install.packages("pacman"); library ("pacman")

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

# 5 - Plot Time Series Returns
# 5.1 - Create the plot
returns_plot <- ggplot(price_data, aes(x = date, y = daily_return)) +
  geom_line(color = "#0073C2FF", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = paste(ticker, "Daily Returns"),
    subtitle = paste(start_date, "to", end_date),
    x = "Date",
    y = "Daily Return (%)",
    caption = "Data source: Yahoo Finance"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

# 5.2 - Display the plot in VS Code
print(returns_plot)


# 5.3 - Save the graphic as a JPEG image to the 'output' folder
ggsave(
  filename = file.path(output_dir, "asml_returns_plot.jpg"),
  plot = returns_plot,
  device = "jpeg",   # Specifies the image type
  width = 10, 
  height = 6, 
  units = "in",
  dpi = 300          # High resolution for a crisp image
)

# 6 - News Data: Fetch recent company news headlines from Finnhub
# 6.1 - Set API Key (Consider using Sys.getenv("FINNHUB_KEY") for shared environments)
finnhub_key <- "d6k8lu1r01qko8c3sr60d6k8lu1r01qko8c3sr6g" 

# 6.2 - Fetch news data using a clean query list
# Using as.character() ensures the ymd objects are correctly formatted for the URL
response <- GET(
  url = "https://finnhub.io/api/v1/company-news",
  query = list(
    symbol = ticker,
    from   = as.character(start_date),
    to     = as.character(end_date),
    token  = finnhub_key
  )
)

# 6.3 - Check for successful API response
if (status_code(response) != 200) {
  stop("API Error: Check your Finnhub Key or network connection.")
}

# 6.4 - Parse and clean news data in one pipeline
news_data <- content(response, as = "text", encoding = "UTF-8") %>%
  fromJSON() %>%
  as_tibble() %>%
  mutate(
    # Convert Unix timestamp to POSIXct and then a Date object
    datetime = as_datetime(datetime),
    date     = as.Date(datetime)
  ) %>%
  # Reproducibility check: Ensure data is strictly within the window
  filter(date >= start_date & date <= end_date) %>%
  select(date, datetime, headline, summary, source) %>%
  arrange(desc(datetime))

# 6.5 - Check if data was returned
if (nrow(news_data) == 0) {
  message("Warning: No news found for this timeframe.")
} else {
  message("Successfully fetched ", nrow(news_data), " headlines.")
}

# 6.6 - Preview the first few headlines
print(head(news_data))

# 6.7 - Save the news data to the 'input' folder
write_csv(news_data, file.path(input_dir, "asml_news_headlines.csv"))

# 7 - Plot News Density (Visualizing 'Soft News' frequency)
news_density_plot <- news_data %>%
  count(date) %>%
  ggplot(aes(x = date, y = n)) +
  geom_col(fill = "steelblue", width = 0.7) +
  # Force x-axis to respect the exact range even if some days have zero news
  scale_x_date(
    limits = c(start_date, end_date), 
    date_labels = "%b %d", 
    date_breaks = "1 week"
  ) +
  theme_minimal() +
  labs(
    title = paste(ticker, "News Coverage Density"),
    subtitle = paste(start_date, "to", end_date),
    x = "Date",
    y = "Number of Headlines",
    caption = "Data source: Finnhub API"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

# 7.1 - Display and save the density plot
print(news_density_plot)
ggsave(
  filename = file.path(output_dir, "asml_news_density_plot.jpg"),
  plot = news_density_plot,
  device = "jpeg",
  width = 10, 
  height = 6, 
  dpi = 300
)


















