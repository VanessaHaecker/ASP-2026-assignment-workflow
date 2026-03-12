#!/usr/bin/env Rscript

# Project: ASML News Analysis (Hard vs. Soft News)
# Script 03: ASML Return Analysis (Volatility / Absolute Returns)

rm(list = ls()) 

# 1 - Load necessary libraries

if (!require("pacman")) install.packages("pacman"); library("pacman")

pacman::p_load(
  tidyverse,  # For tidy data manipulation
  scales,     # For percentage formatting in plots
  here        # For project structure management
)


here::i_am("01-asml-project/code/03-return-analysis.R")
root <- here::here()

# Define relative paths for the project structure
input_dir  <- file.path(root, "01-asml-project", "input")
output_dir <- file.path(root, "01-asml-project", "output")

# 2 - Load data

prices <- read_csv(file.path(input_dir, "asml_prices.csv"))
news   <- read_csv(file.path(input_dir, "asml_news_classified_FINAL.csv"))

# 3 - Aggregate data at daily level to determine the dominant news type per day

daily_news_summary <- news %>%
  mutate(category = str_trim(category)) %>% 
  filter(category %in% c("Hard", "Soft")) %>% 
  group_by(date) %>%
  summarise(
    hard_count = sum(category == "Hard", na.rm = TRUE),
    soft_count = sum(category == "Soft", na.rm = TRUE),
    total_news = hard_count + soft_count,
    
    # Determine the dominant news type for the day, with a tie-breaker for "Mixed"
    main_info_type = case_when(
      hard_count > soft_count ~ "Hard",
      soft_count > hard_count ~ "Soft",
      hard_count == soft_count & total_news > 0 ~ "Mixed", 
      TRUE ~ "None"
    )
  ) %>%
  
  filter(main_info_type %in% c("Hard", "Soft", "Mixed"))


# 4 - Merge with stock data

analysis_data <- prices %>%
  inner_join(daily_news_summary, by = "date") %>%
  mutate(abs_return = abs(daily_return)) # Absolute return as a measure of volatility


# 5 - Descriptive statistics: volatility by news type

cat("\n=== Descriptive Statistics: Volatility by News Type ===\n")
stats_summary <- analysis_data %>%
  group_by(main_info_type) %>%
  summarise(
    n_days = n(),
    avg_volatility = mean(abs_return, na.rm = TRUE),
    sd_volatility = sd(abs_return, na.rm = TRUE)
  ) %>%
  arrange(desc(n_days))

print(stats_summary)


# 6 - Visualizations
# Define uniform color palette (including Mixed)
custom_colors <- c("Hard" = "#e74c3c", "Soft" = "#3498db", "Mixed" = "#95a5a6")

# A) Boxplot of Volatility
plot_box <- ggplot(analysis_data, aes(x = main_info_type, y = abs_return, fill = main_info_type)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 8) +
  theme_minimal() +
  scale_fill_manual(values = custom_colors) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title = "Market Reaction: Hard vs. Soft News (ASML)",
    x = "Dominant News Type",
    y = "Daily Absolute Return",
    fill = "Category"
  )

# Display plot in VS Code
print(plot_box)

# Save graphic in high resolution
ggsave(
  filename = file.path(output_dir, "asml_volatility_comparison.png"), 
  plot = plot_box, 
  device = "png",
  width = 8, 
  height = 6, 
  units = "in",
  dpi = 300
)
print("Graphic 'asml_volatility_comparison.png' saved in output folder.")

# B) Time series of volatility
plot_ts <- ggplot(analysis_data, aes(x = date)) +
  geom_rect(aes(xmin = date - 0.5, xmax = date + 0.5, 
                ymin = 0, ymax = Inf, fill = main_info_type), 
            alpha = 0.3) +
  geom_line(aes(y = abs_return), color = "#2c3e50", linewidth = 1) +
  geom_point(aes(y = abs_return), color = "#2c3e50", size = 2.5) +
  scale_fill_manual(values = custom_colors) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1), limits = c(0, NA)) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  theme_minimal() +
  labs(
    title = "ASML Time Series Daily Absolute Returns: Hard vs. Soft News",
    subtitle = "Absolute daily returns",
    x = "Date",
    y = "Daily Absolute Return",
    fill = "Dominant News"
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

print(plot_ts)

ggsave(
  filename = file.path(output_dir, "asml_volatility_timeseries.png"), 
  plot = plot_ts, 
  device = "png",
  width = 10, 
  height = 5, 
  units = "in",
  dpi = 300
)
print("Graphic 'asml_volatility_timeseries.png' saved in output folder.")

# C) Bar chart of volatility shocks
plot_bar <- ggplot(analysis_data, aes(x = date, y = abs_return, fill = main_info_type)) +
  geom_col(width = 0.6, alpha = 0.85) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  scale_fill_manual(values = custom_colors) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1), expand = expansion(mult = c(0, 0.1))) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  theme_minimal() +
  labs(
    title = "Absolute Daily Returns over Time by Dominant News Type",
    x = "Date",
    y = "Daily Absolute Return",
    fill = "Dominant News"
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold")
  )

print(plot_bar)

ggsave(
  filename = file.path(output_dir, "asml_volatility_bars.png"), 
  plot = plot_bar, 
  device = "png",
  width = 10, 
  height = 5, 
  units = "in",
  dpi = 300
)
print("Graphic 'asml_volatility_bars.png' saved in output folder.")

# 7 - Extra Check: Which days were "Mixed" and what was the return?

cat("\n=== Detailed view: Days with 'Mixed' News ===\n")

mixed_news_check <- analysis_data %>%
  # Filter only Mixed days
  filter(main_info_type == "Mixed") %>%
  # Select relevant columns for a clear table
  select(date, hard_count, soft_count, daily_return, abs_return) %>%
  # Sort by date
  arrange(date)

print(mixed_news_check)


