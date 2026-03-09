#!/usr/bin/env Rscript

# Project: ASML News Analysis (Hard vs. Soft News)
# Script 04: ASML Directional Return Analysis (Up/Down & Magnitude)
# Path: 01-asml-project/code/04-directional-analysis.R

rm(list = ls()) # Clear the environment

# ==============================================================================
# 1 - Load necessary libraries
# ==============================================================================
if (!require("pacman")) install.packages("pacman"); library("pacman")

pacman::p_load(
  tidyverse,  # For tidy data manipulation
  scales,     # For percentage formatting in plots
  here        # For project structure management
)

# Path setup for reproducibility
here::i_am("01-asml-project/code/04-directional-analysis.R")

# Define the root ONLY ONCE at the beginning of the script
root <- here::here()

# Define relative paths for the project structure
input_dir  <- file.path(root, "01-asml-project", "input")
output_dir <- file.path(root, "01-asml-project", "output")

# ==============================================================================
# 2 - Load & Prepare Data
# ==============================================================================
prices <- read_csv(file.path(input_dir, "asml_prices.csv"))
news   <- read_csv(file.path(input_dir, "asml_news_classified_FINAL.csv"))

# PREPARATION: Clean classification of the dominant news type
daily_news_summary <- news %>%
  mutate(category = str_trim(category)) %>% 
  filter(category %in% c("Hard", "Soft")) %>% 
  group_by(date) %>%
  summarise(
    hard_count = sum(category == "Hard", na.rm = TRUE),
    soft_count = sum(category == "Soft", na.rm = TRUE),
    total_news = hard_count + soft_count,
    
    # NEW LOGIC: Which type truly dominates?
    main_info_type = case_when(
      hard_count > soft_count ~ "Hard",
      soft_count > hard_count ~ "Soft",
      hard_count == soft_count & total_news > 0 ~ "Mixed", 
      TRUE ~ "None"
    )
  ) %>%
  filter(main_info_type %in% c("Hard", "Soft", "Mixed"))

# Merge the datasets
analysis_data <- prices %>%
  inner_join(daily_news_summary, by = "date")

# ==============================================================================
# 3 - Methodology: Base metrics for the analysis
# ==============================================================================
analysis_data <- analysis_data %>%
  mutate(
    actual_return = daily_return,
    abs_return = abs(daily_return),
    return_direction = case_when(
      daily_return > 0 ~ "Up",
      daily_return < 0 ~ "Down",
      TRUE ~ "Flat"
    ),
    is_positive_day = ifelse(daily_return > 0, 1, 0)
  )

# ==============================================================================
# 4 - Descriptive Analysis 1: Means & Distribution
# ==============================================================================
cat("\n=== Analysis 1: Average Return by News Type ===\n")

directional_stats <- analysis_data %>%
  group_by(main_info_type) %>%
  summarise(
    n_days = n(),
    mean_ret = mean(actual_return, na.rm = TRUE),
    median_ret = median(actual_return, na.rm = TRUE),
    win_rate = mean(is_positive_day, na.rm = TRUE) 
  ) %>%
  arrange(desc(n_days))

print(directional_stats)

# ==============================================================================
# 5 - Descriptive Analysis 2: Reaction Magnitude by Direction
# ==============================================================================
cat("\n=== Analysis 2: Reaction Magnitude on Up vs. Down Days ===\n")

magnitude_stats <- analysis_data %>%
  group_by(main_info_type, return_direction) %>%
  summarise(
    n_days = n(),
    avg_impact = mean(abs_return, na.rm = TRUE), 
    max_impact = max(abs_return, na.rm = TRUE),  
    .groups = "drop"
  )

print(magnitude_stats)

# ==============================================================================
# 6 - Visualization: Violin Plot of Return Distribution
# ==============================================================================
plot_magnitude <- ggplot(analysis_data, aes(x = main_info_type, y = actual_return, fill = main_info_type)) +
  geom_violin(alpha = 0.4, color = "darkgray") +
  # Jitter shows the actual data points
  geom_jitter(aes(color = return_direction), width = 0.2, alpha = 0.7, size = 2) +
  theme_minimal() +
  scale_fill_manual(values = c("Hard" = "#e74c3c", "Soft" = "#3498db", "Mixed" = "#95a5a6")) +
  scale_color_manual(values = c("Up" = "#2ecc71", "Down" = "#c0392b", "Flat" = "#7f8c8d")) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title = "Return Distribution: Direction & Magnitude by News Type",
    subtitle = "Violin plot shows density, points represent individual trading days",
    x = "Dominant News Type", 
    y = "Daily Return (Actual Return)", 
    color = "Daily Direction", 
    fill = "News Category"
  )

# Display plot in VS Code
print(plot_magnitude)

# Save graphic in high resolution
ggsave(
  filename = file.path(output_dir, "asml_directional_distribution.png"), 
  plot = plot_magnitude, 
  device = "png",
  width = 8, 
  height = 6, 
  units = "in",
  dpi = 300
)
print("Graphic 'asml_directional_distribution.png' saved in output folder.")

# ==============================================================================
# 7 - Export the comprehensive dataset
# ==============================================================================
write_csv(analysis_data, file.path(output_dir, "asml_comprehensive_analysis.csv"))
print("Comprehensive analysis dataset exported successfully.")