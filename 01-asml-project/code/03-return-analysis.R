# Script 03: ASML Return Analysis (EMH Test)
# Path: 01-asml-project/code/03-return-analysis.R

library(tidyverse)
library(here)

# 1 - Daten laden
prices <- read_csv(here("01-asml-project/input/asml_prices.csv"))
news   <- read_csv(here("01-asml-project/input/asml_news_classified.csv"))

# 2 - Daten zusammenführen (Merge)
# Wir gruppieren die News pro Tag, um zu sehen, welche Info-Art dominiert
daily_info <- news %>%
  group_by(date) %>%
  summarise(info_type = ifelse(sum(category == "Hard") > sum(category == "Soft"), "Hard", "Soft"))

final_data <- prices %>%
  left_join(daily_info, by = "date") %>%
  filter(!is.na(info_type)) # Nur Tage mit News behalten

# 3 - Analyse: Reagiert der Markt bei 'Hard News' heftiger?
analysis <- final_data %>%
  group_by(info_type) %>%
  summarise(
    avg_abs_return = mean(abs(daily_return), na.rm = TRUE),
    count = n()
  )

print(analysis)


#neuer code 
library(tidyverse)
library(here)

# 1 - Daten laden
prices <- read_csv(here("01-asml-project/input/asml_prices.csv"))
news   <- read_csv(here("01-asml-project/input/asml_news_classified.csv"))

# 2 - Daten auf Tagesebene aggregieren
# Wir bestimmen für jeden Tag, ob Hard oder Soft News dominiert haben
daily_news_summary <- news %>%
  filter(category != "Error") %>%
  group_by(date) %>%
  summarise(
    hard_count = sum(category == "Hard"),
    soft_count = sum(category == "Soft"),
    # Zuordnung: Der Typ mit den meisten Meldungen gewinnt
    main_info_type = ifelse(hard_count >= soft_count, "Hard", "Soft")
  )

# 3 - Merge mit den Aktiendaten
analysis_data <- prices %>%
  inner_join(daily_news_summary, by = "date") %>%
  mutate(abs_return = abs(daily_return)) # Absolute Rendite als Maß für Volatilität

# 4 - Statistische Auswertung für den Bericht
stats_summary <- analysis_data %>%
  group_by(main_info_type) %>%
  summarise(
    n_days = n(),
    avg_volatility = mean(abs_return, na.rm = TRUE),
    sd_volatility = sd(abs_return, na.rm = TRUE)
  )

print(stats_summary)

# 5 - Visualisierung: Boxplot der Volatilität
ggplot(analysis_data, aes(x = main_info_type, y = abs_return, fill = main_info_type)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Marktreaktion: Hard vs. Soft News (ASML)",
    subtitle = "Absolute tägliche Renditen als Maß für Informationsverarbeitung",
    x = "Nachrichtentyp",
    y = "Absolute Rendite (Volatilität)",
    fill = "Kategorie"
  )

# Grafik speichern für das Dokument
ggsave(here("01-asml-project/output/asml_volatility_comparison.png"), width = 8, height = 6)

# ... Dein bisheriger Code bis Schritt 5 ...

# ==============================================================================
# 6 - Statistische Signifikanztests
# ==============================================================================

# A) Welch's T-Test (Vergleich der Mittelwerte)
t_test_result <- t.test(abs_return ~ main_info_type, data = analysis_data)
print("--- Welch's T-Test ---")
print(t_test_result)

# B) Wilcoxon-Rangsummentest (Robust gegen Ausreißer / Schiefe Verteilung)
wilcox_result <- wilcox.test(abs_return ~ main_info_type, data = analysis_data)
print("--- Wilcoxon Rank-Sum Test ---")
print(wilcox_result)

# C) Lineare Regression (Optional, sieht gut im Report aus)
# Umwandlung in Dummy: 1 wenn Hard, 0 wenn Soft
analysis_data <- analysis_data %>%
  mutate(hard_dummy = ifelse(main_info_type == "Hard", 1, 0))

regression_model <- lm(abs_return ~ hard_dummy, data = analysis_data)
print("--- Regressionsmodell (OLS) ---")
summary(regression_model)