#!/usr/bin/env Rscript
# ==============================================================================
# Script 03: ASML Return Analysis (EMH Test)
# Path: 01-asml-project/code/03-return-analysis.R
# ==============================================================================

library(tidyverse)
library(here)

# Pfad-Setup für Reproduzierbarkeit
here::i_am("01-asml-project/code/03-return-analysis.R")

# ==============================================================================
# 1 - Daten laden
# ==============================================================================
prices <- read_csv(here("01-asml-project/input/asml_prices.csv"))

# WICHTIG: Wir laden die fehlerfrei reparierte FINAL-Datei
news   <- read_csv(here("01-asml-project/input/asml_news_classified_FINAL.csv"))

# ==============================================================================
# 2 - Daten auf Tagesebene aggregieren
# ==============================================================================
# Wir bestimmen für jeden Tag, ob Hard oder Soft News dominiert haben
daily_news_summary <- news %>%
  filter(category != "Error") %>% # Nur zur Sicherheit
  group_by(date) %>%
  summarise(
    hard_count = sum(category == "Hard"),
    soft_count = sum(category == "Soft"),
    # Zuordnung: Der Typ mit den meisten Meldungen gewinnt (Bei Gleichstand: Hard)
    main_info_type = ifelse(hard_count >= soft_count, "Hard", "Soft")
  )

# ==============================================================================
# 3 - Merge mit den Aktiendaten
# ==============================================================================
analysis_data <- prices %>%
  inner_join(daily_news_summary, by = "date") %>%
  mutate(abs_return = abs(daily_return)) # Absolute Rendite als Maß für Volatilität

# ==============================================================================
# 4 - Statistische Auswertung (Deskriptiv)
# ==============================================================================
stats_summary <- analysis_data %>%
  group_by(main_info_type) %>%
  summarise(
    n_days = n(),
    avg_volatility = mean(abs_return, na.rm = TRUE),
    sd_volatility = sd(abs_return, na.rm = TRUE)
  )

print("=== Deskriptive Statistik ===")
print(stats_summary)

# ==============================================================================
# 5 - Visualisierung: Boxplot der Volatilität
# ==============================================================================
plot <- ggplot(analysis_data, aes(x = main_info_type, y = abs_return, fill = main_info_type)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 8) +
  theme_minimal() +
  scale_fill_manual(values = c("Hard" = "#2c3e50", "Soft" = "#3498db")) +
  labs(
    title = "Marktreaktion: Hard vs. Soft News (ASML)",
    subtitle = "Absolute tägliche Renditen als Maß für Informationsverarbeitung",
    x = "Nachrichtentyp",
    y = "Absolute Rendite (Volatilität)",
    fill = "Kategorie"
  )

# Grafik speichern für das Dokument
ggsave(here("01-asml-project/output/asml_volatility_comparison.png"), plot = plot, width = 8, height = 6)
print("Grafik 'asml_volatility_comparison.png' im Output-Ordner gespeichert.")

# ==============================================================================
# 6 - Statistische Signifikanztests (Inferenzstatistik)
# ==============================================================================

cat("\n=== Welch's T-Test (Vergleich der Mittelwerte) ===\n")
t_test_result <- t.test(abs_return ~ main_info_type, data = analysis_data)
print(t_test_result)

cat("\n=== Wilcoxon Rank-Sum Test (Robust gegen Ausreißer) ===\n")
wilcox_result <- wilcox.test(abs_return ~ main_info_type, data = analysis_data)
print(wilcox_result)

cat("\n=== Regressionsmodell (OLS) ===\n")
# Umwandlung in Dummy: 1 wenn Hard, 0 wenn Soft
analysis_data <- analysis_data %>%
  mutate(hard_dummy = ifelse(main_info_type == "Hard", 1, 0))

regression_model <- lm(abs_return ~ hard_dummy, data = analysis_data)
summary(regression_model) %>% print()




# ==============================================================================
## ==============================================================================
# 5 - Visualisierung: Zeitreihe der Volatilität (Absolute Renditen)
# ==============================================================================

library(scales)

plot_ts <- ggplot(analysis_data, aes(x = date)) +
  # 1. Die farbigen Hintergründe für jeden Tag (Hard vs. Soft)
  geom_rect(aes(xmin = date - 0.5, xmax = date + 0.5, 
                ymin = 0, ymax = Inf, fill = main_info_type), 
            alpha = 0.3) +
  
  # 2. Die eigentliche Zeitreihe der ABSOLUTEN Renditen (Volatilität)
  geom_line(aes(y = abs_return), color = "#2c3e50", linewidth = 1) +
  geom_point(aes(y = abs_return), color = "#2c3e50", size = 2.5) +
  
  # 3. Optische Anpassungen
  scale_fill_manual(values = c("Hard" = "#e74c3c", "Soft" = "#3498db")) +
  # y-Achse bei 0 beginnen lassen, da absolute Zahlen nicht negativ sein können
  scale_y_continuous(labels = percent_format(accuracy = 0.1), limits = c(0, NA)) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  theme_minimal() +
  labs(
    title = "ASML Volatilität: Hard vs. Soft News",
    subtitle = "Absolute tägliche Renditen zeigen die Intensität der Informationsverarbeitung",
    x = "Datum",
    y = "Volatilität (Absolute Rendite)",
    fill = "Dominante News"
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

# Grafik speichern
ggsave(here("01-asml-project/output/asml_volatility_timeseries.png"), plot = plot_ts, width = 10, height = 5)
print("Grafik 'asml_volatility_timeseries.png' im Output-Ordner gespeichert.")

# mit blakendiagramm 

# ==============================================================================
# 5 - Visualisierung: Balkendiagramm der Volatilität (Absolute Renditen)
# ==============================================================================

library(scales)

plot_bar <- ggplot(analysis_data, aes(x = date, y = abs_return, fill = main_info_type)) +
  # 1. Balkendiagramm für isolierte Tages-Schocks
  geom_col(width = 0.6, alpha = 0.85) +
  
  # 2. Eine Basislinie (Nulllinie) zur optischen Erdung
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  
  # 3. Optische Anpassungen für ein sauberes, akademisches Layout
  scale_fill_manual(values = c("Hard" = "#e74c3c", "Soft" = "#3498db")) +
  # expand = c(0,0) sorgt dafür, dass die Balken direkt auf der Linie stehen
  scale_y_continuous(labels = percent_format(accuracy = 0.1), expand = expansion(mult = c(0, 0.1))) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  theme_minimal() +
  labs(
    title = "ASML Volatilitäts-Schocks: Hard vs. Soft News",
    subtitle = "Tägliche absolute Renditen visualisiert als isolierte Marktreaktionen",
    x = "Datum",
    y = "Volatilität (Absolute Rendite)",
    fill = "Dominante News"
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), # Entfernt vertikale Gitternetzlinien für mehr Klarheit
    plot.title = element_text(face = "bold")
  )

# Grafik speichern
ggsave(here("01-asml-project/output/asml_volatility_bars.png"), plot = plot_bar, width = 10, height = 5)
print("Grafik 'asml_volatility_bars.png' im Output-Ordner gespeichert.")
