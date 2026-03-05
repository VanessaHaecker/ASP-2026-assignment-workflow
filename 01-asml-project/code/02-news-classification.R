#!/usr/bin/env Rscript

# Project: ASML News  Analysis (Hard vs. Soft News)
# Path: 01-asml-project/code/02-news-classification.R

if (!require("pacman")) install.packages("pacman"); library("pacman")
p_load(ellmer)
p_load(jsonlite)
p_load(tidyverse)
p_load(here)

# 1 - Projekt-Setup (Pfade fixieren)
here::i_am("01-asml-project/code/02-news-classification.R")
root <- here::here()

input_dir  <- file.path(root, "01-asml-project", "input")
output_dir <- file.path(root, "01-asml-project", "output")

# 2 - Daten laden
news_data <- read_csv(file.path(input_dir, "asml_news_headlines.csv"))

# 3 - LLM Setup (Ollama statt OpenRouter)
# Definiere das Schema direkt im System Prompt wie in deinem Kurs-Beispiel
system_prompt = "You are a financial economist. You only respond to the exact question I ask and your response is valid JSON. 
The JSON response should have the following schema: { 'headline': [string], 'category': ['Hard', 'Soft'], 'reasoning': [string] }.
'Hard' news are quantitative, verifiable facts (earnings, orders). 'Soft' news are qualitative (opinions, rumors, sentiment)."

chat_asml = chat_ollama(
  model = "llama3.2", # Stelle sicher, dass dieses Modell geladen ist: ollama pull llama3.2
  system_prompt = system_prompt
)

# 4 - Klassifizierung durchführen
# Wir erstellen eine Liste, um die Ergebnisse zu speichern
results_list = list()

# Wir gehen die Schlagzeilen durch (zum Testen erst einmal nur die ersten 10)
# Entferne [1:10], um alle zu klassifizieren
for (i in 1:nrow(news_data[1:10, ])) {
  
  headline_to_check = news_data$headline[i]
  
  message(paste("Verarbeite Zeile:", i))
  
  # Chat-Abfrage
  response = chat_asml$chat(paste0("Classify the following ASML news headline: ", headline_to_check))
  
  # Parse JSON (wie im Kurs-Beispiel)
  response_json = fromJSON(response)
  
  # Füge das Datum aus den Originaldaten hinzu
  response_json$date = news_data$date[i]
  
  # Speichere in der Liste
  results_list[[i]] = as.data.frame(response_json)
}

# 5 - Ergebnisse zusammenführen und speichern
news_classified <- bind_rows(results_list)

# Speichere die Ergebnisse im 'input' Ordner für das finale Skript
write_csv(news_classified, file.path(input_dir, "asml_news_classified.csv"))

print("Klassifizierung abgeschlossen!")
print(head(news_classified))