#!/usr/bin/env Rscript

# Project: ASML News Analysis (Hard vs. Soft News)
# Script 02b: Error Recovery (Single-Shot) - without modifying the original file, but creating a new one with the repaired data
# reason for this script: the LLM is very strict with the output format, and any deviation (e.g., a single line of non-JSON text) breaks the entire classification process. This script identifies the error lines, sends them one by one to the LLM for reclassification, and creates a new file with the repaired categories.

# 1 - Load necessary libraries
if (!require("pacman")) install.packages("pacman"); library("pacman")

p_load(
  tidyverse, # Data manipulation & stringr (for text cleaning)
  ellmer,    # Interface for LLMs
  jsonlite,  # Parsing JSON responses
  here       # Path management
)

options(timeout = 1200)

here::i_am("01-asml-project/code/02b-error-recovery.R")
root <- here::here()
input_dir <- file.path(root, "01-asml-project", "input")

# 1 - Load the partially classified data with errors
data_file <- file.path(input_dir, "asml_news_partial.csv")
df <- read_csv(data_file)

# 2 - Identify rows where the category is "Error" (indicating a parsing failure)
error_indices <- which(df$category == "Error")
message(paste("Gefundene Fehlerzeilen:", length(error_indices)))

if (length(error_indices) > 0) {
  
  # 3 - LLM Setup (Single-Shot Prompting for each headline)
  system_prompt_single <- "You are a highly precise, strictly JSON-only financial text classification API. 
You output ONLY a valid JSON object. No markdown, no conversational text, no preambles.

TASK: Classify the provided financial news headlines about ASML into 'Hard' or 'Soft' information.

DEFINITIONS & EDGE CASES:
- HARD: Quantitative, objective, and verifiable company facts. 
  Examples: Official company earnings reports, concrete order numbers, dividend announcements, exact revenue figures.
- SOFT: Qualitative, interpretive, subjective, or forward-looking information.
  Examples: Analyst ratings/reports (e.g., 'Analyst upgrades ASML'), market sentiment, macroeconomic speculation, rumors, and industry forecasts.
  CRITICAL RULE ON 'REPORTS': An official ASML financial/earnings report is HARD. An analyst, media, or industry report *about* ASML is SOFT.

OUTPUT FORMAT:
Return EXACTLY this JSON structure for the headline provided:
{
  \"headline\": \"[Insert exact headline]\",
  \"category\": \"Hard\" or \"Soft\",
  \"reasoning\": \"[Strictly one short sentence explaining why based on the definitions]\"
}

FAILURE TO RETURN PURE JSON WILL BREAK THE SYSTEM."

  chat_asml <- chat_ollama(
    model = "llama3-fixed", 
    system_prompt = system_prompt_single
  )
  
  # 4 - Loop through each error line, send the headline to the LLM, and attempt to repair the category
  for (i in error_indices) {
    headline_to_check <- df$headline[i]
    message(paste("Repariere Zeile", i, ":", str_trunc(headline_to_check, 40)))
    
    response <- chat_asml$chat(paste("Classify this headline:", shQuote(headline_to_check)))
    
    tryCatch({
      json_match <- str_extract(response, "\\{.*\\}")
      
      if (!is.na(json_match)) {
        res_parsed <- fromJSON(json_match)
        
        if (!is.null(res_parsed$category)) {
          df$category[i] <- res_parsed$category
          df$reasoning[i] <- res_parsed$reasoning
        }
      }
    }, error = function(e) {
      message(paste("!!! Zeile", i, "konnte nicht behoben werden:", e$message))
    })
  }
  
  # 5 - Save the repaired dataset to a new file (both the full version with reasoning and a clean version for analysis)
  
  # A) New file 1: Full version with reasoning
  write_csv(df, file.path(input_dir, "asml_news_FULL_recovered.csv"))
  
  # B) New file 2: Clean version for analysis (only date, headline, and repaired category)
  final_analysis_data <- df %>% select(date, headline, category)
  write_csv(final_analysis_data, file.path(input_dir, "asml_news_classified_FINAL.csv"))
  
  message("Reparatur abgeschlossen! Neue Dateien: 'asml_news_FULL_recovered.csv' und 'asml_news_classified_FINAL.csv' wurden erstellt.")
  
} else {
  message("Keine Errors gefunden! Die Originaldatei war bereits perfekt.")
}
