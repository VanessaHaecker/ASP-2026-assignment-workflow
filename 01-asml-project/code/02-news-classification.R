#!/usr/bin/env Rscript

# PROJECT: ASML News Analysis (Hard vs. Soft News)
# Script 02: ASML News Classification (Chain-of-Thought Prompting)

rm(list = ls()) 

# 1 - Load Environment & Libraries
# Set the timeout limit to 20 minutes to prevent connection interruptions
options(timeout = 1200)

if (!require("pacman")) install.packages("pacman"); library("pacman")
p_load(
  ellmer,    # Interface for LLMs
  jsonlite,  # Parsing JSON responses
  tidyverse, # Data manipulation
  here       # Path management
)

here::i_am("01-asml-project/code/02-news-classification.R")
root <- here::here()

# 2 - Define relative paths for the project structure
input_dir  <- file.path(root, "01-asml-project", "input")
output_dir <- file.path(root, "01-asml-project", "output")

# 3 - Data Loading
news_file <- file.path(input_dir, "asml_news_headlines.csv")

if (!file.exists(news_file)) {
  stop("Input data missing! Please run '01-data-collection.R' first.")
}

news_data <- read_csv(news_file)

# 4 - LLM Configuration & Prompt Engineering
system_prompt <- "You are a highly precise, strictly JSON-only financial text classification API. 
You output ONLY a valid JSON array of objects. No markdown, no conversational text, no preambles.

TASK: Classify the provided financial news headlines about ASML into 'Hard' or 'Soft' information.

DEFINITIONS & EDGE CASES:
- HARD: Quantitative, objective, and verifiable company facts. 
  Examples: Official company earnings reports, concrete order numbers, dividend announcements, exact revenue figures.
- SOFT: Qualitative, interpretive, subjective, or forward-looking information.
  Examples: Analyst ratings/reports (e.g., 'Analyst upgrades ASML'), market sentiment, macroeconomic speculation, rumors, and industry forecasts.
  CRITICAL RULE ON 'REPORTS': An official ASML financial/earnings report is HARD. An analyst, media, or industry report *about* ASML is SOFT.

OUTPUT FORMAT:
Return EXACTLY this JSON structure for EVERY headline provided, wrapped in ONE array:
[
  {
    \"headline\": \"[Insert exact headline]\",
    \"category\": \"Hard\" or \"Soft\",
    \"reasoning\": \"[Strictly one short sentence explaining why based on the definitions]\"
  }
]

FAILURE TO RETURN PURE JSON WILL BREAK THE SYSTEM."

chat_asml <- chat_ollama(
  model = "llama3-fixed", 
  system_prompt = system_prompt
)

# 5 - Batch Classification Loop with Robust Parsing & Sanitization

set.seed(42) 
batch_size <- 5 
batches <- split(news_data, (seq_len(nrow(news_data)) - 1) %/% batch_size)

results_list <- list()

message(paste("Starte bereinigte Klassifizierung für", nrow(news_data), "Schlagzeilen in", length(batches), "Paketen..."))

for (i in seq_along(batches)) {
  current_batch <- batches[[i]]
  
  # Sanitize headlines to prevent JSON parsing issues (e.g., unescaped quotes, newlines)
  safe_headlines <- stringr::str_replace_all(current_batch$headline, '\"', "'")
  safe_headlines <- stringr::str_replace_all(safe_headlines, "[\r\n]", " ")
  
  batch_prompt <- paste(
    "Classify the following headlines as a JSON array of objects. Return ONLY the array:",
    paste(shQuote(safe_headlines), collapse = ", ")
  )
  
  message(paste("Verarbeite Paket", i, "von", length(batches)))
  
  response <- chat_asml$chat(batch_prompt)
  
  batch_result <- tryCatch({
    json_match <- stringr::str_extract(response, stringr::regex("\\[.*\\]", dotall = TRUE))
    
    if(is.na(json_match)) stop("Kein Array gefunden.")
    
    res_df <- jsonlite::fromJSON(json_match, simplifyVector = TRUE) %>% as.data.frame()
    
    if(!all(c("headline", "category", "reasoning") %in% colnames(res_df))) {
      stop("Spalten fehlen.")
    }
    
    res_df <- res_df %>% select(headline, category, reasoning)
    n_returned <- min(nrow(res_df), nrow(current_batch))
    
    res_df <- res_df[1:n_returned, ] 
    res_df$date <- current_batch$date[1:n_returned]
    
    res_df
    
  }, error = function(e) {
    message(paste("!!! Fehler in Paket", i, "-", e$message))
    data.frame(
      headline = current_batch$headline,
      category = "Error",
      reasoning = "Parsing failed",
      date = current_batch$date,
      stringsAsFactors = FALSE
    )
  })
  
  results_list[[i]] <- batch_result
  write_csv(bind_rows(results_list), file.path(input_dir, "asml_news_partial.csv"))
}


# 6 - Combine Results, Save Output, and Generate Report Appendix

news_classified <- bind_rows(results_list)

full_output <- news_classified %>% select(date, headline, category)
write_csv(full_output, file.path(input_dir, "asml_news_classified.csv"))

report_appendix <- news_classified %>%
  filter(category != "Error") %>%
  slice_sample(n = min(30, sum(news_classified$category != "Error"))) %>% 
  select(category, headline, reasoning)

write_csv(report_appendix, file.path(output_dir, "llm_reasoning_report.csv"))

message("Klassifizierung abgeschlossen!")

