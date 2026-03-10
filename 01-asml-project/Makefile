# --- IMPORTANT NOTE ---
# Please ONLY run the analysis scripts (Script 03 and 04) using 'make all'.
# Data collection (Script 01) and LLM classification (Script 02 & 02b) are highly 
# compute- and time-intensive. Script 01 requires a live Finnhub API key, and 
# Script 02 uses a local LLM (Ollama/LLaMA3) which took approximately 5 hours to run.
# REPRODUCIBILITY NOTICE:
# The Finnhub live API dynamically updates its historical news feed (e.g., removing 
# duplicates, altering timestamps), and LLM outputs have inherent stochasticity. 
# Therefore, running Scripts 01 and 02 again will result in slight data discrepancies. 
# To guarantee strict scientific reproducibility of the final analysis and plots, 
# a frozen snapshot of the original, classified dataset is safely stored in the 
# 'input/' directory. Please rely on this static dataset for grading.

# --- MASTER LIST OF GOALS ---
# This only triggers the fast, reproducible analysis scripts (03 & 04).
all: output/asml_volatility_comparison.png \
     output/asml_volatility_timeseries.png \
     output/asml_volatility_bars.png \
     output/asml_directional_distribution.png \
     output/asml_comprehensive_analysis.csv

# --- SCRIPT 03: VOLATILITY ANALYSIS ---
# Merges price data with classified news to analyze market volatility.
output/asml_volatility_comparison.png output/asml_volatility_timeseries.png output/asml_volatility_bars.png: code/03-return-analysis.R input/asml_news_classified_FINAL.csv input/asml_prices.csv
	Rscript code/03-return-analysis.R

# --- SCRIPT 04: DIRECTIONAL ANALYSIS ---
# Conducts final analysis on return magnitude and direction based on news type.
output/asml_directional_distribution.png output/asml_comprehensive_analysis.csv: code/04-directional-analysis.R input/asml_news_classified_FINAL.csv input/asml_prices.csv
	Rscript code/04-directional-analysis.R

# --- DANGER ZONE: DATA COLLECTION & LLM CLASSIFICATION ---
# These targets are decoupled from 'make all' to prevent accidental execution 
# triggered by file timestamp changes on different operating systems.
rebuild_data:
	@echo "WARNING: Starting the 5-hour API and LLM process."
	# Rscript code/01-data-collection.R
	# Rscript code/02-news-classification.R
	# Rscript code/02b-error-recovery.R

