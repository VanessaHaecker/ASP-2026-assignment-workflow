# ASML News Analysis: Hard vs. Soft Information and Market Volatility

## Project Overview
This project analyzes the impact of financial news on the stock market performance of ASML Holding N.V. (Ticker: ASML). Specifically, it investigates whether "Hard News" (verifiable, quantitative facts like earnings reports) drives different market volatility and directional returns compared to "Soft News" (analyst ratings, market sentiment, rumors).

## Repository Structure
* **`code/`**: Contains all R scripts used for data collection, LLM classification, and statistical analysis.
* **`input/`**: Stores the raw API data, stock prices, and the fully processed/classified datasets.
* **`output/`**: Contains all generated visualizations (plots) and statistical summary tables.
* **`Makefile`**: The automated build instruction for reproducing the analysis.


The pipeline is intentionally decoupled. Scripts 01 and 02 are highly resource-intensive:
1. **Script 01** requires a live Finnhub API Key.
2. **Script 02** utilizes a locally hosted Large Language Model (Ollama/LLaMA3) to classify the headlines. This machine-learning process took approximately **5 hours** to complete.

To ensure a seamless and reproducible grading experience without risking API timeouts or extreme compute latency, all processed files have been securely saved in the `input/` directory. 

## Methodology
1. **Data Collection**: Sourced 30 days of ASML stock price data (Yahoo Finance) and company news headlines (Finnhub API).
2. **Classification (Chain-of-Thought)**: Employed a strictly prompted LLaMA3 model to categorize each headline as either 'Hard' or 'Soft' news based on objective financial definitions. For reproducibility, classification used a local Llama 3.2 instance with temperature fixed at 0.0 via a custom Ollama Modelfile.
3. **Volatility & Directional Analysis**: Merged the daily dominant news category with ASML's daily stock returns to measure absolute volatility and directional magnitude.

## Key Findings
* **Volume**: The news cycle was heavily dominated by Soft News (14 days) compared to Hard News (2 days).
* **Volatility**: Interestingly, Soft News days exhibited a higher average volatility (2.15%) than Hard News days (1.26%), suggesting that speculation and analyst sentiment drove significant market reactions during this specific timeframe.