These are replication scripts for my study on sportswashing. The scripts do the following:

1) nexis_lesen.R

Reads Nexis Uni exports (RTF), converts them to text, splits them into documents/paragraphs, parses/cleans dates (incl. multilingual month/day strings), filters paragraphs that mention Paris/France, joins in outlet metadata (country/continent/type), produces a few descriptive plots, and exports:

df_joined.csv (full cleaned dataset)
df_joined_sample.csv (random sample for downstream sentiment coding)

2) sentiment_analysis_nexis.ipynb

Runs LLM-based sentiment classification (via a local Ollama model) on the Nexis sample produced above and writes:

df_joined_sample_sentiment.csv

3) sentiment_analysis_reddit.py

Runs LLM-based sentiment classification for Reddit data (skipping deleted content), stores the predicted labels, flags mismatches vs. a human label column, and prints evaluation metrics (accuracy/precision/recall/F1 + confusion matrix). Exports:

llm_eval_2.csv

4) reddit_graphs.R

Loads the Reddit sentiment output, aggregates by date, and generates descriptive figures (e.g., net sentiment over time). Saves plots as:

reddit1.pdf, reddit2.pdf, reddit3.pdf

5) regression_analysis.R

Loads the (news) sentiment data and medal data, merges/aggregates them, and estimates panel regressions (incl. fixed effects) relating sentiment to medals and other covariates. Produces:

regression table regression_table_main.tex

figures such as sent_meds.pdf, sent_meds_counts.pdf

6) translation.R

Uses an LLM (via rollama) to translate French Reddit items into English (title/comment), parses the model output into structured fields, and writes a translation output file.
