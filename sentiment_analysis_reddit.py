import ollama
import pandas as pd
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score

finalsample = pd.read_csv("reddit_during.csv")

# LLM-Aufruf
def analyst(text, system_prompt):
    response = ollama.chat(
        model="llama3.1:8b-instruct-q8_0",
        messages=[
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": text}
        ],
        options={
            "temperature": 0.2,
            "seed": 15
        }
    )
    return response['message']['content'].strip()

# Daten einlesen und bereinigen
finalsample = pd.read_csv("final_sent.csv")
finalsample = finalsample[finalsample['Comment Body'] != '[deleted]']
finalsample.reset_index(drop=True, inplace=True)

# Prompt zur 3-Wege-Klassifikation
system_prompt = (
    "You are a sentiment analysis assistant.\n\n"
    "Your task: Given a post title and a comment, classify the comment into exactly one of three categories:\n"
    "- positive\n"
    "- negative\n"
    "- neutral\n\n"
    "Instructions:\n"
    "- First, output only the classification label: positive, negative, or neutral.\n"
    "- Then, after three vertical bars (`|||`), provide a short sentence explaining your decision.\n"
    "- Do not include any other text.\n\n"
    "Guidelines:\n"
    "- Use the post title only for context. Base the classification solely on the comment.\n"
    "- Classify as **positive** if the comment expresses clear praise, enthusiasm, admiration, hope, or encouragement.\n"
    "- Classify as **negative** if the comment expresses criticism, frustration, sarcasm, disappointment, or complaints.\n"
    "- Classify as **neutral** if the comment is primarily descriptive, balanced, mixed, analytical, or emotionally flat.\n"
    "- Do not mix categories. Choose the dominant emotional tone.\n\n"
    "Output format:\n"
    "[positive|negative|neutral] ||| [Short reason]\n\n"
    "Examples:\n"
    "Input: He absolutely nailed it. Output: positive ||| The comment expresses clear admiration.\n"
    "Input: The organizers messed up everything again. Output: negative ||| It criticizes the event organization.\n"
    "Input: It was on yesterday. Output: neutral ||| The comment is descriptive and contains no emotional tone.\n"
)

# Ergebnis-Spalten
finalsample["llm_sentiment"] = None
finalsample["sentiment_reasoning"] = None

# LLM-Auswertung
for x in range(len(finalsample)):
    text = "Post Title: " + finalsample.at[x, "Post Title"] + "\nComment: " + finalsample.at[x, "Comment Body"]
    output = analyst(text, system_prompt)
    if '|||' in output:
        sentiment, reason = output.split('|||')
        finalsample.at[x, "llm_sentiment"] = sentiment.strip().lower()
        finalsample.at[x, "sentiment_reasoning"] = reason.strip()
    else:
        finalsample.at[x, "llm_sentiment"] = "error"
        finalsample.at[x, "sentiment_reasoning"] = "error"


finalsample["mismatch"] = (
    finalsample["final_sent"].str.strip().str.lower() != finalsample["llm_sentiment"].str.strip().str.lower()
).astype(int)

# Ergebnisse speichern
finalsample.to_csv("llm_eval_2.csv", index=False)

finalsample = pd.read_csv("llm_eval_2.csv")

# Verteilung ausgeben
print(finalsample["llm_sentiment"].value_counts())
print(finalsample['final_sent'].value_counts())

# Strip + lowercase zur Normalisierung
finalsample["final_sent"] = finalsample["final_sent"].str.strip().str.lower()
finalsample["llm_sentiment"] = finalsample["llm_sentiment"].str.strip().str.lower()

# Evaluation für alle drei Klassen
classes = ["positive", "neutral", "negative"]

for target_class in classes:
    # Binäre Klassifikation: 1 = Zielklasse, 0 = Rest
    y_true = (finalsample["final_sent"] == target_class).astype(int)
    y_pred = (finalsample["llm_sentiment"] == target_class).astype(int)

    # Metriken berechnen
    accuracy = accuracy_score(y_true, y_pred)
    precision = precision_score(y_true, y_pred, zero_division=0)
    recall = recall_score(y_true, y_pred, zero_division=0)
    f1 = f1_score(y_true, y_pred, zero_division=0)

    # Ausgabe
    print(f"Evaluation for class: '{target_class}'")
    print(f"Accuracy:  {accuracy:.4f}")
    print(f"Precision: {precision:.4f}")
    print(f"Recall:    {recall:.4f}")
    print(f"F1 Score:  {f1:.4f}")

