library(readr)
library(rollama)


posts <- c("What do you think about the olympics?", "Whats your opinion on paris 2024?", "I loved visiting the olympics!", "I hated the olympics")
comments <- c("They sucked!", "I loved being there.", "I watched Table Tennis.", "True")

df <- data.frame(post_title = posts, comment_body = comments)

comments <- as.character(comments)
posts <- as.character(posts)

prompt1 <- rollama::make_query(
  text = paste0("Post Title: ", posts, "\nComment: ", comments),
  prompt = "\ncategories: positive, negative, neutral",
  template = "{prefix}{text}\n{prompt}",
  prefix = "Classify the sentiment of the following comment in relation to the post title:",
  system = paste0(
    "Analyze the sentiment of a comment based on the given post title.",
    " Your response must be one of the following categories: 'positive', 'negative', or 'neutral'.\n\n",
    "Definitions:\n",
    "- Positive: The comment expresses a supportive or enthusiastic opinion about the Olympics.\n",
    "- Negative: The comment expresses a critical or unfavorable opinion about the Olympics.\n",
    "- Neutral: The comment does not express a clear opinion about the Olympics.\n\n",
    "Instructions:\n",
    "1. Take the post title into account when deciding the sentiment.\n",
    "2. Respond with only one word: 'positive', 'negative', or 'neutral'. Do not provide any additional explanation.",
    "Focus on the sentiment of the comment and not the post title.",
    "If a post title is expresses an opinion but the comment doesn't it's still neutral"
  )
)

annotated <- query(
  prompt1, 
  model = "llama3.2",  # Specify model explicitly
  screen = TRUE, 
  model_params = list(temperature = 0.2, seed = 15)
)


sentiments <- sapply(annotated, function(x) x$message$content)

df$sentiment <- sentiments

