library(readr)
library(rollama)
library(openxlsx)
library(stringr)


df1 <- read_csv("C:/Users/alexa/PycharmProjects/reddit scraping/reddit_before_after.csv")

random_sample <- df1[sample(nrow(df1), 200), ]

prompt1 <- rollama::make_query(
  text = paste0(
    "Post Title: ", random_sample$`Post Title`, "\n",
    "Comment: ", random_sample$`Comment Body`
  ),
  template = "{prefix}{text}",
  prefix = "Translate Post Title and Comment from French to English:\n",
  system = paste(
    "You are a translation assistant. Translate the given Post Title and Comment from French to English.",
    "If the Post Title or Comment is already in English, DO NOT change them. Just copy them as they are.",
    "Regardless of whether translation is needed or not, ALWAYS produce output in the format shown below.",
    "NEVER skip or omit either field, even if it is already English.",
    "Use this exact format with markers:",
    "",
    "### COMMENT_TRANSLATION:",
    "{translated_or_unchanged_comment}",
    "### POST_TITLE_TRANSLATION:",
    "{translated_or_unchanged_title}",
    "",
    "Do NOT include any extra text, explanations, or headers. Output ONLY the formatted result.",
    sep = "\n"
  )
)

# Send to model
annotated <- query(
  prompt1,
  model = "llama3.2",
  screen = TRUE,
  model_params = list(temperature = 0.2, seed = 15)
)


translation <- sapply(annotated, function(x) x$message$content)



# Prepare an empty data frame
df2 <- data.frame(Comment = character(0), Title = character(0), stringsAsFactors = FALSE)

for (x in 1:length(translation)) {
  input_text <- translation[x]
  
  # Regex: capture comment and title flexibly
  matches <- str_match(
    input_text,
    "### COMMENT_TRANSLATION:\\s*(.*?)\\s*### POST_TITLE_TRANSLATION:\\s*(.*)"
  )
  
  comment <- ifelse(is.na(matches[,2]), "", str_trim(matches[,2]))
  title <- ifelse(is.na(matches[,3]), "", str_trim(matches[,3]))
  
  df2 <- rbind(df2, data.frame(Comment = comment, Title = title, stringsAsFactors = FALSE))
}


write.csv(translation, "C:\\Users\\alexa\\Documents\\sportswashing\\data.csy", row.names=FALSE)


