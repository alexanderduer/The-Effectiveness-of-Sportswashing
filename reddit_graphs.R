library(ggplot2)
library(dplyr)
library(scales)

df <- read.csv("C:/Users/alexa/PycharmProjects/Sportswashing/during_sample/llm_10000.csv", stringsAsFactors = FALSE)
df <- df %>%
  filter(Comment.Date != as.Date("2024-07-25"))

table(df$llm_sentiment)

sentiment_counts <- table(df$llm_sentiment)
pos <- ifelse("positive" %in% names(sentiment_counts), sentiment_counts["positive"], 0)
neg <- ifelse("negative" %in% names(sentiment_counts), sentiment_counts["negative"], 0)
difference <- as.numeric(pos - neg)


df$Date <- as.Date(df$Comment.Date)

daily_diff <- df %>%
  filter(llm_sentiment %in% c("positive", "negative")) %>%
  group_by(Date, llm_sentiment) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = llm_sentiment, values_from = n, values_fill = 0) %>%
  mutate(Difference = positive - negative)

a <- ggplot(daily_diff, aes(x = Date, y = Difference, fill = Difference > 0)) +
  geom_col(width = 0.8, show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "forestgreen", "FALSE" = "firebrick")) +
  geom_hline(yintercept = 0, color = "black") +
  labs(x = "Date", y = "Net Sentiment"
  ) +
  scale_x_date(date_labels = "%b %d", date_breaks = "3 days") +  
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    axis.text = element_text(size = 12),              
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 13)
  )

ggsave(
  "C:/Users/alexa/Documents/sportswashing/DailySentimentBalance.pdf",
  plot = a,
  width = 7.5, height = 5, dpi = 300
)


total <- pos + neg
share_df <- data.frame(
  Sentiment = c("Positive", "Negative"),
  Share = c(pos, neg) / total
)

b <- ggplot(share_df, aes(x = Sentiment, y = Share, fill = Sentiment)) +
  geom_col(width = 0.6, show.legend = TRUE) +
  geom_text(aes(label = percent(Share, accuracy = 1)), 
            vjust = -0.4, size = 5, fontface = "bold") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Positive" = "forestgreen", "Negative" = "firebrick")) +
  labs(
    y = "Percentage of Comments", x = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_blank(),       
    axis.ticks.x = element_blank(),   
    legend.position = "bottom",         
    plot.title = element_text(face = "bold", size = 16)
  ) +
  coord_cartesian(ylim = c(0, 0.75))   

ggsave(
  filename = "C:/Users/alexa/Documents/sportswashing/OverallSentimentDistribution.pdf",
  plot = b,
  width = 7.5, height = 5, dpi = 300
)


df$Comment.Date <- as.Date(df$Comment.Date)

c <- ggplot(df[df$llm_sentiment %in% c("positive", "negative"), ], 
       aes(x = Comment.Date, fill = llm_sentiment)) +
  geom_bar(position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c("positive" = "forestgreen", "negative" = "firebrick")) +
  labs(x = "Date", y = "Number of Comments", fill = "Sentiment") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom"
  )

ggsave(
  filename = "C:/Users/alexa/Documents/sportswashing/DailySentimentTrend.pdf",
  plot = c,
  width = 7.5, height = 5, dpi = 300
)






