library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)
library(modelsummary)


pfad <- "C:/Users/alexa/Documents/sportswashing/sent"
pfad2 <- "C:/Users/alexa/Documents/sportswashing"


df <- list.files(
  path = pfad,
  pattern = "\\.csv$",
  full.names = TRUE
) |>
  lapply(read_csv, show_col_types = FALSE) |>
  bind_rows()

x <- unique(df$Country)

df$Country[df$Country == "South Arabia"] <- "Saudi Arabia"

df_medallists <- read_csv(
  file.path(pfad2, "medallists.csv"),
  show_col_types = FALSE
)

df_medals_total <- read_csv(
  file.path(pfad2, "medals_total_Tokyo.csv"),
  show_col_types = FALSE
)


y <- unique(df_medallists$country)

x2 <- x[!x %in% y]
print(x2)

df_medallists <- df_medallists |>
  mutate(country = recode(
    country,
    "United States"  = "USA",
    "Great Britain"   = "United Kingdom",
    "Türkiye"  = "Turkey",
    #""  = "Costa Rica",
    #""   = "Venezuela",
    #""  = "Camerun",
    #""  = "South Arabia",
    "IR Iran"   = "Iran",
    #""  = "Nigeria", # no medals
    "Malaysia"  = "Malasia",
    #""   = "Uruguay",
    "Korea"  = "South Corea",
    #""  = "Luxemburg",
    #""  = "Chad"
  ))

df_all <- full_join(
  df_medallists,
  df_medals_total,
  by = c("nationality_code" = "Country Code")
)

df_all1 <- df_all |>
  mutate(
    medal_points_row = case_when(
      medal_type == "Gold Medal"   ~ 3L,
      medal_type == "Silver Medal" ~ 2L,
      medal_type == "Bronze Medal" ~ 1L,
      TRUE                         ~ 0L
    ),
    medal_count_row = if_else(is.na(medal_type), 0L, 1L)
  ) |>
  group_by(medal_date, country) |>
  mutate(
    medal_count  = sum(medal_count_row, na.rm = TRUE),
    paris_points = sum(medal_points_row, na.rm = TRUE)
  ) |>
  ungroup() |>
  select(-medal_count_row, -medal_points_row)

df_all1 <- df_all1 |>
  mutate(
    tokyo_points =
      3L * replace_na(`Gold Medal`, 0L) +
      2L * replace_na(`Silver Medal`, 0L) +
      1L * replace_na(`Bronze Medal`, 0L)
  )

df_all1 <- df_all1 |>
  rename(Total_Tokyo = Total)

df_all1 <- df_all1 |>
  select(-c(medal_code, nationality, nationality_code, nationality_long, discipline, gender, team, team_gender, event, event_type, url_event, birth_date, code_athlete, code_team, is_medallist, Rank))

df_all1 <- df_all1 |>
  mutate(medal_count = if_else(is.na(medal_type), NA_integer_, medal_count))

df_all1 <- df_all1 |>
  mutate(medal_count = replace_na(medal_count, 0))

df_all1 <- df_all1 |>
  mutate(Total_Tokyo = replace_na(Total_Tokyo, 0))  



df_all2 <- df_all1 |>
  mutate(
    medal_score_weighted = if_else(
      tokyo_points > 0,
      paris_points / tokyo_points,
      NA_real_
    )
  )

max_score <- max(df_all2$medal_score_weighted, na.rm = TRUE)

df_all2 <- df_all2 |>
  mutate(
    medal_score_weighted = if_else(
      tokyo_points == 0,
      max_score,
      medal_score_weighted
    )
  )

df_all2 <- df_all2 |>
  mutate(
    medal_score = if_else(
      Total_Tokyo > 0,
      medal_count / Total_Tokyo,
      NA_real_
    )
  )

max_score1 <- max(df_all2$medal_score, na.rm = TRUE)

df_all2 <- df_all2 |>
  mutate(
    medal_score = if_else(
      Total_Tokyo == 0,
      max_score,
      medal_score
    )
  )

df_all2 <- df_all2 |>
  mutate(
    country = if_else(is.na(country) | country == "", Country, country),
    country_long = if_else(is.na(country_long) | country_long == "", Country, country_long)
  )

df_all2 <- df_all2 |>
  mutate(
    Country = if_else(is.na(Country) | Country == "", country_long, Country)
  )

df_all2 <- df_all2 %>%
  mutate(medal_date = medal_date + 1)

setdiff(df$Country, df_all1$country)
setdiff(df_all1$country, df$Country)
unique(df$Country)


df_all2_key <- df_all2 %>%
  group_by(country, medal_date) %>%
  summarise(
    medal_count = first(medal_count),
    Total_Tokyo  = first(Total_Tokyo),
    medal_score        = first(medal_score),
    medal_score_weighted        = first(medal_score_weighted),
    paris_points = first(paris_points),
    tokyo_points = first(tokyo_points),
    .groups = "drop"
  )



df_merged <- df %>%
  left_join(df_all2_key,
            by = c("Country" = "country",
                   "date_clean1" = "medal_date"))

df_merged <- df_merged |>
  mutate(
    Country = recode(
      Country,
      "Camerun"     = "Cameroon",
      "Malasia"     = "Malaysia",
      "South Corea" = "South Korea",
      "Luxemburg"   = "Luxembourg"
    )
  )

df_merged <- df_merged %>%
  mutate(
    sentiment_num = case_when(
      sentiment == "positive" ~  1,
      sentiment == "neutral"  ~  0,
      sentiment == "negative" ~ -1,
      TRUE ~ NA_real_
    )
  )

df_merged1 <- df_merged %>%
  group_by(Country, date_clean1) %>%
  mutate(sentiment_score = mean(sentiment_num, na.rm = TRUE),
         medal_count = ifelse(is.na(medal_count), 0, medal_count),
         paris_points = ifelse(is.na(paris_points), 0, paris_points),
         number_par = n()) %>%
  ungroup()

df_merged1 <- df_merged1 %>%
  mutate(medal_score = if_else(is.na(medal_score), 0, medal_score),
         medal_score_weighted = if_else(is.na(medal_score_weighted), 0, medal_score_weighted))

df_merged1$Name[df_merged1$Name == "clarin"] <- "clarín"


df_plot <- df_merged1 |>
  distinct(Country, date_clean1, medal_score, medal_score_weighted, sentiment_score, medal_count, paris_points, number_par)


p2 <- ggplot(df_plot, aes(x = log(medal_score_weighted), y = sentiment_score)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Log weighted medal score",
    y = "Average sentiment"
  ) +
  theme_minimal()

ggsave("sentiment_regression.pdf", plot=p2)


m1 <- lm(sentiment_score ~ log(medal_score_weighted+1) + number_par, data=df_plot)
summary(m1)

m2 <- lm(sentiment_score ~ log(medal_score+1) + number_par, data=df_plot)
summary(m2)

m3 <- lm(sentiment_score ~ log(medal_score_weighted+1) + number_par + Country, data=df_plot)
summary(m3)

m4 <- lm(sentiment_score ~ log(medal_score+1) + number_par + Country, data=df_plot)
summary(m4)

m5 <- lm(sentiment_score ~ log(medal_score_weighted+1) + number_par + Country + as.factor(date_clean1), data=df_plot)
summary(m5)

m6 <- lm(sentiment_score ~ log(medal_score+1) + number_par + Country  + as.factor(date_clean1), data=df_plot)
summary(m6)



p1 <- ggplot(
  transform(df_plot, date = as.Date(date_clean1)),
  aes(date_clean1, sentiment_score)
) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~Country, ncol = 5) +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(
    x = "Date",
    y = "Average Sentiment"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("sentiment_by_country.pdf", plot=p1)



options(modelsummary_format_numeric_latex = "plain")




models <- list("M1"=m1, "M2"=m2, "M3"=m3, "M4"=m4, "M5"=m5, "M6"=m6)

coef_map <- c(
  "log(medal_score_weighted + 1)" = "log(Weighted medal score + 1)",
  "log(medal_score + 1)"          = "log(Medal score + 1)",
  "number_par"                    = "Number of paragraphs"
)

gof_map <- data.frame(
  raw   = c("nobs", "r.squared", "adj.r.squared"),
  clean = c("N", "R$^2$", "Adj. R$^2$"),
  fmt   = c(0, 3, 3)
)

add_rows <- data.frame(
  term = c("Country fixed effects", "Date fixed effects"),
  M1 = c("No",  "No"),
  M2 = c("No",  "No"),
  M3 = c("Yes", "No"),
  M4 = c("Yes", "No"),
  M5 = c("Yes", "Yes"),
  M6 = c("Yes", "Yes"),
  check.names = FALSE
)

tab_latex <- modelsummary(
  models,
  output    = "latex",
  coef_map  = coef_map,
  gof_map   = gof_map,
  add_rows  = add_rows,
  statistic = "({std.error})",
  stars     = TRUE,
  title     = "OLS regressions: Sentiment and medal performance"
)

cat(as.character(tab_latex))









