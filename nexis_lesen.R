library(tidyverse)
library(striprtf)
library(lubridate)
library(stringi)

# Function to check if a string is a valid date
is_date <- function(x) {
  # Try to parse using flexible orders
  parsed <- parse_date_time(
    x,
    orders = c(
      "BdY",        # August 10, 2024
      "BdYA",      # August 10, 2024 Saturday
      "bdY",        # Aug 10, 2024
      "bdYA"      # Aug 10, 2024 Sat
    ),
  )
  !is.na(parsed)
}

# Folders (use forward slashes on Windows in R)
in_dir  <- "C:/Users/alexa/Downloads/nexis/"
out_dir <- "C:/Users/alexa/Downloads/nexis/txt_files"

# Ensure output directory exists
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Replacement strings
header_line <- "Start of document"   
exact_find  <- "End of Document"               # case-sensitive match

# Find all .rtf files (case-insensitive), recursively
rtf_files <- list.files(in_dir, pattern = "\\.rtf$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)


df_list <- list()

for (f in 1:length(rtf_files)) {
  txt <- read_rtf(rtf_files[f])
  
  # Replace ALL exact occurrences (case-sensitive) of "End of Document"
  txt <- gsub(exact_find, header_line, txt, fixed = TRUE)
  
  # Prepend the header line at the very beginning of the file
  txt <- c(header_line, txt)

  # Build output path (flattened into one folder, keeping just the filename)
  out_file <- file.path(out_dir, paste0(tools::file_path_sans_ext(basename(rtf_files[f])), ".txt"))
  source <- str_remove(basename(out_file), "\\.txt$")
  source_clean <- str_remove(source, "\\d+$")
  
  # Write out as .txt without changing bytes (avoid accidental re-encoding)
  writeLines(txt, con = out_file, useBytes = TRUE)
  
  df <- as.data.frame(txt) %>% 
    filter(txt!="") %>% 
    mutate(
      source = source,
      source_clean = source_clean,
      txt  = str_squish(txt),
      is_start  = str_detect(txt, fixed(header_line, ignore_case = TRUE)),
      doc_id    = cumsum(is_start),
      doc_id = paste0(source, "_", doc_id)
    ) %>% 
    mutate( 
      words     = str_count(txt, boundary("word")),
      candidate = if_else(words == 4, txt, NA_character_),   # nur kurze Strings
      date_flag = is_date(candidate),                        # sicher: NA bleibt NA -> FALSE unten
      date_flag = replace_na(date_flag, FALSE),
      date      = if_else(date_flag, txt, NA_character_)
    ) %>%
    group_by(doc_id) %>%
    mutate(
      doc_date = first(na.omit(date)),
      title = str_squish(dplyr::nth(txt, 2, default = NA_character_))
    ) %>%
    ungroup() %>% 
    select(-c(candidate,is_start,date_flag,date)) %>%
    filter(words >= 10) %>% 
    group_by(doc_id) %>% 
    mutate(par_id = paste0(doc_id,"_", row_number())) %>% 
    ungroup() 
    
  df_list[[f]] <- df
}

df_full <- bind_rows(df_list)


# resolve problem with missing dates
#missings <- unique(df_full$source[is.na(df_full$doc_date)])
#rtf_new <- paste0("C:/Users/alexa/Downloads/nexis/", missings, ".RTF")
#txt_new <- paste0("C:/Users/alexa/Downloads/nexis/txt_files/", missings, ".txt")


##################################



txt_new <- list.files(out_dir, pattern = "\\.txt$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)


df_list <- list()

for (f in 1:length(txt_new)) {
  txt <- readLines(txt_new[f])
  
  source <- tools::file_path_sans_ext(basename(txt_new[f]))
  source_clean <- str_remove(source, "\\d+$")

  df <- as.data.frame(txt) %>% 
    filter(txt!="") %>% 
    filter(txt!=" ") %>% 
    mutate(
      source = source,
      source_clean = source_clean,
      txt  = str_squish(txt),
      is_start  = str_detect(txt, fixed(header_line, ignore_case = TRUE)),
      doc_id    = cumsum(is_start),
      doc_id = paste0(source, "_", doc_id)
    ) %>% 
    mutate( 
      words     = str_count(txt, boundary("word")),
      candidate = if_else(words == 4, txt, NA_character_),   # nur kurze Strings
    ) %>%
    group_by(doc_id) %>%
    mutate(
      date = ifelse(length(txt) >= 4, txt[4], NA),
      doc_date = first(na.omit(date)),
      title = str_squish(dplyr::nth(txt, 2, default = NA_character_))
    ) %>%
    ungroup() %>% 
    select(-c(candidate,is_start,date)) %>%
    filter(words >= 10) %>% 
    group_by(doc_id) %>% 
    mutate(par_id = paste0(doc_id,"_", row_number())) %>% 
    ungroup() 
  
  df_list[[f]] <- df
}

df_full_complete <- bind_rows(df_list)

#df_full_old <- df_full[!is.na(df_full$doc_date),]
#df_full_complete <- rbind(df_full_old, df_full_new)

#write_rds(df_full_complete, "full_dataset.RDS")


# convert date to common format ----------------


x=unique(df_full_complete$doc_date, max=Inf)
x <- x[grepl("august|july|september|julio|agosto|juillet|vendredi|dimanche|samedi|mercredi|jeudi|mardi|lundi|juli|domingo|GMT|samstag|freitag|mittwoch|dienstag|donnerstag|montag|sonntag|julho|june|luglio", x, ignore.case = TRUE)]
df_full_complete$doc_date1 <- ifelse(df_full_complete$doc_date %in% x, df_full_complete$doc_date, NA) 

month_map <- c(
  # German
  "Juli"="July","August"="August",
  # French
  "Juillet"="July", "Août"="August", "Aout"="August", 
  # Portuguese
  "Julho"="July", "agosto"="August",
  # Spanish
  "Julio"="July", "Agosto"="August",
  # ?
  "juli"="July", "augustus"="August",   "Julyo"="July", "luglio"="July"
)

# --- 2. Clean & translate function
clean_dates <- function(x) {
  x |>
    # normalize accents to Latin letters
    stri_trans_general("Latin-ASCII") |>
    # replace month names to English
    str_replace_all(month_map) |>
    # remove weekday names (common DE/FR/PT)
    str_remove_all("\\b(Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday|
                   Montag|Dienstag|Mittwoch|Donnerstag|Freitag|Samstag|Sonntag|
                   Lundi|Mardi|Mercredi|Jeudi|Vendredi|Samedi|Dimanche|
                   lunedi|mardi|mercoledi|giovedi|venerdi|sabato|domenica|
                   Segunda-feira|Terca-feira|Quarta-feira|Quinta-feira|Sexta-feira|
                   lunes|martes|miercoles|jueves|viernes|sabado|domingo|
                   maandag|dinsdag|woensdag|donderdag|vrijdag|zaterdag|
                   PM|AM|EST|BST|PDT)\\b") |>
    str_squish()
}

# --- 3. Apply cleaning
df_full_complete$date_clean <- clean_dates(df_full_complete$doc_date1)
#unique(df_full_complete$date_clean)

df_full_complete$date_clean1 <- as.Date(parse_date_time(df_full_complete$date_clean,
  orders = c(
    "BdY",        # August 10, 2024
    "dBY",      
    "YBd",
    "BdYHM")
))

unique(df_full_complete$date_clean[is.na(df_full_complete$date_clean1)])
table(df_full_complete$date_clean1)

df_full_complete$outside_range <- ifelse(df_full_complete$date_clean1>as.Date("2024-08-11"), TRUE, FALSE)

df_full_complete <- df_full_complete %>%
  filter(outside_range == FALSE)


# Limit to those mentioning Paris or France ------------------


terms <- c("paris", "parís",
           "france", "frankreich", "francia", "frança", "fransa")

pattern <- paste0(
  "\\b(",
  paste(terms, collapse = "|"),
  ")(?:'s|s)?\\b"
)

df_full_complete <- df_full_complete %>%
  mutate(
    mentions_france_paris = str_detect(
      txt,
      regex(pattern, ignore_case = TRUE)
    )
  )

table(df_full_complete$mentions_france_paris)

df_filtered <- df_full_complete %>%
  filter(mentions_france_paris == TRUE)

# Pfad zur Datei
file_path <- "C:/Users/alexa/Documents/sportswashing/"

# CSV einlesen
df <- read.csv(paste0(file_path, "NexisUni_list-1.csv"), stringsAsFactors = FALSE)

df <- df[, c("Name", "Country", "Continent", "Type_newspaper", "R_Name")]
df <- df[ df$R_Name != "" & !is.na(df$R_Name), ]
df_joined <- df_filtered %>%
  left_join(df, by = c("source_clean" = "R_Name"))

df_joined[df_joined$source_clean == "USAToday", c("Name", "Country", "Continent", "Type_newspaper")] <- 
  list("USA Today Online", "USA", "North America", "broadsheet")
df_joined$Country[df_joined$Country=="Camer\xfan"] <- "Camerun"


df_joined %>%
  mutate(date_clean1 = as.Date(date_clean1)) %>%
  distinct(doc_id, date_clean1, Country) %>%   # 1 document per day per country
  count(Country, date_clean1, name = "n_docs") %>%
  ggplot(aes(x = date_clean1, y = n_docs)) +
  geom_col() +
  facet_wrap(~Country, scales = "free_y", ncol = 5) +
  labs(
    x = "Date",
    y = "Number of paragraphs"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste0(file_path, "plotycountry.pdf"), width=12, height=7)



df_joined %>%
  group_by(Country) %>%
  summarise(n_docs = n_distinct(doc_id)) %>%
  ggplot(aes(x = reorder(Country, -n_docs), y = n_docs)) +
  geom_col() +
  labs(
    x = "Region",
    y = "Articles"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

df_joined %>%
  mutate(date_clean1 = as.Date(date_clean1)) %>%
  distinct(doc_id, date_clean1) %>%          # jedes Dokument pro Tag nur 1×
  count(date_clean1, name = "n_docs") %>%    # Anzahl doc_id pro Tag
  ggplot(aes(x = date_clean1, y = n_docs)) +
  geom_point(size = 3) +                     # EIN Punkt pro Tag
  labs(
    title = "Number of Documents per Day",
    x = "Date",
    y = "Count of doc_id"
  ) +
  theme_minimal()

length(unique(df_joined$source_clean))

script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

write.csv(df_joined, file.path(script_dir, "df_joined.csv"), row.names = FALSE)


df_joined$random_order <- sample(1:nrow(df_joined))

unique(table(df_joined$random_order))

write.csv

df_joined_sample <- df_joined[df_joined$random_order %in% 1:2000, ]

write.csv(df_joined_sample, file.path(script_dir, "df_joined_sample.csv"), row.names = FALSE)




# =========================
# LOAD PROCESSED DATA ONLY
# =========================
library(tidyverse)
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
df_joined <- read.csv(file.path(script_dir, "df_joined.csv"), stringsAsFactors = FALSE)
df_joined$date_clean1 <- as.Date(df_joined$date_clean1)






