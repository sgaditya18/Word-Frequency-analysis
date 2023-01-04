install.packages("readr")
library(readr)
install.packages('tm')
library('tm')
install.packages('dplyr')
library('dplyr')
install.packages("tidytext")
library(tidytext)
install.packages("ggplot2")
library(ggplot2)
install.packages("tidyr")
library(tidyr)
install.packages("ggraph")
library(ggraph)
install.packages("igraph")
library(igraph)



##reading csv of each year 

d2017 <- read.csv("2017.csv")
d2018 <- read.csv("2018.csv")
d2019 <- read.csv("2019.csv")
d2020 <- read.csv("2020.csv")
d2021 <- read.csv("2021.csv")
d2022 <- read.csv("2022.csv")

##function to cleanse the tweets
data_clean <- function(x){
  x$tweet <- gsub("@\\w+", "", x$tweet)  ##removing mentions present in the tweet
  x$tweet <- gsub("(f|ht)tp\\S+\\s*", "", x$tweet)  ##removing hyperlinks shared in the tweet
  x$tweet <- gsub("[[:punct:]]", "", x$tweet)     ##removing punctuations in the tweet
  x$tweet <- trimws(x$tweet, which = c("both","left","right"), whitespace = "[\t\r\n]") ##trimming the tweets
}


##converting the tables into dataframes by calling by the cleansing function

d2017 <- data.frame(data_clean(d2017))
d2018 <- data.frame(data_clean(d2018))
d2019 <- data.frame(data_clean(d2019))
d2020 <- data.frame(data_clean(d2020))
d2021 <- data.frame(data_clean(d2021))
d2022 <- data.frame(data_clean(d2022))

##creating a stopwords dataframe for English language 
stopwords <- data.frame(word = stopwords(kind = "en"))


##word frequency for 2017 by splitting the column into tokens and excluding stopwords 
y2017 <- d2017 %>%
  unnest_tokens(word, data_clean.d2017.) %>%
  anti_join(stopwords)
freq2017 <- y2017 %>% count(word) %>% arrange(desc(n))
head(freq2017,10)       ##top 10 words by high word frequency

##word frequency for 2018 by splitting the column into tonkens and excluding stopwords 
y2018 <- d2018 %>%
  unnest_tokens(word, data_clean.d2018.) %>%
  anti_join(stopwords)
freq2018 <- y2018 %>% count(word) %>% arrange(desc(n))
head(freq2018,10)        ##top 10 words by high word frequency

##word frequency for 2019 by splitting the column into tonkens and excluding stopwords 

y2019 <- d2019 %>%
  unnest_tokens(word, data_clean.d2019.) %>%
  anti_join(stopwords)
freq2019 <- y2019 %>% count(word) %>% arrange(desc(n))
head(freq2019,10)        ##top 10 words by high word frequency

##word frequency for 2020 by splitting the column into tonkens and excluding stopwords 
y2020 <- d2020 %>%
  unnest_tokens(word, data_clean.d2020.) %>%
  anti_join(stopwords)
freq2020 <- y2020 %>% count(word) %>% arrange(desc(n))
head(freq2020,10)        ##top 10 words by high word frequency

##word frequency for 2021 by splitting the column into tonkens and excluding stopwords 
y2021 <- d2021 %>%
  unnest_tokens(word, data_clean.d2021.) %>%
  anti_join(stopwords)
freq2021 <- y2021 %>% count(word) %>% arrange(desc(n))
head(freq2021,10)          ##top 10 words by high word frequency

##word frequency for 2022 by splitting the column into tonkens and excluding stopwords 
y2022 <- d2022 %>%
  unnest_tokens(word, data_clean.d2022.) %>%
  anti_join(stopwords)
freq2022 <- y2022 %>% count(word) %>% arrange(desc(n))
head(freq2022,10)          ##top 10 words by high word frequency


#Histogram For 2017
ggplot(head(freq2017,10), aes(x = reorder(word, -n), y = n, fill = word)) + geom_col() +
  labs(title = "Top 10 Words Frequency for 2017", x = "Word", y = "Frequency")


#Histogram For 2018
ggplot(head(freq2018,10), aes(x = reorder(word, -n), y = n, fill = word)) + geom_col() +
  labs(title = "Top 10 Words Frequency for 2018", x = "Word", y = "Frequency")

#Histogram For 2019
ggplot(head(freq2019,10), aes(x = reorder(word, -n), y = n, fill = word)) + geom_col() +
  labs(title = "Top 10 Words Frequency for 2019", x = "Word", y = "Frequency")

#Histogram For 2020
ggplot(head(freq2020,10), aes(x = reorder(word, -n), y = n, fill = word)) + geom_col() +
  labs(title = "Top 10 Words Frequency for 2020", x = "Word", y = "Frequency")


#Histogram For 2021
ggplot(head(freq2021,10), aes(x = reorder(word, -n), y = n, fill = word)) + geom_col() +
  labs(title = "Top 10 Words Frequency for 2021", x = "Word", y = "Frequency")

#Histogram For 2022
ggplot(head(freq2019,10), aes(x = reorder(word, -n), y = n, fill = word)) + geom_col() +
  labs(title = "Top 10 Words Frequency for 2022", x = "Word", y = "Frequency")

##Adding year field to the data 

freq2017$year <- 2017
freq2018$year <- 2018
freq2019$year <- 2019
freq2020$year <- 2020
freq2021$year <- 2021
freq2022$year <- 2022


##calculating term frequency by rank for 2017
g17 <- freq2017 %>%
  group_by(year) %>%
  summarize(total = sum(n))
freq2017 <- left_join(freq2017, g17)    ##Adding total field to the df
freqrank2017 <- freq2017 %>%
  group_by(year) %>%
  mutate(rank = row_number(), `term frequency` = n / total) %>%
  ungroup()

rank_subset17 <- freqrank2017 %>%         ##filtering for first 500 ranks
  filter(
    rank < 500,
  )

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset17)  ##fitting the linear model by taking log of rank and term frequency


##Adding an absolute line from the intercept and slope values obtained above 
freqrank2017 %>%
  ggplot(aes(rank, `term frequency`, color = year)) +
  labs(title = "Log-Log plot for 2017", x = "Rank", y = "Term Frequency") +
  geom_abline(intercept = -1.6296, slope = -0.6505,
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()


##calculating term frequency by rank for 2018

g18 <- freq2018 %>%
  group_by(year) %>%
  summarize(total = sum(n))
freq2018 <- left_join(freq2018, g18)    ##Adding total field to the df
freqrank2018 <- freq2018 %>%
  group_by(year) %>%
  mutate(rank = row_number(), `term frequency` = n / total) %>%
  ungroup()

rank_subset18 <- freqrank2018 %>%         ##filtering for first 500 ranks
  filter(
    rank < 500,
  )

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset18)  ##fitting the linear model by taking log of rank and term frequency


##Adding an absolute line from the intercept and slope values obtained above 
freqrank2018 %>%
  ggplot(aes(rank, `term frequency`, color = year)) +
  labs(title = "Log-Log plot for 2018", x = "Rank", y = "Term Frequency") +
  geom_abline(intercept = -1.6323, slope = -0.6503,
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()


##calculating term frequency by rank for 2019

g19 <- freq2019 %>%
  group_by(year) %>%
  summarize(total = sum(n))
freq2019 <- left_join(freq2019, g19)    ##Adding total field to the df
freqrank2019 <- freq2019 %>%
  group_by(year) %>%
  mutate(rank = row_number(), `term frequency` = n / total) %>%
  ungroup()

rank_subset19 <- freqrank2019 %>%         ##filtering for first 500 ranks
  filter(
    rank < 500,
  )

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset19)  ##fitting the linear model by taking log of rank and term frequency


##Adding an absolute line from the intercept and slope values obtained above 
freqrank2019 %>%
  ggplot(aes(rank, `term frequency`, color = year)) +
  labs(title = "Log-Log plot for 2019", x = "Rank", y = "Term Frequency") +
  geom_abline(intercept = -1.6372, slope = -0.6528,
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()


##calculating term frequency by rank for 2020

g20 <- freq2020 %>%
  group_by(year) %>%
  summarize(total = sum(n))
freq2020 <- left_join(freq2020, g20)    ##Adding total field to the df
freqrank2020 <- freq2020 %>%
  group_by(year) %>%
  mutate(rank = row_number(), `term frequency` = n / total) %>%
  ungroup()

rank_subset20 <- freqrank2020 %>%         ##filtering for first 500 ranks
  filter(
    rank < 500,
  )

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset20)  ##fitting the linear model by taking log of rank and term frequency


##Adding an absolute line from the intercept and slope values obtained above 
freqrank2020 %>%
  ggplot(aes(rank, `term frequency`, color = year)) +
  labs(title = "Log-Log plot for 2020", x = "Rank", y = "Term Frequency") +
  geom_abline(intercept = -1.6525, slope = -0.6473,
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()


##calculating term frequency by rank for 2021

g21 <- freq2021 %>%
  group_by(year) %>%
  summarize(total = sum(n))
freq2021 <- left_join(freq2021, g21)    ##Adding total field to the df
freqrank2021 <- freq2021 %>%
  group_by(year) %>%
  mutate(rank = row_number(), `term frequency` = n / total) %>%
  ungroup()

rank_subset21 <- freqrank2021 %>%         ##filtering for first 500 ranks
  filter(
    rank < 500,
  )

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset21)  ##fitting the linear model by taking log of rank and term frequency


##Adding an absolute line from the intercept and slope values obtained above 
freqrank2021 %>%
  ggplot(aes(rank, `term frequency`, color = year)) +
  labs(title = "Log-Log plot for 2021", x = "Rank", y = "Term Frequency") +
  geom_abline(intercept = -1.6468, slope = -0.6341,
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()


##calculating term frequency by rank for 2022

g22 <- freq2022 %>%
  group_by(year) %>%
  summarize(total = sum(n))
freq2022 <- left_join(freq2022, g22)    ##Adding total field to the df
freqrank2022 <- freq2022 %>%
  group_by(year) %>%
  mutate(rank = row_number(), `term frequency` = n / total) %>%
  ungroup()

rank_subset22 <- freqrank2022 %>%         ##filtering for first 500 ranks
  filter(
    rank < 500,
  )

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset22)  ##fitting the linear model by taking log of rank and term frequency


##Adding an absolute line from the intercept and slope values obtained above 
freqrank2022 %>%
  ggplot(aes(rank, `term frequency`, color = year)) +
  labs(title = "Log-Log plot for 2022", x = "Rank", y = "Term Frequency") +
  geom_abline(intercept = -1.7492, slope = -0.5473,
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()


##Creating bigram network graph for each year 

##2017
dt2017_bigrams <- d2017 %>%
  unnest_tokens(bigram, data_clean.d2017., token = "ngrams", n = 2)

# Counting the bigrams
dt2017_bigrams %>%
  count(bigram, sort = TRUE)
dt2017_bigrams_separated <- dt2017_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

dt2017_bigrams_filtered <- dt2017_bigrams_separated %>%
  filter(!word1 %in% stopwords$word) %>%
  filter(!word2 %in% stopwords$word)
dt2017_bigrams_counts <- dt2017_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
dt2017_bigrams_counts



# Plotting Bigram graph
dt2017_bigrams_graph <- dt2017_bigrams_counts %>%
  filter(n > 10) %>%
  graph_from_data_frame()


set.seed(2017)
g <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(dt2017_bigrams_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = g, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

##2018
dt2018_bigrams <- d2018 %>%
  unnest_tokens(bigram, data_clean.d2018., token = "ngrams", n = 2)

# Counting the bigrams
dt2018_bigrams %>%
  count(bigram, sort = TRUE)
dt2018_bigrams_separated <- dt2018_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

dt2018_bigrams_filtered <- dt2018_bigrams_separated %>%
  filter(!word1 %in% stopwords$word) %>%
  filter(!word2 %in% stopwords$word)
dt2018_bigrams_counts <- dt2018_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
dt2018_bigrams_counts



# Plotting Bigram graph
dt2018_bigrams_graph <- dt2018_bigrams_counts %>%
  filter(n > 10) %>%
  graph_from_data_frame()


set.seed(2020)
g <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(dt2018_bigrams_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = g, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


##2019
dt2019_bigrams <- d2019 %>%
  unnest_tokens(bigram, data_clean.d2019., token = "ngrams", n = 2)

# Counting the bigrams
dt2019_bigrams %>%
  count(bigram, sort = TRUE)
dt2019_bigrams_separated <- dt2019_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

dt2019_bigrams_filtered <- dt2019_bigrams_separated %>%
  filter(!word1 %in% stopwords$word) %>%
  filter(!word2 %in% stopwords$word)
dt2019_bigrams_counts <- dt2019_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
dt2019_bigrams_counts



# Plotting Bigram graph
dt2019_bigrams_graph <- dt2019_bigrams_counts %>%
  filter(n > 10) %>%
  graph_from_data_frame()


set.seed(2020)
g <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(dt2019_bigrams_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = g, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

##2020
dt2020_bigrams <- d2020 %>%
  unnest_tokens(bigram, data_clean.d2020., token = "ngrams", n = 2)

# Counting the bigrams
dt2020_bigrams %>%
  count(bigram, sort = TRUE)
dt2020_bigrams_separated <- dt2020_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

dt2020_bigrams_filtered <- dt2020_bigrams_separated %>%
  filter(!word1 %in% stopwords$word) %>%
  filter(!word2 %in% stopwords$word)
dt2020_bigrams_counts <- dt2020_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
dt2020_bigrams_counts



# Plotting Bigram graph
dt2020_bigrams_graph <- dt2020_bigrams_counts %>%
  filter(n > 10) %>%
  graph_from_data_frame()


set.seed(2020)
g <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(dt2020_bigrams_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = g, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


##2021
dt2021_bigrams <- d2021 %>%
  unnest_tokens(bigram, data_clean.d2021., token = "ngrams", n = 2)

# Counting the bigrams
dt2021_bigrams %>%
  count(bigram, sort = TRUE)
dt2021_bigrams_separated <- dt2021_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

dt2021_bigrams_filtered <- dt2021_bigrams_separated %>%
  filter(!word1 %in% stopwords$word) %>%
  filter(!word2 %in% stopwords$word)
dt2021_bigrams_counts <- dt2021_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
dt2021_bigrams_counts



# Plotting Bigram graph
dt2021_bigrams_graph <- dt2021_bigrams_counts %>%
  filter(n > 10) %>%
  graph_from_data_frame()


set.seed(2020)
g <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(dt2021_bigrams_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = g, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


##2022
dt2022_bigrams <- d2022 %>%
  unnest_tokens(bigram, data_clean.d2022., token = "ngrams", n = 2)

# Counting the bigrams
dt2022_bigrams %>%
  count(bigram, sort = TRUE)
dt2022_bigrams_separated <- dt2022_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

dt2022_bigrams_filtered <- dt2022_bigrams_separated %>%
  filter(!word1 %in% stopwords$word) %>%
  filter(!word2 %in% stopwords$word)
dt2022_bigrams_counts <- dt2022_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
dt2022_bigrams_counts



# Plotting Bigram graph
dt2022_bigrams_graph <- dt2022_bigrams_counts %>%
  filter(n > 4) %>%
  graph_from_data_frame()


set.seed(2022)
g <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(dt2022_bigrams_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = g, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()








