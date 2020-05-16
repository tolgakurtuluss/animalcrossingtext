crit <- read.csv("critic.csv")
items <- read.csv("items.csv")
review <- read.csv("user_reviews.csv")

library(dplyr)
library(tidytext)
library(tm)
library(ggplot2)
library(plotly)

#CRITICS ANALYSIS
crit %>%
  count(date) %>%
  ggplot(aes(x = n, y = date)) +
  geom_bar(stat = "identity") +
  ylab("") + xlab("Number") +
  ggtitle("Daily Number of Critics")

crit$text <- tolower(crit$text)
crit$text <- gsub("â€™", "'", crit$text)
crit$text <- gsub("ã©", "e", crit$text)
crit$text <- gsub("-", " ", crit$text)
crit$text <- gsub("; ", " ", crit$text)
crit$text <- gsub(": ", " ", crit$text)

a <- crit %>%
  filter(grade > mean(grade)) %>%
  select(grade, text) %>%
  unnest_tokens(word, text) %>%
  filter(!(word %in% c(stopwords('en')))) %>%
  count(word, sort = TRUE) %>%
  mutate(name = "abovemean")


b <- crit %>%
  filter(grade <= mean(grade)) %>%
  select(grade, text) %>%
  unnest_tokens(word, text) %>%
  filter(!(word %in% c(stopwords('en')))) %>%
  count(word, sort = TRUE) %>%
  mutate(name = "belowmean")

aa <- rbind(a,b)

aa %>%
  filter(!(word %in% c('animal','crossing'))) %>%
  group_by(name) %>%
  top_n(n = 8, n) %>%
  ggplot(aes(x = reorder_within(word, n, name), y = n, fill = name)) +
  geom_col(show.legend = FALSE) +
  xlab("") +
  ylab("Mean of all grades is equal to 90.63551") +
  coord_flip() +
  facet_wrap(~name, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Most Used Words")

#UNIQUE WORDS ANALYSIS
a1 <- a %>%
  filter(!(word %in% b$word))

b1 <- b %>%
  filter(!(word %in% a$word))

aa1 <- rbind(a1,b1)

aa1 %>%
  group_by(name) %>%
  top_n(n = 10, n) %>%
  ggplot(aes(x = reorder_within(word, n, name), y = n, fill = name)) +
  geom_col(show.legend = FALSE) +
  ylab("Unique words by above&below values") +
  xlab("") +
  coord_flip() +
  facet_wrap(~name, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Most Used Unique Words")


items %>%
  count(category,sort = TRUE) %>%
  mutate(perc = round(100*n/length(items$category),2)) %>%
  plot_ly(labels = ~category, values = ~perc, sort = TRUE) %>%
  add_pie(hole = 0.3) %>%
  layout(title = "Percentage of Categories")

items %>%
  select(category,orderable) %>%
  group_by(orderable) %>%
  count(category,sort = TRUE) %>%
  filter(!(orderable %in% c('TRUE','FALSE'))) %>%
  plot_ly(labels = ~category, values = ~n, sort = TRUE) %>%
  add_pie(hole = 0.3) %>%
  layout(title = "Most NA values by category")

items %>%
  select(name,category,sell_value,buy_value) %>%
  filter(sell_value != 'NA' & buy_value != 'NA') %>%
  mutate(diff = sell_value-buy_value) %>%
  arrange(desc(diff)) %>%
  filter(diff > -10) %>%
  group_by(category) %>%
  top_n(n = 5, diff) %>%
  ggplot(aes(x = reorder_within(name, diff, category), y = diff, fill = category)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~category, ncol = 3, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Most Profitable Items by Category")

  
  #REVIEW ANALYSIS
  
  review$text <- tolower(review$text)
  review$text <- gsub("â€˜", "'", review$text)
  review$text <- gsub("ã©", "e", review$text)
  review$text <- gsub("â€™", "'", review$text)
  review$text <- gsub("<(.*)>", "", review$text) #Remove Unicodes
  
  c <- review %>%
    select(text) %>%
    unnest_tokens(word, text) %>%
    filter(!(word %in% c(stopwords('en')))) %>%
    count(word, sort = TRUE) %>%
    mutate(cat="word")
  
  
  cc <- review %>%
    select(text) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    count(bigram, sort = TRUE)
  
  library(tidyr)
  
  bigrams_separated <- cc %>%
    separate(bigram, c("word1", "word2"), sep = " ")
  
  bigrams_filtered <- bigrams_separated %>%
    filter(!word1 %in% c(stopwords('en'))) %>%
    filter(!word2 %in% c(stopwords('en'))) %>%
    filter(!word1 %in% c('ð','à')) %>%
    filter(!word2 %in% c('ð','à'))
  
  c1 <- bigrams_filtered %>%
    unite(bigram, word1, word2, sep = " ") %>%
    mutate(cat="bigram")
  
  colnames(c) = c("text","n","cat")
  colnames(c1) = c("text","n","cat")
  
  dd<-rbind(c,c1)
  
  dd %>% group_by(cat) %>%
    top_n(n = 10, n) %>%
    ggplot(aes(x = reorder_within(text, n, cat), y = n, fill = cat)) +
    geom_col(show.legend = FALSE) +
    ylab("") +
    xlab("") +
    coord_flip() +
    facet_wrap(~cat, ncol = 2, scales = "free_y") +
    scale_x_reordered() +
    ggtitle("Comparison of Most Used Bigrams and Words")
