# "~/Downloads/alice.rds"
library(tidyverse)
alice <- readRDS("~/PM 556/PM556-/Lecture Codes/alice.rds") 
alice

# Turn data into tidy format
library(tidytext)
alice |>
  unnest_tokens(token, text)# text is alice and token is the name of the column

library(dplyr)
alice |>
  unnest_tokens(token, text) |>
  count(token, sort = TRUE) # count tokens?

library(dplyr)
alice |>
  unnest_tokens(token, text) |>
  group_by(chapter) |>
  count(token) |>
  top_n(10, n) # top 10 tokens 

library(dplyr) # box plot of the top 10 words
library(ggplot2)
alice |>  # runs this whole code at once
  unnest_tokens(token, text) |>
  count(token) |>
  top_n(10, n) |>     # pipe straight into ggplot
  ggplot(aes(n, token)) + # need plus symbol 
  geom_col()

stop_words |>
  filter(lexicon == "snowball") |>
  pull(word)  # filter snowball stop words and pulls just the words

sort(table(stop_words$word), decreasing = TRUE) # removes duplicated stop words

# remove stop words and list the words and how many times it pops up 
alice |>
  unnest_tokens(token, text) |>
  anti_join(stop_words, by = c("token" = "word")) |>  # take words from this data set and remove them from this data set
  count(token, sort = TRUE) # by = represents the columns
# pipe into count to see if it works

# clean up code
alice |>
  unnest_tokens(word, text) |>
  anti_join(stop_words, by = c("word")) |>  # olumn exists in both data sets
  count(word, sort = TRUE)


 library(ggplot2)
alice |>
  unnest_tokens(word, text) |>
  anti_join(stop_words, by = c("word")) |>
  count(word, sort = TRUE) |>
  top_n(10, n) |>
  ggplot(aes(n, fct_reorder(word, n))) +
  geom_col()

# extract bigrams, n=2 represents the nuber of words you pull
alice |>
  unnest_ngrams(ngram, text, n = 2) # rename new column and list column from dataset you want to list 

alice |>
  unnest_ngrams(ngram, text, n = 2) |>
  count(ngram, sort = TRUE) # list the number of times ngram listed

# when the second word is alice
alice |>
  unnest_ngrams(ngram, text, n = 2) |>
  separate(ngram, into = c("word1", "word2"), sep = " ") |>  # separate into 2 words
  select(word1, word2) |>
  filter(word1 == "alice") |>
  count(word2, sort = TRUE) # words that come before alice

# TF-IDF in tidy verse
alice |>
  unnest_tokens(text, text) |>  # call columns text (new col, data set col)
  count(text, chapter) |>   # count # times appear in each Ch.
  bind_tf_idf(text, chapter, n) |>  # connect these columns into the table that identifes TF, IDF and Tf_IDF
  arrange(desc(tf_idf))   # sort tf_idf by a descending order, 

library(textdata)
get_sentiments('bing')  # lists the "emotion" of a text connotations
get_sentiments('afinn') # assigned value,based on how 
get_sentiments('nrc')   # collection of sentiments, categorizes based on those senitments 

# merge two dataset 
diff_by_chap <- alice |>
  unnest_tokens(word, text) |>
  inner_join(get_sentiments("bing")) |>  # adds senitment column into shared words (btwn datasets)
  group_by(chapter) |> 
  summarise(sentiment = sum(sentiment == "positive") - sum(sentiment == "negative"))  # create new variable, then taking the diffenrce of the positive and negative words


barplot(diff_by_chap$sentiment, names.arg = diff_by_chap$chapter)


# affinn lexicon
avg_by_chap <- alice |>
  unnest_tokens(word, text) |>
  inner_join(get_sentiments("afinn")) |> 
  group_by(chapter) |> 
  summarise(sentiment = mean(value))
  
  barplot(avg_by_chap$sentiment, names.arg = avg_by_chap$chapter)
  
# NRC Lexicon
  alice |>
    unnest_tokens(word, text) |>
    inner_join(get_sentiments("nrc")) |> 
    group_by(chapter) |> 
    summarise(sentiment = names(which.max(table(sentiment))))
 
   # remove pos, neg connotations
  nrc_fun <- get_sentiments("nrc")
  nrc_fun <- nrc_fun[!nrc_fun$sentiment %in% c("positive","negative"), ]
  
  alice |>
    unnest_tokens(word, text) |>
    inner_join(nrc_fun) |> 
    group_by(chapter) |> 
    summarise(sentiment = names(which.max(table(sentiment))))