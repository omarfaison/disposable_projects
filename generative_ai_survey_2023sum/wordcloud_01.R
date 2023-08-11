en_stop_words<-as.data.frame(stopwords("en")) 
names(en_stop_words)<-c("word")

challenges_stop_words<-raw %>%
  unnest_tokens(word, challenges) %>%
  count(word) %>%
  filter(n>max(n)/4)

challenges_short_words<-raw %>%
  unnest_tokens(word, challenges) %>%
  count(word) %>%
  summarize(word=word,
            ltrs=nchar(word),
            wrd_count=n) %>%
  filter(ltrs<=3)

benefits_stop_words<-raw %>%
  unnest_tokens(word, benefits) %>%
  count(word) %>%
  filter(n>max(n)/4)

feedback_stop_words<-raw %>%
  unnest_tokens(word, feedback) %>%
  count(word) %>%
  filter(n>max(n)/4)

stop_words <- select(challenges_stop_words, word) %>%
  full_join(select(benefits_stop_words, word)) %>%
  full_join(select(feedback_stop_words, word))

challenges_words<-raw %>%
  unnest_tokens(word, challenges) %>%
  select(word) %>%
  anti_join(challenges_short_words, by="word") %>%
  count(word) %>%
  arrange(desc(n))

challenges_words2<-raw %>%
  unnest_tokens(word, challenges) %>%
  select(word) %>%
  anti_join(en_stop_words, by="word") %>%
  count(word) %>%
  arrange(desc(n))

benefits_words<-raw %>%
  unnest_tokens(word, benefits) %>%
  count(word) %>%
  arrange(desc(n))

feedback_words<-raw %>%
  unnest_tokens(word, feedback) %>%
  count(word) %>%
  arrange(desc(n))