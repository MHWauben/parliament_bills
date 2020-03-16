# Working off tutorials: 
#https://rstudio-pubs-static.s3.amazonaws.com/265713_cbef910aee7642dc8b62996e38d2825d.html 
#https://rpubs.com/tsholliger/301914 

library(tm)
library(SnowballC)
library(magrittr)
library(tidytext)
library(ggplot2)
library(dplyr)

### PREPARE CORPUS ----
bill_corpus <- VCorpus(VectorSource(bill_text[bill_text$text != "", 'text']))
writeLines(head(strwrap(bill_corpus[[2]]), 15)) 

bill_corpus <- tm_map(bill_corpus, removePunctuation)
bill_corpus <- tm_map(bill_corpus, removeNumbers)
bill_corpus <- tm_map(bill_corpus, removeWords, stopwords("english"))

# Transform to lower case (need to wrap in content_transformer)
bill_corpus <- tm_map(bill_corpus,content_transformer(tolower))
bill_corpus <- tm_map(bill_corpus, stripWhitespace)

writeLines(head(strwrap(bill_corpus[[2]]), 15))
bill_corpus <- tm_map(bill_corpus, content_transformer(gsub), pattern = "“|”|—", replacement = "")
bill_corpus <- tm_map(bill_corpus, stemDocument)

# Create DocumentTermMatrix for analysis ----
dtm_bill <- DocumentTermMatrix(bill_corpus)

### EXPLORATORY CHECKS ----
# Which words appear at least 1000 times in the corpus?
findFreqTerms(dtm_bill, lowfreq = 1000, highfreq = Inf)
# Which words are at least .70 associated with 'health'?
findAssocs(dtm_bill, "health", .70)

# Create DocumentTermMatrix with TF-IDF weighting
dtm_bill_tfidf <- DocumentTermMatrix(bill_corpus, control = list(weighting = weightTfIdf))# Which words are at least .70 associated with 'health'?
health_words <- unlist(findAssocs(dtm_bill_tfidf, "health", .25))
rel_words <- data.frame(corr = health_words,
                        names = gsub('health\\.', '', names(health_words)),
                        row.names = NULL, stringsAsFactors = FALSE) %>%
  dplyr::mutate(weight = corr / max(corr))

# convert dtm into a df
df_bill_tfidf <- tidy(dtm_bill_tfidf)


# take the product of tf and idf and create new column labeled "tf_idf". Graph it. 
tidy_tfidf <- bind_tf_idf(df_bill_tfidf, term = 'term', document = 'document', n = 'count')

bill_tfidf <- tidy_tfidf %>% 
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(term, levels = rev(unique(term))),
         levs = factor(document, levels = 1:6)) %>%  
  group_by(document) %>% 
  top_n(6, wt = tf_idf) %>% 
  ungroup() 

ggplot(bill_tfidf %>% filter(!is.na(levs)), aes(word, tf_idf, fill = document)) +
  geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
  facet_wrap(~levs, ncol = 2, scales = "free") +
  coord_flip()

### Create categorisation of health-related or not ----
health_docs <- tidy_tfidf %>%
  left_join(rel_words, by = c('term' = 'names'))

health_tfidf <- tidy_tfidf %>%
  dplyr::mutate(cat = ifelse(document %in% health_docs$document, 'health', 'non-health'))
