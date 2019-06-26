library(rtweet)
library(ggmap)
library(tidytext)
library(ggplot2)
library(topicmodels)
library(dplyr)
library(SnowballC)

source("code/twitter-tokens.R")

create_token(
    app = appname,
    consumer_key = consumer_key,
    consumer_secret = consumer_secret,
    access_token = access_token,
    access_secret = access_token_secret,
    set_renv = TRUE
) -> twitter_token

chitweets <-search_tweets("-filter:replies AND -filter:quote",
                          n = 18000,
                          type = "recent",
                          retryonratelimit = FALSE,
                          include_rts = FALSE,
                          geocode = "41.50,-87.41,10mi")   # Chicago

# corpus: the tidy way
tidychi <- chitweets %>%
    unnest_tokens("word", text, to_lower = TRUE)

# remove stop words: the tidy way
data("stop_words")
my_stop_words <- tibble(
    word = c("https", "t.co", "amp","RT", "i", "i'm", "the")
)
tidychi <- tidychi %>%
    anti_join(stop_words, by = c("word"))
tidychi <- tidychi %>%
    anti_join(my_stop_words, by = c("word"))

tidychi %>% count(word) %>% arrange(desc(n))

tidychi<-tidychi[-grep("\\b\\d+\\b", tidychi$word),]
tidychi$word <- gsub("\\s+","",tidychi$word)

tidychi<-tidychi %>%
    mutate_at("word", funs(wordStem((.), language="en")))

# top words
chi_top_words <- tidychi %>% count(word) %>% arrange(desc(n))

#create factor variable to sort by frequency
chi_top_words$word <- factor(chi_top_words$word,
                             levels = chi_top_words$word[order(chi_top_words$n,
                                                               decreasing=TRUE)])
top_20<-chi_top_words[1:20,]

ggplot(top_20, aes(x=word, y=n))+
    geom_bar(stat="identity")+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ylab("Number of Times Word Appears")+
    xlab("")+
    guides(fill=FALSE)


# document-term matrix: the tidy way
tidychi <- tidychi %>% count(word, created_at) %>% bind_tf_idf(word, created_at, n)
chi_dtm<-
    tidychi %>%
    count(created_at, word) %>%
    cast_dtm(created_at, word, n)

chi_models <- LDA(chi_dtm, k=10, control = list(seed = 321))
chi_topics <- tidy(chi_models, matrix = "beta")

chi_top_terms <-
    chi_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

chi_top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()
ggsave("chi_twitter_topics.png", width = 20, height = 14, units = "cm")

# subset docs/dictionary approach
dict <- readLines("../siccs_group/good_dictionary")
tidychi_dict <- tidychi[grepl(paste(dict, collapse = "|"), tidychi$word,
                                perl = TRUE, ignore.case = TRUE),]
tidychi_dict <- tidychi_dict[, c("word", "created_at", "n")]
tidychi_dict <- tidychi_dict %>% count(word, created_at) %>%
    bind_tf_idf(word, created_at, n)

chi_dict_dtm <-
    tidychi_dict %>%
    count(created_at, word) %>%
    cast_dtm(created_at, word, n)

chi_dict_models <- LDA(chi_dict_dtm, k=10, control = list(seed = 321))
chi_dict_topics <- tidy(chi_dict_models, matrix = "beta")

chi_dict_top_terms <-
    chi_dict_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)


chi_dict_top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()
ggsave("chi_twitter_topics_w_dict.png", width = 20, height = 14, units = "cm")

### Chicago-related twitter handles
# @suntimes
# @chicagotribune
# @chicagosmayor (?)
