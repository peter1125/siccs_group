library(tm)
library(tidytext)
library(dplyr)
library(SnowballC)
library(topicmodels)
library(ggplot2)
library(stm)


findread <- function(d = NULL, pattern = "*txt"){
    if (is.null("d")){
        d <- data.frame(docidx = character(),
                        text = character(),
                        docname = character(),
                        docdate = Date())
    }
    l <- list.files("data/clean_agenda/", pattern = "*txt")
    for(k in l){
        txtfile <- file(paste0("data/clean_agenda/",k), "r")
        t <- readLines(txtfile)
        t <- paste0(unlist(t, recursive = FALSE), collapse = " ")
        idx <- as.character(match(k,l))
        ddate <- gsub("([0-9]{2})([0-9]{2})([0-9]{2}).txt",
                      "\\3-\\1-\\2",
                      k,
                      perl = TRUE)
        ddate <- as.Date(ddate, format = "%y-%m-%d")
        d <- rbind(d, data.frame(docidx = idx,
                                 text = t,
                                 docname = k,
                                 docdate = ddate),
                   stringsAsFactors = FALSE)
        close(txtfile)
    }
    d
}

dd <- findread()
dd$text <- as.character(dd$text)
dd$docname <- as.character(dd$docname)

# corpus: the tidy way
tidyres<- dd %>%
    unnest_tokens("words", input = text, to_lower = TRUE)

# remove stop words: the tidy way
data("stop_words")
names(stop_words) <- c("words", "lexicon")
tidyres <- tidyres %>%
    anti_join(stop_words, by = c("words"))
my_stop_words <- tibble(
    words = c("__", "___", "_n","a.m")
)
tidyres<-tidyres %>%
    anti_join(my_stop_words)


tidyres <- tidyres[-grep("\\b\\d+\\b", tidyres$words),] # remove numbers
tidyres <- tidyres[-grep("\\b\\d+", tidyres$words),] # remove numbers
tidyres <- tidyres[-grep("\\d+\\b", tidyres$words),] # remove numbers
tidyres$words <- gsub("\\s+","", tidyres$words) # remove white space
# stemming
tidyres <-tidyres %>%
    mutate_at("words", list(~wordStem((.), language="en")))

tidyres_tf_idf <- tidyres %>% count(words, docidx) %>%
    bind_tf_idf(words, docidx, n)

# document-term matrix: the tidy way
tidyres_DTM<-
    tidyres %>%
    count(docidx, words) %>%
    cast_dtm(docidx, words, n)

# topic models
agendamodels <- LDA(tidyres_DTM, k = 10, control = list(seed = 111))
ag_topics <- tidy(agendamodels, matrix = "beta")
ag_top_terms <-
    ag_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

ag_top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()
ggsave("agenda_topics.png", width = 20, height = 14, units = "cm")


# structured topic model
processed <- textProcessor(dd$text, metadata = dd)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta
First_STM <- stm(documents = out$documents, vocab = out$vocab,
                 K = 7, prevalence =~ docdate + docname ,
                 max.em.its = 75, data = out$meta,
                 init.type = "Spectral", verbose = FALSE)

png(filename = "agenda_stm_plot.png", height = 450, width = 800)
plot(First_STM)
dev.off()

findThoughts(First_STM, texts = dd$text,
             n = 1, topics = c(1,6))
findingk <- searchK(out$documents, out$vocab, K = c(3:10),
                    prevalence =~ docdate + docname,
                    data = meta, verbose=FALSE)

png(filename = "agenda_stm_k_checks.png", height = 450, width = 800)
plot(findingk)
dev.off()
