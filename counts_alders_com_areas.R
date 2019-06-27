library(dplyr)
library(ggplot2)
library(forcats)

findread <- function(d = NULL,                  # is dataframe provided?
                     dirc = "data/clean_agenda/",
                     pattern = "*txt",
                     collapse = TRUE){          # collate lines from readLines
    if (is.null("d")){
        d <- data.frame(docidx = character(),
                        text = character(),
                        docname = character(),
                        docdate = Date())
    }
    l <- list.files(path = dirc, pattern = pattern)
    for(k in l){
        txtfile <- file(paste0(dirc,k), "r")
        t <- readLines(txtfile)
        if(collapse){
            t <- paste0(unlist(t, recursive = FALSE), collapse = " ")
        }
        idx <- as.character(match(k,l))
        capsate <- gsub("([0-9]{2})([0-9]{2})([0-9]{2}).txt",
                           "\\3-\\1-\\2",
                           k,
                           perl = TRUE)
        capsate <- as.Date(capsate, format = "%y-%m-%d")
        d <- rbind(d, data.frame(docidx = idx,
                                 text = t,
                                 docname = k,
                                 docdate = capsate),
                   stringsAsFactors = FALSE)
        close(txtfile)
    }
    d
}

caps <- findread(dirc = "data/captions/", collapse = FALSE)
caps$text <- as.character(caps$text)
caps$docname <- as.character(caps$docname)

# read in extended captions (2015-2019) from csv
caps15 <- read.csv("data/captions/captions_2015-2019.csv", stringsAsFactors = FALSE)
caps15$text <- tolower(caps15$text)
caps15$date <- as.Date(caps15$meeting_date, format = "%d-%B-%y")
caps15$docname <- as.factor(caps15$meeting_date)

# read in alderpeople's names
infile <- file("alderpeople_dictionary", "r")
ald_names <- readLines(infile)
ald_names <- tolower(ald_names)
close(infile)

# use words instead of 2grams
tidycaps15 <- caps15 %>%
    unnest_tokens(token = "words", input = text, output = words, to_lower = TRUE)

# count occurences of aldermen's names
ald_only <- tidycaps15[tidycaps15$words %in% ald_names,]
ald_counts <- as.data.frame(table(ald_only$words, ald_only$date))

p <- ggplot(ald_counts, aes(x = Var2, y = Freq, group = Var1)) +
    geom_line(col = "grey40") +
    labs(title = "Participation in City Council meetings",
         x = "Date",
         y = "Occurrences of A.'s name") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
p
ggsave(filename = "participation_absolute.png", plot = p,
       width = 20, height = 11, units = "cm", dpi = 300)

ald_split_date <- split(ald_counts, ald_counts$Var2)
ald_counts$total_mentions <- unsplit(sapply(ald_split_date,
                                            function(x) sum(x$Freq)),
                                     ald_counts$Var2)
ald_counts$relfreq <- ald_counts$Freq/ald_counts$total_mentions

ald_split_person <- split(ald_counts, ald_counts$Var1)
ald_counts$avg_mentions <- unsplit(sapply(ald_split_person,
                                          function(x) mean(x$relfreq)),
                                   ald_counts$Var1)
ald_counts <- ald_counts[order(ald_counts$Var1),]
dups <- duplicated(ald_counts$Var1)
ald_ranks <- ald_counts[!dups, c("Var1", "avg_mentions")]
ald_ranks$freqrank <- rank(-ald_ranks$avg_mentions, ties.method = "first")
ald_counts <- merge(ald_counts, ald_ranks, by = "Var1")
ald_counts$freqrank[ald_counts$freqrank > 3] <- NA
ald_counts$freqrank <- as.factor(ald_counts$freqrank)

p <- ggplot(ald_counts, aes(x = Var2, y = relfreq, group = Var1, col = freqrank)) +
    geom_line() +
    labs(title = "Participation in City Council meetings",
         x = "Date",
         y = "Share of mentions") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    guides(col = FALSE)
p
ggsave(filename = "participation_relative.png", plot = p,
       width = 20, height = 11, units = "cm", dpi = 300)

#
#   And now, community areas
#

# read in community areas titles
infile <- file("community_area_dictionary", "r")
com <- readLines(infile)
com <- tolower(com)
close(infile)

# use bigrams instead of words
tidycaps15 <- caps15 %>%
    unnest_tokens(token = "ngrams", n = 2,
                  input = text, output = bigrams,
                  to_lower = TRUE)

# count occurences of aldermen's names
com_only <- tidycaps15[tidycaps15$bigrams %in% com,]
com_counts <- as.data.frame(table(com_only$bigrams, com_only$date))

com_split_date <- split(com_counts, com_counts$Var2)
com_counts$total_session_mentions <- unsplit(sapply(com_split_date,
                                                    function(x) sum(x$Freq)),
                                             com_counts$Var2)
com_counts$relfreq <- com_counts$Freq/com_counts$total_session_mentions

com_split_com <- split(com_counts, com_counts$Var1)
com_counts$avg_mentions <- unsplit(sapply(com_split_com,
                                          function(x) mean(x$relfreq)),
                                   com_counts$Var1)
com_counts$total_com_mentions <- unsplit(sapply(com_split_com,
                                                function(x) sum(x$Freq)),
                                         com_counts$Var1)



dups <- duplicated(com_counts$Var1)
com_counts$Var1 <- fct_reorder(com_counts$Var1, -com_counts$total_com_mentions)

p <- ggplot(com_counts[!dups,], aes(x = Var1, y = total_com_mentions)) +
    geom_bar(stat = "identity") +
    labs(title = "Community Areas mentioned in City Council meetings",
         x = "Community Area",
         y = "Total mentions") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p

ggsave(filename = "com_areas_total_mentions.png", plot = p,
       width = 20, height = 11, units = "cm", dpi = 300)
