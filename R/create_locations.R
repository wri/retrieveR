suppressMessages(library(stm))
library(hunspell)
library(wordVectors)
library(pbapply)
suppressMessages(library(tm))
suppressMessages(library(tidyverse))
library(corpus)
library(rmarkdown)

cat("\nReading in data", "\n")

data <- read.csv("../ocr/full_corpus.csv")
data$sentences <- as.character(data$sentences)
data <- data[nchar(data$sentences) > 70,]

wv1 <- wordVectors::read.binary.vectors("../word2vec/wordvec_ndcs_new.bin")
df1 <- data.frame(wv1@.Data)

data$sentences <- gsub("([A-z])-([A-z])", "\\1 \\2", data$sentences)
sum(grepl("([A-z])-\\s+([a-z])", data$sentences))
data$sentences <- gsub("([A-z])-\\s+([a-z])", "\\1\\2", data$sentences)
data$sentences <- gsub("([A-z])\\s+-([a-z])", "\\1\\2", data$sentences)
data$sentences <- gsub("-", " ", data$sentences)

cat("Bundling n-grams", "\n")

bigrams <- rownames(df1)[grepl("_", rownames(df1))]
bigrams <- bigrams[!(grepl("[0-9]", bigrams))]
to_bundle <- gsub("_", " ", bigrams)

for(i in seq_along(to_bundle)) {
  to_bundle[i] <- paste("\\s+", to_bundle[i], "\\s+", sep="")
  bigrams[i] <- paste(" ", bigrams[i], " ", sep="")
}

bundled <- data$sentences

pb <- txtProgressBar(min = 0, max = length(to_bundle), style = 3)
for(i in seq_along(to_bundle)) {
  setTxtProgressBar(pb, i)
  bundled <<- gsub(to_bundle[i], bigrams[i], bundled)
}
close(pb)
data$sentences <- bundled

cat("Calculating idf", "\n")
nopunc <- gsub("[!.,%0-9:;]", "", data$sentences)
nopunc <- paste(nopunc, collapse=" ")
nopunc <- strsplit(nopunc, split=" ")
nopunc <- do.call("rbind", nopunc)
list_of_words <- as.data.frame(t(nopunc)) %>%
  group_by(V1) %>%
  summarise(n=n())


cat("Calculating paragraph-level neural embeddings \n")

pb <- txtProgressBar(min = 0, max = nrow(data), style = 3)
create_point <- function(id) {
  setTimerProgressBar(pb, id)
  query <- data$sentences[id]
  query <- unlist(strsplit(query, " "))
  query <- query[!query %in% stopwords_en]
  query <- gsub("[0-9]", "", query)
  query <- gsub("[,.!?:;$%]", "", query)
  query <- query[!query == ""]
  query <- data.frame(query = as.character(query)) %>%
    group_by(query) %>%
    summarise(n=n()) %>%
    mutate(query = as.character(query))
  query <- query[query$query %in% rownames(df1),]
  norm_vec <- function(x) sqrt(sum(x^2))
  #vecs <- query$n / norm_vec(query$n)
  embeds <- df1[rownames(df1) %in% query$query,]
  embeds <- embeds[order(rownames(embeds)),]
  
  tf <- list_of_words[list_of_words$V1 %in% rownames(embeds),]
  idf <- log(nrow(data)/(tf$n))
  print(idf)
  vecs <- (idf*query$n)/norm_vec(idf*query$n)
  #print(vecs)

  sentence_embedding <- t(as.matrix(vecs)) %*% as.matrix(embeds)
  #print(unname(unlist(wv1 %>% closest_to(sentence_embedding, n = 10) %>% select(word))))
  #vecs <- query$n / norm_vec(query$n)
  #sentence_embedding <- t(as.matrix(vecs)) %*% as.matrix(embeds)
  #cat("\n")
  #print(unname(unlist(wv1 %>% closest_to(sentence_embedding, n = 10) %>% select(word))))
  return(sentence_embedding)
}

locations <- lapply(c(1:nrow(data)), create_point)
close(pb)

cat("Saving the word embeddings to 'embeddings.rds'\n")
saveRDS(locations, "embeddings.rds")
cat("Saving the word embeddings to 'embeddings.rds'\n")
saveRDS(locations, "embeddings.rds")


