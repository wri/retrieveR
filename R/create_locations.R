#' Create neural embedding weights for each paragraph in a corpus
#' @param corpus Dataframe returned from make_corpus 
#' @param embedding Path to neural embedding (.bin)
#' @keywords creation
#' @export
#' @examples
#' create_locations()

create_locations <- function(corpus, embedding = "embeddings.bin") {
  library(magrittr)
  cat("\nReading in data", "\n")
  data <- corpus
  data$sentences <- as.character(data$sentences)
  #data <- data[nchar(data$sentences) > 70,]
  wv1 <- wordVectors::read.binary.vectors(embedding)
  df1 <- data.frame(wv1@.Data)

  data$sentences <- gsub("([A-z])-([A-z])", "\\1 \\2", data$sentences)
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
  nopunc <- do.call("rbind", strsplit(paste(nopunc, collapse=" "), split = " "))
  list_of_words <- as.data.frame(t(nopunc)) %>%
    dplyr::group_by(V1) %>%
    dplyr::summarise(n=n())

  head(list_of_words)

  cat("Calculating paragraph-level neural embeddings \n")
  pb <- txtProgressBar(min = 0, max = nrow(data), style = 3)
  create_point <- function(id) {
    setTxtProgressBar(pb, id)
    query <- data$sentences[id]
    query <- unlist(strsplit(query, " "))
    query <- query[!query %in% corpus::stopwords_en]
    query <- gsub("[0-9]", "", query)
    query <- gsub("|[,.!?:;$%]", "", query)
    query <- query[!query == ""]
    query <- data.frame(query = as.character(query)) %>%
      dplyr::group_by(query) %>%
      dplyr::summarise(n=n()) %>%
      dplyr::mutate(query = as.character(query))
    query <- query[query$query %in% rownames(df1),]
    norm_vec <- function(x) sqrt(sum(x^2))
    embeds <- df1[rownames(df1) %in% query$query,]
    embeds <- embeds[order(rownames(embeds)),]
    
    tf <- list_of_words[list_of_words$V1 %in% rownames(embeds),]
    idf <- log(nrow(data)/(tf$n))
    vecs <- (idf*query$n)/norm_vec(idf*query$n)
    sentence_embedding <- t(as.matrix(vecs)) %*% as.matrix(embeds)
    return(sentence_embedding)
  }
  locations <- lapply(c(1:nrow(data)), create_point)
  close(pb)

  cat("Saving the word embeddings to 'embeddings.rds'\n")
  saveRDS(locations, "embeddings.rds")
  cat("There are now", nrow(data), "observations!\n")
}