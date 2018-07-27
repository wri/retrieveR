library(magrittr)

folder <- 'background-texts'
folders <- list.files(path=folder, pattern=".txt")
full_folders <- paste(folder, folders, sep="/")
added_words <- c("rangelands", "ers", "sra", "reverse", "operationalize", "operationalized", "pastoralism", "hydropower", "landuse", "smallscale", "largescale", "percent",
                 "underexploited", "agroforestry", "siltation", "intergovernmental", "subsector", "subsectors", "rainfall", "rangeland", "agribusiness", "flr", "landscape", "cop", "agroforest", "kenya", "malawi", "rwanda", "programmes", "programme", "fuelwood", "ngos", "kenyas", "kigali", "slm", "africa", "ghg", "sectoral", "kenyan", "malawis", "african", "longterm", "woodfuels", "timeframe", "nairobi", "fao","sdg", "sdgs", "agro", "npv", "rainfed", "streambank", "cookstoves", "transboundary", "anthropogenic", "manmade", "geospatial", "subcomponent", "nontimber", "agroprocessing", "multicriteria", "crossectoral", "microfinance", "evapotranspiration", "silviculture", "nonforested", "eutrophication", "sociocultural", "gdp", "usd", "hiv", "womens", "un", "uk", "eu")

count_dig2 <- function(s, data) {
    s2 <- gsub("[0-9]","", data[s])
    perc <- round(((1-(nchar(s2)/nchar(data[s])))*100),1)
    return(perc)
}

create_df <- function(ind) {
  folder <- full_folders[ind]
  country <- folders[ind] # list of files to read in
  files <- list.files(path=folder, pattern=".txt")
  dir <- paste(folder, files, sep="")
  #cat(dir, "\n")
  
  # readtext reads in all the files in the above list 
  readtext <- function(file){
    cat("Reading ", file, "\n")
    .con <- file(description=file)
    .text <- readLines(file)
    close(.con)
    names(.text) <- file
    #text <- paste(.text, sep="", collapse="")
    return(.text)
  }

  # convert_pagraphs removes whitespace (find.dup), pastes together paragraphs,
  # pastes together broken sentences, and removes titles, tables, figures, links,
  # and a lot of noise (paragraphs that aren't real sentences)

  convert_paragraphs <- function(input) {
    colname <- as.character(input)
    file <- readtext(input)
    file <- gsub("\\s+", " ", file) # Convert multiple spaces to one space
    for(i in c(1:length(file))) {
      if(file[i] == " ") {
        file[i] <- ""
      }
    }
    
    # find.dup just counts the number of consecutive empty lines
    find.dup <- function(x) {
      consec <- rep(0, length(x))
      for(i in 2:length(x)) {
        if(x[i] == x[i-1]) {
          consec[i] <- consec[i-1] + 1
        } else {
          consec[i] <- 0
        }
      }
      return(consec)
    }
    
    # Group up lines between empty lines (paragraphs) and pastes them together
    length_pre <- length(file)
    rmfigtable <- grepl("^Figure\\s+\\d|^figure\\s+\\d|^Table\\s+\\d|^table\\s+\\d", file)
    file <- file[rmfigtable==F]
    digs <- unlist(lapply(c(1:length(file)), count_dig2, file))
    file <- file[digs < 50]
    length_post <- length(file)
    length_removed <- length_pre-length_post
    #cat("Removed ", length_removed, "lines with tables or only numbers \n")

    na_loc <- file == ""
    l <- which(na_loc == F)
    groups <- as.vector(l)
    y <- sort(groups)
    g <- cumsum(c(1, abs(y[-length(y)] - y[-1]) > 1))
    paragraphs <- by(y, g, identity)
    pgraphs <- rep(NA, length(paragraphs)) # Paste together paragraphs
    for(i in c(1:length(pgraphs))) {
      pgraphs[i] <- paste(file[unlist(paragraphs[i])], collapse = " ")
    }
    
    for(i in c(1:length(pgraphs))) { # Some paragraphs got missed and this fixes that
      if(grepl("   ", pgraphs[i])) {
        pgraphs[i] <- strsplit(unlist(pgraphs[i]), "   ")
      }
    }

    # Convert the resulting paragraphs to a dataframe
    pgraphs <- unlist(pgraphs, recursive=F)
    pgraphs <- pgraphs[pgraphs != ""]
    pgraphs <- na.omit(pgraphs)
    pgraphs <- pgraphs[nchar(pgraphs) > 20]
    pgraphs <- pgraphs[nchar(pgraphs) < 5000]
    pgraphs <- data.frame(sentences = pgraphs)
    pgraphs$name <- rep(colname, nrow(pgraphs))
    pgraphs$sentences <- as.character(pgraphs$sentences)
    pgraphs <- pgraphs[!is.na(pgraphs$sentences),]

    # Remove table of contents, links, page numbers, sentences without
    # words, sentences without lower case words (titles)
    dot <- grepl("[.]{4}|http|…|Page\\s+\\d", pgraphs$sentences)
    pgraphs <- pgraphs[dot == F,]
    lower <- grepl("[a-z]", pgraphs$sentences)
    pgraphs <- pgraphs[lower==T,]
    pgraphs <- pgraphs[duplicated(pgraphs$sentences) == F,]
    pgraphs$sentences <- gsub("[A-Z]{5,}\\s+[A-Z]{5,}", " ", pgraphs$sentences)
    haswords <- grepl("[A-z]{5}|[a-z]{5}", pgraphs$sentences)
    haslower <- grepl("\\s+[a-z]{5,30}\\s+", pgraphs$sentences)
    pgraphs <- pgraphs[haswords ==T,]
    pgraphs <- pgraphs[haslower == T,]
    
    # Paste together lines that do not start with a capital letter
    # under the assumption that such lines are broken sentences
    d <- grepl("^[a-z]|^\\s+[a-z]", pgraphs$sentences)
    d <- which(d == T)
    if(length(d) > 0) {
      y <- sort(d)
      g <- cumsum(c(1, abs(y[-length(y)] - y[-1]) > 1))
      split_sentences <- by(y, g, identity)
      paste_together <- function(ind) {
        ind <- unlist(split_sentences[ind])
        before <- ind[1] - 1
        ind <- append(before, ind)
        ind <- ind[ind >= 1]
        pasted <- paste(pgraphs$sentences[ind], collapse=" ")
        return(pasted)
      }
      d <- seq_along(split_sentences)
      pasted <- lapply(d, paste_together)
      to_na <- as.numeric(unlist(split_sentences))
      begin_pasted <- d
      for(i in d) {
        temp <- unname(unlist(split_sentences[i]))
        begin_pasted[i] <- temp[1] - 1
      }
      begin_pasted[begin_pasted == 0] <- 1
      pgraphs$sentences[to_na] <- NA
      pgraphs$sentences[begin_pasted] <- pasted
      pgraphs <- pgraphs[!is.na(pgraphs$sentences),]
      pgraphs$sentences <- unlist(pgraphs$sentences)
    }
    
    
    # Paste together lines that end with a word without punctuation,
    # or that end with a : or a - under the assumption that such lines are broken sentences

    d <- grepl("[a-z]$|[a-z]\\s+$|,$|,\\s+$|:$|:\\s+$|—$|—\\s+$|;$|;\\s+", pgraphs$sentences)
    d <- which(d == T)
    if(length(d) > 0) {
      y <- sort(d)
      g <- cumsum(c(1, abs(y[-length(y)] - y[-1]) > 1))
      split_sentences <- by(y, g, identity)
      paste_together <- function(ind) {
        ind <- unlist(split_sentences[ind])
        after <- ind[length(ind)] + 1
        if(after <= length(pgraphs$sentences)) {
          ind <- append(ind, after)
        }
        pasted <- paste(pgraphs$sentences[ind], collapse=" ")
        return(pasted)
      }
      d <- seq_along(length(split_sentences))
      pasted <- lapply(d, paste_together)
      to_na <- as.numeric(unlist(split_sentences))
      end_pasted <- rep(NA, length(d))
      for(i in d) {
        temp <- unname(unlist(split_sentences[i]))
        end_pasted[i] <- temp[length(temp)] + 1
      }
      end_pasted <- end_pasted[end_pasted <= length(pgraphs$sentences)]
      pgraphs$sentences[to_na] <- NA
      pgraphs$sentences[end_pasted] <- pasted
      pgraphs <- pgraphs[!is.na(pgraphs$sentences),]
      pgraphs$sentences <- unlist(pgraphs$sentences)
    }
    return(pgraphs)
  }

  # apply the convert_paragraphs function to every file in the directory
  # and return a dataframe indexed by country and name
  f1 <- lapply(dir, convert_paragraphs)
  f1 <- do.call("rbind", f1)
  f1$country <- rep(folders[ind], nrow(f1))
  f1$sentences <- as.character(f1$sentences)
  return(f1)
}

clean_data <- function(data) {
  toremove <- list("<(.|\n)*?>", "\\\\", "\n", "\t", "[^\x20-\x7E]",
                   "\\)", "\\(", "[A-Z]{4,}", "", "\\/", "\\[[^\\]]*\\]", "page", "Page", "([\\])")
  pb <- txtProgressBar(min = 0, max=length(toremove), style=3)
  gsub.mult <- function(n) {
    setTxtProgressBar(pb, n)
    data$text <<- gsub(toremove[n], "", data$text)
  }
  res <- lapply(c(1:length(toremove)), gsub.mult)[[length(toremove)]]
  close(pb)
  return(res)
}

rm_multi_space <- function(x, data) {
  y <- gsub("\\s+", " ", data[x,1])
  return(y)
}

parse_sentences <- function(n, data) {
  sentences <- as.character(data[n, 1])
  sentences <- unlist(strsplit(sentences, split="\\."))
  sentences <- sentences[sentences != ""]
  sentences <- sentences[nchar(sentences) > 55]
  sentences <- sentences[nchar(sentences) < 2500]
  sentences <- paste(sentences, ".", sep="")
  return(sentences)
}

detect_language <- function(n) {
  lang <- cld2::detect_language(as.character(test[n, 1]))
  return(lang)
}

check_bad <- function(id, in_dict) {
  setTxtProgressBar(pb, id)
  sentence <- test2$sentences[id]
  sentence <- tolower(sentence)
  bad_words <- hunspell::hunspell_find(sentence, ignore = added_words, dict = hunspell::dictionary(in_dict))
  bad_words <- unlist(bad_words)
  #for(i in seq_along(bad_words)) {
  #  bad_words[i] <- paste("\\s+", bad_words[i], "\\s+", sep="")
  #}
  return(bad_words)
}

check_spelling <- function(id) {
  setTxtProgressBar(pb, id)
  sentence <- test2$sentences[id]
  sentence <- tolower(sentence)
  bad_words <- hunspell::hunspell_find(sentence, ignore = added_words)
  bad_words <- unlist(bad_words)
  if(length(bad_words) > 0) {
    suggested <- hunspell::hunspell_suggest(bad_words)
    for(i in seq_along(bad_words)) {
      bad_words[i] <- paste("\\s+", bad_words[i], "\\s+", sep="")
    }
    #cat("Suggested: ", unlist(suggested), "\n")
    replacements <- rep(NA, length(suggested))
    for(i in c(1:length(suggested))) {
      replacements[i] <- suggested[[i]][1]
      replacements[i] <- paste(" ", replacements[i], " ", sep="")
    }
    for(i in c(1:length(bad_words))) {
      if(replacements[i] != " NA ") {
        sentence <- gsub(bad_words[i], replacements[i], sentence)
      }
    }
    return(sentence)
  } else {
    return(sentence)
  }
}

total_data <- lapply(seq_along(full_folders), create_df)
total_data <- do.call("rbind", total_data)
cat("Data loaded", "\n")
total_data$text <- unlist(lapply(c(1:nrow(total_data)), rm_multi_space, total_data))
cat("Extra space removed", "\n")

total_data$text <- clean_data(total_data)
total_data$text <- gsub("(\\d+)(\\w)", "\\1 \\2", total_data$text)

cat("Text cleaned", "\n")

#test <- lapply(c(1:nrow(total_data)), parse_sentences, total_data)
#est <- unlist(test)
test <- data.frame(sentences = total_data$text)
cat("There are ", nrow(test), " documents", "\n")
test$language <- rep(NA, nrow(test))
test <- test[!is.na(test$sentences),]
test <- test[!duplicated(test$sentences),]


test$languages <- unlist(lapply(c(1:nrow(test)), detect_language))
test <- test[test$languages == "en",]
test <- test[!duplicated(test$sentences),]
cat("Text subsetted to english", "\n")
test2 <- test
test2$sentences <- as.character(test2$sentences)
bf <- nrow(test2)
test2 <- test2[nchar(as.character(test2$sentences)) > 150,]
test2 <- test2[nchar(as.character(test2$sentences)) < 10000,]
af <- bf - nrow(test2)
cat("Removed ", af, "documents for length issues", "\n")

cat("Beginning spelling correction, this may take awhile!", "\n")
pb <- txtProgressBar(min = 0, max=nrow(test2), style=3)
bad_words_en <- unlist(lapply(1:nrow(test2), check_bad, "en_US"))
close(pb)
pb <- txtProgressBar(min = 0, max=nrow(test2), style=3)
bad_words_gb <- unlist(lapply(1:nrow(test2), check_bad, "en_GB"))
close(pb)


bad_words_en <- data.frame(bad_words_en)
bad_words_en <- bad_words_en %>%
  dplyr::group_by(bad_words_en) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::mutate(bad_words_en = as.character(bad_words_en)) %>%
  dplyr::filter(nchar(bad_words_en) > 2)
  #filter(n >= 10)

bad_words_gb <- data.frame(bad_words_gb)
bad_words_gb <- bad_words_gb %>%
  dplyr::group_by(bad_words_gb) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::mutate(bad_words_gb = as.character(bad_words_gb)) %>%
  dplyr::filter(nchar(bad_words_gb) > 2)

british_words <- bad_words_en$bad_words_en[!bad_words_en$bad_words_en %in% bad_words_gb$bad_words_gb]
added_words <- append(added_words, british_words)

common_badwords <- bad_words_en$bad_words_en[bad_words_en$n > 15]
added_words <- append(added_words, common_badwords)
added_words <- unique(added_words)



cat("\nCorrecting spelling errors", "\n")
pb <- txtProgressBar(min = 0, max=nrow(test2), style=3)
corrected <- unlist(lapply(1:nrow(test2), check_spelling))
close(pb)
cat("\n")
test2$sentences <- corrected

res <- lapply(c(1:nrow(test2)), function(x) tolower(test2$sentences[x]))
test2$sentences <- res
test2$sentences <-gsub(":", "", test2$sentences)


#test2 <- increase_chunk(test2)
test2$sentences <- as.character(test2$sentences)
test2 <- test2[!is.na(test2$sentences),]
test2 <- test2[!duplicated(test2$sentences),]
test2 <- as.data.frame(test2)
colnames(test2) <- "sentences"
test2 <- test2[,1]
#cat("Text chunked to 7 sentences", "\n")

calcbigram <- function(input) {
  corpus <- tm::Corpus(tm::VectorSource(input))
  corpus <- tm::tm_map(corpus, removePunctuation)
  corpus <- tm::tm_map(corpus, removeNumbers)
  ngrams <- tm::term_stats(corpus, ngrams=2:4)
  ngrams <- ngrams %>%
    dplyr::arrange(desc(count))
  ngrams <- ngrams[1:25000,]
  return(ngrams)
}

bgrams <- calcbigram(test2)
bgrams$word <- stringr::str_count(bgrams$term, " ")
bgrams <- bgrams %>%
  group_by(term) %>%
  arrange(desc(word))

bgrams <- bgrams[-which(grepl("^any |^these |^than |^through |^so |^its |^they |^from |^if |^or |^but |^an |^can |^with |^will |^which |^at |^there |^by |^that |^this |^their |^the |^a |^in |^to |^as |^of |^it |^and |^ are |^is |^be |^has |^have |^do |^on |^are |^for ", bgrams$term)),]
bgrams <- bgrams[-which(grepl(" any$| these$| than$| through$| so$| its$| they$| from$| if$| or$| but$| an$| can$| with$| will$| which$| that$| this$| their$| the$| a$| in$| to$| as$| of$| it$| and$| are$| is$| be$| has$| have$| do$| on$| are$| for$", bgrams$term)),]

head(bgrams$term)

bgrams_bef <- paste0("\\s+", bgrams$term, "\\s+")

bgrams_und <- gsub(" ", "_", bgrams$term)
bgrams_und <- paste0(" ", bgrams_und, " ")

bundled <- test2

for(i in seq_along(bgrams_bef)) {
  if(i %% 100 == 0) {
    cat("Completed", i, "documents \n")
  }
  bundled <<- gsub(bgrams_bef[i], bgrams_und[i], bundled)
}

#write.csv("bgrams.csv", bgrams)

test2 <- bundled
test2 <- gsub("[!.,;:%\\$]", "", test2)
test2 <- gsub('"\\/|\\\\', "", test2)
test2 <- gsub("[0-9]", "", test2)
test2 <- gsub("([A-z])-\\s+([a-z])", "\\1\\2", test2)
test2 <- gsub("([A-z])\\s+-\\([a-z])", "\\1\\2", test2)
test2 <- gsub('\"', "", test2)
test2 <- gsub("'", "", test2)
test2 <- gsub("-", "", test2)

#corpus <- Corpus(VectorSource(bundled))
#test <- tm_map(corpus, content_transformer(tolower)) 
#test <- tm_map(test, removeWords, stopwords("english")) 
#test <- gsub('[!.,;;"\\(\\)%&\\+]', "", test)
#test <- gsub('-', "", test)
#test <- gsub("[0-9]", "", test)
#test <- tm_map(test, removeNumbers) 
#test <- tm_map(test, stripWhitespace) 



# Word count per document
#corpus <- corpus(test)
#dfm <- dfm(corpus)
#words <- dfm@Dimnames$features
#counts <- unname(colSums(dfm))
#wcounts <- data.frame(words = words, counts = counts)
#toremove <- wcounts$words[wcounts$counts < 5]
#toremove <- as.character(toremove)
#toremove <- paste0(" ", toremove, " ")

#corpus <- Corpus(VectorSource(bundled))
#test5 <- tm_map(corpus, content_transformer(removeWords), 
#                 toremove)

write.csv(test2, "full_lsa_corpus.csv")
write.table(test2, "full_lsa_text.txt", row.names=F, col.names=F)

sepfiles <- function(ind, data) {
  item <- data[ind]
  write.table(item, paste0("background-chunks/", ind, ".txt"), row.names=F, col.names=F)
}

lapply(c(1:length(test2)), sepfiles, test2)
