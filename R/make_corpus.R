library(magrittr)

added_words <- c("rangelands", "ers", "sra", "reverse", "operationalize", "operationalized", "pastoralism", "hydropower", "landuse", "smallscale", "largescale", "percent",
                 "underexploited", "agroforestry", "siltation", "intergovernmental", "subsector", "subsectors", "rainfall", "rangeland", "agribusiness", "flr", "landscape", "cop", "agroforest", "kenya", "malawi", "rwanda", "programmes", "programme", "fuelwood", "ngos", "kenyas", "kigali", "slm", "africa", "ghg", "sectoral", "kenyan", "malawis", "african", "longterm", "woodfuels", "timeframe", "nairobi", "fao","sdg", "sdgs", "agro", "npv", "rainfed", "streambank", "cookstoves", "transboundary", "anthropogenic", "manmade", "geospatial", "subcomponent", "nontimber", "agroprocessing", "multicriteria", "crossectoral", "microfinance", "evapotranspiration", "silviculture", "nonforested", "eutrophication", "sociocultural", "gdp", "usd", "hiv", "womens", "un", "uk", "eu")


folders <- list.files(path='results/')
subfolders <- paste0('results/', folders)

allfiles <- list()
for(i in c(1:length(subfolders))) {
  allfiles[[i]] <- paste0(subfolders[i], "/", list.files(subfolders[i]))
}
subfolders <- unlist(allfiles)


print(folders)
print(subfolders)

count_dig <- function(s) {
    s2 <- gsub("[0-9]","", test2$sentences[s])
    perc <- round(((1-(nchar(s2)/nchar(test2$sentences[s])))*100),1)
    return(perc)
}

count_dig2 <- function(s, data) {
    s2 <- gsub("[0-9]","", data[s])
    perc <- round(((1-(nchar(s2)/nchar(data[s])))*100),1)
    return(perc)
}


create_df <- function(ind) {
  folder <- paste0(subfolders[ind], "/")
  print(folder)
  #country <- folders[ind] # list of files to read in
  files <- list.files(path=folder, pattern=".txt")
  print(files)
  dir <- paste(folder, files, sep="")  
  # readtext reads in all the files in the above list 
  readtext <- function(file){
    cat("Reading", file, "\n")
    .con <- file(description=file)
    .text <- readLines(file)
    close(.con)
    names(.text) <- file
    if(sum(nchar(.text)) < 10) {
      .text <- "null"
    }
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
      if(length(x) > 2) {
        for(i in 2:length(x)) {
          if(x[i] == x[i-1]) {
            consec[i] <- consec[i-1] + 1
          } else {
            consec[i] <- 0
          }
        }
        return(consec)
      }
    }
    
    # Remove all empty lines preceded by at least 4 empty lines
    breaks <- find.dup(file) # Create a list of sequential empty lines
    to.rm <- rep(FALSE, length(breaks))
    for(i in c(1:length(breaks))) {
      if(length(breaks) > 2) {
        if(breaks[i] > 4) {
        to.rm[i] <- TRUE
      }
      }
    }
    file <- file[!to.rm]
    length_pre <- length(file)
    rmfigtable <- grepl("^Figure\\s+\\d|^figure\\s+\\d|^Table\\s+\\d|^table\\s+\\d", file)
    file <- file[rmfigtable==F]
    digs <- unlist(lapply(c(1:length(file)), count_dig2, file))
    file <- file[digs < 80]
    length_post <- length(file)
    length_removed <- length_pre-length_post

    # Group up lines between empty lines (paragraphs) and pastes them together
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
    pgraphs <- data.frame(sentences = pgraphs)
    pgraphs$name <- rep(colname, nrow(pgraphs))
    pgraphs$sentences <- as.character(pgraphs$sentences)
    pgraphs <- pgraphs[!is.na(pgraphs$sentences),]

    # Remove table of contents, links, page numbers, sentences without
    # words, sentences without lower case words (titles)
    dot <- grepl("[.]{4}|http|…|Page\\s+\\d", pgraphs$sentences)
    pgraphs <- pgraphs[dot == F,]
    #lower <- grepl("[a-z]", pgraphs$sentences)
    #pgraphs <- pgraphs[lower==T,]
    #pgraphs <- pgraphs[duplicated(pgraphs$sentences) == F,]
    pgraphs$sentences <- gsub("[A-Z]{5,}\\s+[A-Z]{5,}", " ", pgraphs$sentences)
    #haswords <- grepl("[A-z]{4}|[a-z]{4}", pgraphs$sentences)
    #haslower <- grepl("\\s+[a-z]{5,30}\\s+", pgraphs$sentences)
    #pgraphs <- pgraphs[haswords ==T,]
    #pgraphs <- pgraphs[haslower == T,]
    
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
  wrapper_function <- function(i) {
    data <- tryCatch(convert_paragraphs(dir[i]), error=function(e) NULL)
    return(data)
  }
  f1 <- lapply(seq_along(dir), wrapper_function)
  f1 <- do.call("rbind", f1)
  f1$country <- rep(folders[ind], nrow(f1))
  f1$sentences <- as.character(f1$sentences)
  pagenames <- stringr::str_match(f1$name, "[0-9]{1,}[.]txt")
  f1$page <- gsub("[.]txt", "", pagenames)
  return(f1)
}

# Remove HTML, large all-caps words, page numbers, unicode characters

clean_data <- function(index, data) {
  toremove <- list("<(.|\n)*?>", "\\\\", "\n", "\t", "[^\x20-\x7E]",
                   "\\)", "\\(", "[A-Z]{6,}", "", "\\/", "\\[[^\\]]*\\]", "page", "Page", "([\\])")
  gsub.mult <- function(n, index) {
    setTxtProgressBar(pb, index)
    data[index,1] <<- gsub(toremove[n], "", data[index,1])
  }
  res <- lapply(c(1:length(toremove)), gsub.mult, index)[[length(toremove)]]
  return(res)
}

# Remove large sections of blank spaces
rm_multi_space <- function(x, data) {
  y <- gsub("\\s+", " ", data[x,1])
  return(y)
}

# Use the cld2 package to determine whether paragraphs are english
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
  return(bad_words)
}

# Use the hunspell package to automatically fix mispelled words
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

# This function is used to identify any titles that are left in after
# the convert_paragraphs function, under the assumption that phrases of
# 6-12 words long that are repeated often within a document are likely 
# the title of the document. 

calcbigram <- function(input) {
  corpus <- tm::Corpus(tm::VectorSource(input))
  ngrams <- corpus::term_stats(corpus, ngrams=6:12)
  ngrams <- ngrams %>%
    dplyr::arrange(desc(count))
  ngrams <- ngrams[1:3000,]
  return(ngrams)
}

remove_names <- function() {
  bigrams <- calcbigram(test2$sentences)
  toremove <- bigrams$term[1:100]
  
  if(length(which(grepl("\\[|\\+", toremove))) > 0) {
    print(which(grepl("\\[|\\+", toremove)))
    toremove <- toremove[-which(grepl("\\[|\\+", toremove))]
  }
  
  res <- test2$sentences
  for(i in seq_along(toremove)) {
    remove <- gsub(" ", "\\\\s+", toremove[i])
    print(sum(grepl(remove, res)))
    res <- gsub(unlist(remove), "", res)
  }
  return(res)
}

total_data <- lapply(c(1:length(subfolders)), create_df)
total_data <- do.call("rbind", total_data)
total_data <- total_data[nchar(total_data$sentences) > 70,]
total_data <- total_data[!is.na(total_data$sentences),]
cat("\nData loaded", "\n")

# Remove extra spaces
total_data$sentences <- unlist(lapply(c(1:nrow(total_data)), rm_multi_space, total_data))
cat("Extra space removed", "\n")
total_data$sentences <- gsub("(\\d+)(\\w)", "\\1 \\2", total_data$sentences)

# Clean data
cat("Beginning data cleaning", "\n")
pb <- txtProgressBar(min = 0, max=nrow(total_data), style=3)
total_data$sentences <- unlist(lapply(c(1:nrow(total_data)), clean_data, total_data))
close(pb)
test <- total_data
cat("Text cleaned", "\n")

# Subset to english
test$languages <- rep(NA, nrow(test))
test <- test[!is.na(test$sentences),]
test <- test[!duplicated(test$sentences),]
test$languages <- unlist(lapply(c(1:nrow(test)), detect_language))
test <- test[test$languages == "en",]
test <- test[!duplicated(test$sentences),]
cat("Text subsetted to english", "\n")
test2 <- test

test2$sentences <- gsub("([A-z])-\\s+([a-z])", "\\1\\2", test2$sentences)
test2$sentences <- gsub("([A-z])\\s+-([a-z])", "\\1\\2", test2$sentences)

# Fix spelling errors
cat("Beginning spelling correction, this may take awhile!", "\n")
cat("\nCreating dictionary", "\n")
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

print(added_words)

#write.table(bad_words, "bad_words.csv")
cat("\nCorrecting spelling errors", "\n")
pb <- txtProgressBar(min = 0, max=nrow(test2), style=3)
corrected <- unlist(lapply(1:nrow(test2), check_spelling))
close(pb)
cat("\n")
test2$sentences <- corrected
test2$sentences <- enc2utf8(test2$sentences)
test2$sentences <- unlist(lapply(c(1:nrow(test2)), function(x) tolower(test2$sentences[x])))
test2$sentences <-gsub(":", "", test2$sentences)
test2$sentences <- as.character(test2$sentences)
test2 <- test2[!is.na(test2$sentences),]
test2 <- test2[!duplicated(test2$sentences),]

# Remove titles
#test2$sentences <- remove_names()
#test2$sentences <- remove_names()
test2 <- test2[nchar(test2$sentences) > 50,]

perc <- unlist(lapply(c(1:nrow(test2)), count_dig))
test2 <- test2[perc < 10,]
#test2 <- test2[-grep("\\d+$", test2$sentences),]
#test2 <- test2[-grep("\\d+\\s+$", test2$sentences),]
citation <- grepl("\\.,", test2$sentences)
eg <- grepl("e\\.g\\.", test2$sentences)
etal <- grepl("et al\\.,", test2$sentences)
etal2 <- grepl("et\\. al\\.,", test2$sentences)
citation[eg == T] <- F
citation[etal == T] <- F
citation[etal2 == T] <- F

if(sum(citation == T) > 0) {
  test2 <- test2[-which(citation == T),]
  cat("removed", sum(citation), "files")
}

citation2 <- grepl("\\&", test2$sentences) & grepl("\\s+[a-z]{1}[.]", test2$sentences) 

if(sum(citation2 == T) > 0) {
  test2 <- test2[-which(citation2 == T),]
}

test2 <- test2[-grepl(",\\s+[a-z]{1}[.]", test2$sentences),]

test2$sentences <- gsub("\\s+[bcdefghijklmnopqrstuvwxyz]{1}\\s+", "", test2$sentences)
#test2 <- test2[-grep("[%]\\s+?", test2$sentences),]
#test2 <- test2[nchar(test2$sentences) > 50,]
#inparenthesis <- grepl("^\\(\\s?", test2$sentences) & grepl("\\)\\s?$", test2$sentences)
#print(sum(inparenthesis))

# Write cleaned-up dataframe to a CSV for further analysis
write.csv(test2, "full_corpus.csv")
cat("Wrote the corpus as full_corpus.csv")
