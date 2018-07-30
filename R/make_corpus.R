#' Creates a corpus  that can be used with create_locations
#' @param folder Path to files
#' @keywords creation
#' @export
#' @examples
#' make_corpus()

make_corpus <- function(folder) {
  library(magrittr)

  added_words <- c("rangelands", "ers", "sra", "reverse", "operationalize", "operationalized", "pastoralism", "hydropower", "landuse", "smallscale", "largescale", "percent",
                   "underexploited", "agroforestry", "siltation", "intergovernmental", "subsector", "subsectors", "rainfall", "rangeland", "agribusiness", "flr", "landscape", "cop", "agroforest", "kenya", "malawi", "rwanda", "programmes", "programme", "fuelwood", "ngos", "kenyas", "kigali", "slm", "africa", "ghg", "sectoral", "kenyan", "malawis", "african", "longterm", "woodfuels", "timeframe", "nairobi", "fao","sdg", "sdgs", "agro", "npv", "rainfed", "streambank", "cookstoves", "transboundary", "anthropogenic", "manmade", "geospatial", "subcomponent", "nontimber", "agroprocessing", "multicriteria", "crossectoral", "microfinance", "evapotranspiration", "silviculture", "nonforested", "eutrophication", "sociocultural", "gdp", "usd", "hiv", "womens", "un", "uk", "eu")
  folders <- list.files(path=folder)
  subfolders <- paste0(folder, "/", folders)

  allfiles <- list()
  for(i in c(1:length(subfolders))) {
    allfiles[[i]] <- paste0(subfolders[i], "/", list.files(subfolders[i]), "/")
  }
  subfolders <- unlist(allfiles)

  count_dig <- function(s) {
      s2 <- gsub("[0-9]","", test2$sentences[s])
      perc <- round(((1-(nchar(s2)/nchar(test2$sentences[s])))*100),1)
      return(perc)
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

  total_data <- create_df(subfolders, main=T)
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


  test2 <- test2[nchar(test2$sentences) > 50,]
  perc <- unlist(lapply(c(1:nrow(test2)), count_dig))
  test2 <- test2[perc < 10,]

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

  # Write cleaned-up dataframe to a CSV for further analysis
  write.csv(test2, "full_corpus.csv")
  return(test2)
  cat("Wrote the corpus as full_corpus.csv")
}