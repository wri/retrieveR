#' Creates a corpus  that can be used with create_locations
#' @param folder Path to files
#' @keywords creation
#' @export
#' @examples
#' create_corpus()

create_corpus <- function(folder) {
  library(magrittr)

  added_words <- c("rangelands", "ers", "sra", "reverse", "operationalize", "operationalized", "pastoralism", "hydropower", "landuse", "smallscale", "largescale", "percent",
                   "underexploited", "agroforestry", "siltation", "intergovernmental", "subsector", "subsectors", "rainfall", "rangeland", "agribusiness", "flr", "landscape", "cop", "agroforest", "kenya", "malawi", "rwanda", "programmes", "programme", "fuelwood", "ngos", "kenyas", "kigali", "slm", "africa", "ghg", "sectoral", "kenyan", "malawis", "african", "longterm", "woodfuels", "timeframe", "nairobi", "fao","sdg", "sdgs", "agro", "npv", "rainfed", "streambank", "cookstoves", "transboundary", "anthropogenic", "manmade", "geospatial", "subcomponent", "nontimber", "agroprocessing", "multicriteria", "crossectoral", "microfinance", "evapotranspiration", "silviculture", "nonforested", "eutrophication", "sociocultural", "gdp", "usd", "hiv", "womens", "un", "uk", "eu")
  folders <- list.files(path=folder)
  folders <- folders[grepl(".pdf", folders) == F]
  subfolders <- paste0(folder, "/", folders)

  allfiles <- list()
  for(i in c(1:length(subfolders))) {
    allfiles[[i]] <- paste0(subfolders[i], "/", list.files(subfolders[i]), "/")
  }
  subfolders <- unlist(allfiles)

  count_dig <- function(s) {
      s2 <- gsub("[0-9]","", df$sentences[s])
      perc <- round(((1-(nchar(s2)/nchar(df$sentences[s])))*100),1)
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
    lang <- cld2::detect_language(as.character(df[n, 1]))
    return(lang)
  }

  check_bad <- function(id, in_dict) {
    setTxtProgressBar(pb, id)
    sentence <- tolower(df$sentences[id])
    bad_words <- unlist(hunspell::hunspell_find(sentence, ignore = added_words,
      dict = hunspell::dictionary(in_dict)))
    return(bad_words)
  }

  # Use the hunspell package to automatically fix mispelled words
  check_spelling <- function(id) {
    setTxtProgressBar(pb, id)
    sentence <- tolower(df$sentences[id])
    bad_words <- unlist(hunspell::hunspell_find(sentence, ignore = added_words))
    if(length(bad_words) > 0) {
      suggested <- hunspell::hunspell_suggest(bad_words)
      for(i in seq_along(bad_words)) {
        bad_words[i] <- paste("\\s+", bad_words[i], "\\s+", sep="")
      }
      replacements <- rep(NA, length(suggested))
      for(i in c(1:length(suggested))) {
        replacements[i] <- paste0(" ",suggested[[i]][1], " ")
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
    bigrams <- calcbigram(df$sentences)
    toremove <- bigrams$term[1:100]
    
    if(length(which(grepl("\\[|\\+", toremove))) > 0) {
      print(which(grepl("\\[|\\+", toremove)))
      toremove <- toremove[-which(grepl("\\[|\\+", toremove))]
    }
    
    res <- df$sentences
    for(i in seq_along(toremove)) {
      remove <- gsub(" ", "\\\\s+", toremove[i])
      print(sum(grepl(remove, res)))
      res <- gsub(unlist(remove), "", res)
    }
    return(res)
  }

  total_data <- create_df(subfolders, main=T)
  total_data <- total_data[nchar(total_data$sentences) > 70 & !is.na(total_data$sentences),]
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
  df <- total_data
  cat("Text cleaned", "\n")

  # Subset to english
  df$languages <- rep(NA, nrow(df))
  df <- df[!is.na(df$sentences),]
  df <- df[!duplicated(df$sentences),]
  df$languages <- unlist(lapply(c(1:nrow(df)), detect_language))
  df <- df[df$languages == "en",]
  df <- df[!duplicated(df$sentences),]
  cat("Text subsetted to english", "\n")

  df$sentences <- gsub("([A-z])-\\s+([a-z])", "\\1\\2", df$sentences)
  df$sentences <- gsub("([A-z])\\s+-([a-z])", "\\1\\2", df$sentences)

  # Fix spelling errors
  cat("Beginning spelling correction, this may take awhile!", "\n")
  cat("\nCreating dictionary", "\n")
  pb <- txtProgressBar(min = 0, max=nrow(df), style=3)
  bad_words_en <- unlist(lapply(1:nrow(df), check_bad, "en_US"))
  close(pb)
  pb <- txtProgressBar(min = 0, max=nrow(df), style=3)
  bad_words_gb <- unlist(lapply(1:nrow(df), check_bad, "en_GB"))
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
  pb <- txtProgressBar(min = 0, max=nrow(df), style=3)
  corrected <- unlist(lapply(1:nrow(df), check_spelling))
  close(pb)
  cat("\n")
  df$sentences <- corrected
  df$sentences <- enc2utf8(df$sentences)
  df$sentences <- unlist(lapply(c(1:nrow(df)), function(x) tolower(df$sentences[x])))
  df$sentences <-gsub(":", "", df$sentences)
  df$sentences <- as.character(df$sentences)
  df <- df[!is.na(df$sentences),]
  df <- df[!duplicated(df$sentences),]


  df <- df[nchar(df$sentences) > 50,]
  perc <- unlist(lapply(c(1:nrow(df)), count_dig))
  df <- df[perc < 10,]

  citation <- grepl("\\.,", df$sentences)
  eg <- grepl("e\\.g\\.", df$sentences)
  etal <- grepl("et al\\.,", df$sentences)
  etal2 <- grepl("et\\. al\\.,", df$sentences)
  citation[eg == T | etal == T | etal2 == T] <- F

  if(sum(citation == T) > 0) {
    df <- df[-which(citation == T),]
    cat("removed", sum(citation), "files")
  }

  citation2 <- grepl("\\&", df$sentences) & grepl("\\s+[a-z]{1}[.]", df$sentences) 

  if(sum(citation2 == T) > 0) {
    df <- df[-which(citation2 == T),]
  }

  df <- df[-grepl(",\\s+[a-z]{1}[.]", df$sentences),]
  df$sentences <- gsub("\\s+[bcdefghijklmnopqrstuvwxyz]{1}\\s+", "", df$sentences)
  if(is.na(df$country)) {
    df$country <- as.character(length(unique(gsub("[0-9]{1,}[.]txt", "", df$name))))
  }

  # Write cleaned-up dataframe to a CSV for further analysis
  write.csv(df, "full_corpus.csv")
  return(df)
  cat("Wrote the corpus as full_corpus.csv")
}