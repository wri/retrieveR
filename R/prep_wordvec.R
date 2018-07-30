#' Corpus creating and data cleaning to create a .csv that can be fed into create_wordvec
#' @param corpus Dataframe returned from make_corpus 
#' @param embedding Path to neural embedding (.bin)
#' @keywords creation
#' @export
#' @examples
#' prep_wordvec()


prep_wordvec <- function(x) {
  library(magrittr)
  library(hunspell)

  folder <- 'background-texts'
  folders <- list.files(path=folder, pattern=".txt")
  full_folders <- paste(folder, folders, sep="/")
  added_words <- c("rangelands", "ers", "sra", "reverse", "operationalize", "operationalized", "pastoralism", "hydropower", "landuse", "smallscale", "largescale", "percent",
                   "underexploited", "agroforestry", "siltation", "intergovernmental", "subsector", "subsectors", "rainfall", "rangeland", "agribusiness", "flr", "landscape", "cop", "agroforest", "kenya", "malawi", "rwanda", "programmes", "programme", "fuelwood", "ngos", "kenyas", "kigali", "slm", "africa", "ghg", "sectoral", "kenyan", "malawis", "african", "longterm", "woodfuels", "timeframe", "nairobi", "fao","sdg", "sdgs", "agro", "npv", "rainfed", "streambank", "cookstoves", "transboundary", "anthropogenic", "manmade", "geospatial", "subcomponent", "nontimber", "agroprocessing", "multicriteria", "crossectoral", "microfinance", "evapotranspiration", "silviculture", "nonforested", "eutrophication", "sociocultural", "gdp", "usd", "hiv", "womens", "un", "uk", "eu")

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
    return(bad_words)
  }

  check_spelling <- function(id) {
    setTxtProgressBar(pb, id)
    sentence <- tolower(test2$sentences[id])
    bad_words <- unlist(hunspell_find(sentence, ignore = added_words))
    if(length(bad_words) > 0) {
      suggested <- hunspell_suggest(bad_words)
      for(i in seq_along(bad_words)) {
        bad_words[i] <- paste0("\\s+", bad_words[i], "\\s+")
      }
      replacements <- rep(NA, length(suggested))
      for(i in c(1:length(suggested))) {
        replacements[i] <- paste0(" ", suggested[[i]][1], " ")
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

  total_data <- create_df(full_folders, main=F)

  cat("Data loaded", "\n")
  total_data$text <- unlist(lapply(c(1:nrow(total_data)), rm_multi_space, total_data))
  cat("Extra space removed", "\n")

  total_data$text <- clean_data(total_data)
  total_data$text <- gsub("(\\d+)(\\w)", "\\1 \\2", total_data$text)

  cat("Text cleaned", "\n")
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

  bad_words_en <- enc2utf8(bad_words_en)
  bad_words_en <- bad_words_en[duplicated(bad_words_en)]
  write.csv(bad_words_en, "bad_words_en.csv")
  bad_words_en <- as.data.frame(bad_words_en)
  bad_words_en <- bad_words_en %>%
    dplyr::group_by(bad_words_en) %>%
    dplyr::summarise(n=n()) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::mutate(bad_words_en = as.character(bad_words_en)) %>%
    dplyr::filter(nchar(bad_words_en) > 2)

  pb <- txtProgressBar(min = 0, max=nrow(test2), style=3)
  bad_words_gb <- unlist(lapply(1:nrow(test2), check_bad, "en_GB"))
  close(pb)

  bad_words_gb <- bad_words_gb[duplicated(bad_words_gb)]
  bad_words_gb <- as.data.frame(bad_words_gb)
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
  test2$sentences <- lapply(c(1:nrow(test2)), function(x) tolower(test2$sentences[x]))
  test2$sentences <-gsub(":", "", test2$sentences)

  test2$sentences <- as.character(test2$sentences)
  test2 <- test2[!is.na(test2$sentences),]
  test2 <- test2[!duplicated(test2$sentences),]
  test2 <- as.data.frame(test2)
  colnames(test2) <- "sentences"
  test2 <- test2[,1]

  calcbigram <- function(input) {
    corpus <- tm::Corpus(tm::VectorSource(input))
    corpus <- tm::tm_map(corpus, tm::removePunctuation)
    corpus <- tm::tm_map(corpus, tm::removeNumbers)
    ngrams <- corpus::term_stats(corpus, ngrams=2:4)
    ngrams <- ngrams %>%
      dplyr::arrange(desc(count))
    ngrams <- ngrams[1:25000,]
    return(ngrams)
  }

  bgrams <- calcbigram(test2)
  bgrams$word <- stringr::str_count(bgrams$term, " ")
  bgrams <- bgrams %>%
    dplyr::group_by(term) %>%
    dplyr::arrange(desc(word))

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

  test2 <- bundled
  test2 <- gsub("[!.,;:%\\$]", "", test2)
  test2 <- gsub('"\\/|\\\\', "", test2)
  test2 <- gsub("[0-9]", "", test2)
  test2 <- gsub("([A-z])-\\s+([a-z])", "\\1\\2", test2)
  test2 <- gsub("([A-z])\\s+-\\([a-z])", "\\1\\2", test2)
  test2 <- gsub('\"', "", test2)
  test2 <- gsub("'", "", test2)
  test2 <- gsub("-", "", test2)

  write.csv(test2, "full_lsa_corpus.csv")
  write.table(test2, "full_lsa_text.txt", row.names=F, col.names=F)
}
