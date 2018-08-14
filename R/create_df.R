#' A function to create a paragraph-per-row dataframe inheriting folder-level metadata
#' @param folders Path to .txt files from working directory
#' @param main Defaults to true. If true, attempts to find page numbers in the .txt names
#' @keywords creation
#' @export
#' @examples
#' create_df()

create_df <- function(folders, main) {
  create_df_worker <- function(ind) {
    folder <- folders[ind]
    country <- folders[ind] # list of files to read in
    files <- list.files(path=folder, pattern=".txt")
    dir <- paste(folder, files, sep="")

    count_dig <- function(s, data) { #calculate % of paragraph that is numeric
      s2 <- gsub("[0-9]","", data[s])
      perc <- round(((1-(nchar(s2)/nchar(data[s])))*100),1)
      return(perc)
    }

    # reads in all the files in the above list
    readtext <- function(file){
      .con <- file(description=file)
      .text <- readLines(file)
      close(.con)
      names(.text) <- file
      if(sum(nchar(.text)) < 10) {
        .text <- "null"
      }
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

      # find.dup counts the number of consecutive empty lines
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

      # Removes figures and tables
      rmfigtable <- grepl("^Figure\\s+\\d|^figure\\s+\\d|^Table\\s+\\d|^table\\s+\\d", file)
      file <- file[rmfigtable==F]
      digs <- unlist(lapply(c(1:length(file)), count_dig, file))
      file <- file[digs < 50]

      # Group up lines between empty lines (paragraphs) and paste them together
      l <- which(file != "")
      y <- sort(as.vector(l))
      #y <- sort(groups)
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
      pgraphs <- pgraphs[nchar(pgraphs) > 20 & nchar(pgraphs) < 5000]
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
      pgraphs <- pgraphs[haswords ==T & haslower == T,]

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
    country_name <- stringr::str_extract(folders[ind], "/[A-Z]{1}[a-z]{1,}/")
    country_name <- gsub("/", "", country_name)
    f1$country <- rep(country_name, nrow(f1))
    f1$sentences <- as.character(f1$sentences)
    if(main == T) {
      pagenames <- stringr::str_match(f1$name, "[0-9]{1,}[.]txt")
      f1$page <- gsub("[.]txt", "", pagenames)
    }
    return(f1)
  }
  to_return <- lapply(seq_along(folders), create_df_worker)
  to_return <- do.call("rbind", to_return)
  return(to_return)
}