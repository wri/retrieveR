#' Query a corpus for a topic and create a report summarizing and containing the results
#' @param country Country, must match the groupings from folders 
#' @param query Query, in the form of "this is a query"
#' @param data Dataframe
#' @param embeddings Path to neural embedding (.bin)
#' @param locations Path to locations exported from create_locations
#' @keywords report
#' @export
#' @examples
#' interactive_report()

interactive_report <- function(country=NULL, query, data, embeddings = "embeddings.bin", locations, type="html") {
  library(magrittr)
  if(is.null(country) & length(unique(data$country)) == 1) {
    country <- data$country[1]
  }
  data$page <- unname(data$page)
  data$page <- data$page[,1]
  count_dig <- function(s) {
    s2 <- gsub("[0-9]","", data$legible[s])
    perc <- round(((1-(nchar(s2)/nchar(data$legible[s])))*100),1)
    return(perc)
  }
  
  # Calculate weights for an input using l2 norm
  create_point <- function(query) {
    query <- unlist(strsplit(query, " "))
    query <- query[!query %in% corpus::stopwords_en]
    query <- gsub("[0-9]", "", query)
    query <- gsub("[,.!?:;$%]", "", query)
    query <- query[!query == ""]
    d <- rep(0, 300)
    numb <- 0
    for(i in c(1:length(query))) {
      x <- df1[rownames(df1) == query[i],]
      if(nrow(x) == 0) {
        cat(query[i], ", ")
        x <- rep(0, 300)
        numb <- numb + 1
      }
      d <- d + x
    }
    d <- d/(length(query) - numb)
    d <- d[1,]
    rownames(d) <- 1
    return(d)
  }
  
  create_query <- function(inp) {
    #unlist the input and remove stopwords
    inp <- unlist(stringr::str_split(inp, " "))
    inp <- inp[!inp %in% corpus::stopwords_en]
    # calculate all permutations and combinations of query
    # and check for n-grams
    if(length(inp) > 1) {
      ngrams <- list()
      comb <- lapply(c(2:length(inp)), function(y) combn(inp, y))
      for(i in c(1:length(comb))) {
        if(!is.null(ncol(comb[[i]]))) {
          for(x in c(1:(ncol(comb[[i]])))) {
            temp <- combinat::permn(comb[[i]][,x])
            for(h in c(1:length(temp))) {
              bigrams <- paste(temp[[h]], collapse="_")
              ngrams[length(ngrams) +1] <- bigrams
            }
          }
        } else {
          temp <- combinat::permn(comb[[i]])
          for(h in c(1:length(temp))) {
            bigrams <- paste(temp[[h]], collapse="_")
            ngrams[length(ngrams) +1] <- bigrams
          }
        }
      }
      ngrams <- unlist(ngrams)
      to_add <- ngrams[ngrams %in% bigrams_full==T]
      to_remove <- unique(unlist(strsplit(to_add, "_")))
      inp <- append(inp, to_add)
      #inp <- inp[!inp %in% to_remove]
      return(inp)
    } 
    # If length is 1 just return the input
    else {
      return(inp)
    }
  }
  
  run_query <- function(x, input) {
    vector <- unlist(locations[x])
    wordVectors::cosineSimilarity(t(as.matrix(vector)), as.matrix(input))
  }
  
  create_docmap <- function(thresh, inp_country) {
    topn <- data %>%
      dplyr::arrange(desc(results)) %>%
      dplyr::select(name, country, legible, results) %>%
      dplyr::filter(results > thresh) %>%
      dplyr::filter(country == inp_country) %>%
      dplyr::group_by(name) %>%
      dplyr::summarise(density=n())
    
    topn$name <- gsub("[0-9]{1,}[.]txt", "", topn$name)
    topn$name <- gsub("[\\.]{1,}", "", topn$name)
    
    allnames <- data %>%
      dplyr::group_by(country, name) %>%
      dplyr::filter(country==inp_country) %>%
      dplyr::summarise(total=n())
    
    
    allnames$name <- gsub("[0-9]{1,}[.]txt", "", allnames$name)
    allnames$name <- gsub("[\\.]{1,}", "", allnames$name)
    
    topn <- dplyr::left_join(allnames, topn, by = "name")
    topn$density[is.na(topn$density)] <- 0
    topn$yes <- 0
    topn$density2 <- (topn$density)/sqrt(topn$total)
    topn$density2 <- topn$density2/max(topn$density2)
    topn$thresh <- thresh
    return(topn)
  }
  
  create_report <- function(target_country) {
    calc_append <- function() {
      grouped <- rep(NA, length(topn$name))
      for(i in seq_along(topn$name)) {
        if(i == 1) {
          grouped[i] <- topn$name[i] == topn$name[i+1]
        }
        if(i > 1) {
          grouped[i] <- topn$name[i] == topn$name[i-1]
        }
      }
      grouped <- as.vector(grouped)
      l <- which(grouped == T)
      y <- sort(l)
      g <- cumsum(c(1, abs(y[-length(y)] - y[-1]) > 1))
      paragraphs <- by(y, g, identity)
      pgraphs <- rep(NA, 2)
      for(i in c(1:length(paragraphs))) {
        unlisted <- unlist(paragraphs[i])
        unlisted <- list(unlisted)
        pgraphs[i] <- unname(unlisted)
      }
      check_prev <- function(x) {
        ids <- unlist(pgraphs[x])
        to_check <- ids[1] - 1
        if(is.na(to_check)) {
          toreturn <- pgraphs[x][[1]][1]
        } else if(to_check > 1) {
          if(topn$name[to_check] == topn$name[to_check + 1]) {
            toreturn <- list(append(to_check, unlist(pgraphs[x])))
            toreturn <- toreturn[[1]][1]
          } else {
            toreturn <- pgraphs[x]
            toreturn <- toreturn[[1]][1]
          } 
        } else {
          toreturn <- pgraphs[x][[1]][1]
        }
        return(toreturn)
      }
      
      pgraphs_final <- unname(unlist(lapply(c(1:length(pgraphs)), check_prev), recursive=F))
      topn$name <- gsub("[.]txt", "", topn$name)
      topn$name <- gsub("[\\.]{1,}", "", topn$name)
      topn$name <- paste0("**", topn$name, "**")
      names <- unique(topn$name)
      names2 <- rep(NA, length(names))
      names2 <- list(names2)
      for(i in c(1:length(names))) {
        topaste <- length(which(topn$name == names[i]))
        list_na <- rep(NA, topaste)
        names2[[i]] <- append(names[i], list_na)
      }
      toprint <- unlist(names2)
      text <- topn$legible
      for(i in seq_along(text)) {
        text[i] <- paste0(i, ". ", text[i])
      }
      toprint[is.na(toprint)] <- text
      toprint2 <- rep(NA, length(toprint)*2)
      for(i in seq_along(toprint2)) {
        if(i %% 2 > 0) {
          id <- ceiling(i/2)
          toprint2[i] <- toprint[id]
        }
      }
      toprint2[is.na(toprint2)] <- ""
      return(toprint2)
    }
    
    topn <- data %>%
      dplyr::arrange(desc(results)) %>%
      dplyr::select(name, country, legible, results, sentences, page) %>%
      dplyr::filter(country==target_country) %>%
      dplyr::filter(results > thresh) %>%
      dplyr::arrange(name, desc(results))
    
    topn$legible <- gsub("^([0-9])[.]", "\\1", topn$legible)
    topn$legible <- gsub("^[A-Z]{1}\\s+", "", topn$legible)
    topn$legible <- gsub("^([a-z])", "\\U\\1", perl=T, topn$legible)
    topn$legible <- gsub("(^\\d+)\\s+([a-z])", "\\1 \\U\\2", perl=T, topn$legible)
    topn$legible <- gsub("(^\\d+[.]?\\d+)\\s+([a-z])", "\\1 \\U\\2", perl=T, topn$legible)
    topn$legible <- gsub("^\\d+", "", topn$legible)
    topn$legible <- gsub("^\\.\\s+", "", topn$legible)
    topn$legible <- gsub("^\\.\\d+\\s+", "", topn$legible)
    topn$legible <- gsub("^-", "", topn$legible)
    topn$legible <- gsub("^([a-z])", "\\U\\1", perl=T, topn$legible)
    topn$legible <- gsub("^[.][0-9]{1,}", "", topn$legible)
    topn$legible <- gsub("^[.]", "", topn$legible)
    topn$legible <- paste0(topn$legible, " ", "**", topn$page, "**")
    
    toprint <- calc_append()
    write.table(toprint, "toprint.txt", row.names=F, col.names=F, quote=F)
    my_text <- readLines("toprint.txt")
    filename <- paste0(paste(query[1], query[2], sep="_"), ".Rmd")
    title <- paste(query, collapse = " ")
    title <- gsub("_", " ", title)
    title <- gsub("(a-z])", "\\U\\1", perl=T, title)
    title <- paste(unique(unlist(strsplit(title, " "))), collapse = " ")
    title <- gsub("^([a-z])", "\\U\\1", perl=T, title)
    title <- paste(title, collapse = " ")
    if(type == "html") {
      rend_type <- rmarkdown::html_document()
    }
    if(type == "pdf") {
      rend_type <- rmarkdown::pdf_document()
    }
    print(rend_type)
    cat("---", paste0('title: ', title), paste0('subtitle: Text extracted from ', target_country, ' documents'), "output:", "pdf_document:", "fig_caption: yes", "---", "!['Why won't this caption show up?'](plot1.png)", my_text, sep="  \n", file=filename)
    rmarkdown::render(filename, rend_type, quiet=T)
    file.remove(filename) #cleanup
    write.csv(topn, paste0(country, ".csv"))
  }
  
  cat("\n", paste0("Querying ", country, "'s", " documents for ", paste(query, collapse=" "), "\n"))
  data$sentences <- as.character(data$sentences)
  #data <- data[nchar(data$sentences) > 70,]
  data$legible <- data$sentences
  
  #for(i in c(1:nrow(data))) {
  #  data$country[i] <- unlist(strsplit(as.character(data$name[i]), "/"))[2]
  #}
  data$legible <- gsub("([.]\\s+)([a-z])", "\\1\\U\\2", data$legible, perl=TRUE)
  data$legible <- gsub("_", " ", data$legible)
  data$legible <- gsub("(^[a-z]|^\\s+[a-z])", "\\U\\1", data$legible, perl=TRUE)
  data$legible <- gsub("\\s+([cdfghjklmnpqrstvwxz]{2,12})\\s+", " \\U\\1\ ", data$legible, perl=T)
  data$legible <- gsub("(\\d)\\s+(\\d)", "\\1\\2", data$legible)
  data$legible <- gsub("^\\s+", "", data$legible)
  data$legible <- gsub("\\s+$", "", data$legible)
  data$legible <- gsub("!\\s+([a-z])", "\\! \\U\\1", data$legible, perl=TRUE)
  data$legible <- gsub("\\?\\s+([a-z])", "\\? \\U\\1", data$legible, perl=TRUE)
  data$legible <- gsub("([A-z])-\\s+([a-z])", "\\1\\2", data$legible)
  data$legible <- gsub("([A-z])\\s+-([a-z])", "\\1\\2", data$legible)
  data$name <- stringr::str_match(data$name, "[^/][A-z 0-9]{1,}/[0-9]{1,}[.]txt$")
  data$name <- data$name[,1]
  data$name <- gsub("[0-9]{1,}[.]txt", "", data$name)
  data$name <- gsub("/", "", data$name)
  
  perc <- unlist(lapply(c(1:nrow(data)), count_dig))
  wv1 <- wordVectors::read.binary.vectors(embeddings)
  df1 <- data.frame(wv1@.Data)
  
  bigrams_full <- rownames(df1)[grepl("_", rownames(df1))]
  bigrams_full <- bigrams_full[!(grepl("[0-9]", bigrams_full))]
  locations <- readRDS(locations)
  data$sentences <- gsub("-", " ", data$sentences)
  query <- create_query(query)
  query_vector <- create_point(query)
  cat("\n")
  cat("Top 50 related words \n")
  cat("If some are not relevant, add one or two of the relevant ones to the query \n\n")
  similars <- unname(unlist(wv1 %>% wordVectors::closest_to(as.matrix(query_vector), n=50) %>% dplyr::select(word)))
  print(similars, quote=F)
  cat("\n\n")
  
  ## Step 2
  ## Query expansion
  finalize_query <- function() {
    query_vector <- create_point(query)
    similars <- unname(unlist(wv1 %>% wordVectors::closest_to(as.matrix(query_vector), n=50) %>% dplyr::select(word)))
    cat("\n")
    print(similars, quote=F)
    cat("\n")
  }
  finalized <- 0
  while(finalized == 0) {
    cat("\n", "The current query words are", query, "\n")
    id <- readline(prompt = "Do you want to add words to the query? (Yes/No) ")
    if(id == "No") {
      finalized <- 1
    }
    if(id == "Yes") {
      to_add <- as.character(readline(prompt = "Words to add, separated by a space "))
      to_add <- unlist(strsplit(to_add, split=" "))
      query <- append(query, to_add)
      finalize_query()
      finalized <- 0
    }
  }
  
  create_results <- function() {
    cat(paste0("Running query on ", length(locations), " documents", "\n"))
    pb <- txtProgressBar(min = 0, max=length(locations), style = 3)
    run_query2 <- function(x, input) {
      setTxtProgressBar(pb, x)
      vector <- unlist(locations[x])
      wordVectors::cosineSimilarity(t(as.matrix(vector)), as.matrix(input))
    }
    results <- lapply(c(1:length(locations)), run_query2, query_vector)
    cat("\n\n")
    return(results)
    close(pb)
  }
  
  results <- create_results()
  data$results <- unlist(results)
  data <- data[-grep("\\d+$", data$sentences),]
  data <- data[-grep("\\d+\\s+$", data$sentences),]
  print(nrow(data))
  
  citation <- grepl("\\.,", data$sentences)
  eg <- grepl("e\\.g\\.", data$sentences)
  etal <- grepl("et al\\.,", data$sentences)
  etal2 <- grepl("et\\. al\\.,", data$sentences)
  citation[etal == T | etal2 == T | eg == T] <- F
  torm <- which(citation == T)
  if(length(torm) > 0) {
    data <- data[-torm,]
  }

  data <- data[-grep("[%]\\s+?", data$sentences),]
  data <- data[-grep("\\s+[A-z]{1}\\.\\s+", data$sentences),]

  cat("\n")
  cat("We need to decide the boundary between relevance and non-relevance", "\n")
  cat("If the two sentences below are relevant, enter 'Yes' to expand the results", "\n\n")
  cat("----------------------------------", "\n")
  
  print_margin <- function(thresh, input_country) {
    change <- sum(data$results < thresh & data$results > thresh - 0.01)
    subs <- data[data$results < thresh,]
    subs <- subs[subs$country == input_country,]
    subs <- subs %>%
      dplyr::group_by(sentences) %>%
      dplyr::arrange(dplyr::desc(results))
    cat(input_country, ":", thresh, "-- The highest similarity is", subs$results[1], "and including these would add", change, "paragraphs", "\n\n")
    for(i in c(1:2)) {
      cat(subs$legible[i], "\n\n")
    }
  }
  
  corrected <- 0
  thresh <- 0.55
  test_length <- sum(data$results > thresh)
  while(test_length > 75) {
    before <- test_length
    thresh <- thresh + 0.01
    test_length <- sum(data$results > thresh)
    change <- test_length - before
    cat("Increasing threshold to", thresh, "removing", abs(change), "paragraphs", "\n")
  }
  print_margin(thresh, country)
  
  while(corrected == 0) {
    cat("\n")
    id <- readline(prompt = "Are these relevant? (Yes / No) ")
    if(id == "No") {
      corrected <- 1
    }
    if(id == "Yes") {
      thresh <- thresh - 0.01
      print_margin(thresh, country)
    }
  }
  
  multi_thresh <- c(thresh - 0.05, thresh, thresh + 0.05)
  testing <- lapply(multi_thresh, create_docmap, country)
  testing <- do.call("rbind", testing)
  testing <- testing[,-c(1,3,4,5)]
  testing$thresh[testing$thresh == unique(testing$thresh)[1]] <- "Related topics"
  testing$thresh[testing$thresh == unique(testing$thresh)[2]] <- "Indirect references"
  testing$thresh[testing$thresh == unique(testing$thresh)[3]] <- "Direct references"
  testing$thresh <- as.factor(testing$thresh)
  testing <- tidyr::gather(testing, key = name, value = amount, -thresh, -density2)
  colnames(testing) <- c("density2", "thresh", "name2", "name")
  testing$yes <- 0
  library(ggplot2)
  ggplot2::ggplot(data=testing, ggplot2::aes(x=reorder(name,density2), y=yes))+
    ggplot2::geom_tile(ggplot2::aes(fill=density2))+
    ggplot2::coord_flip()+
    ggplot2::scale_fill_distiller(palette="BuPu", direction=1)+
    ggplot2::xlab("")+
    ggplot2::ylab("")+
    ggplot2::facet_grid(.~thresh)+
    ggplot2::theme(line = ggplot2::element_blank(), rect = ggplot2::element_blank(), axis.title = ggplot2::element_blank(), 
                   legend.text = ggplot2::element_text(size = ggplot2::rel(0.8)), legend.title = ggplot2::element_text(hjust = 0), 
                   legend.position="none",
                   strip.text = ggplot2::element_text(size = ggplot2::rel(0.7)), axis.text.x=ggplot2::element_blank(), complete = FALSE)
  
  wd <- getwd()
  ggplot2::ggsave(filename=paste0(wd, "/plot1.png"), ggplot2::last_plot(), width=7, height=5, units="in")
  cat(paste0("\n", "Creating ", paste(query, collapse="_"), ".", as.character(type)), "\n")
  suppressWarnings(create_report(country))
  cat(paste0(paste(query, collapse="_"), ".", as.character(type)), " created", "\n")
  file.remove("plot1.png")
  file.remove("toprint.txt")
  write.csv(data, "data-results.csv")
}