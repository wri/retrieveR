library(hunspell)
library(wordVectors)
suppressMessages(library(tm))
suppressMessages(library(tidyverse))
library(corpus)
library(rmarkdown)
suppressMessages(library(combinat))

cat("\n")

args <- commandArgs(trailingOnly = TRUE)
country <- args[1]
#query <- eval(parse(text=(args[2])))
query <- args[2]
#query <- gsub('^"|"$', "", query)

cat(paste0("Querying ", country, "'s", " documents for ", paste(query, collapse=" "), "\n"))

cat("Reading in data and neural embeddings \n")
data <- read.csv("../stm/full_corpus.csv")
data$sentences <- as.character(data$sentences)
data <- data[nchar(data$sentences) > 70,]

data$legible <- data$sentences
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




count_dig <- function(s) {
  s2 <- gsub("[0-9]","", data$legible[s])
  perc <- round(((1-(nchar(s2)/nchar(data$legible[s])))*100),1)
  return(perc)
}

create_point <- function(query) {
  query <- unlist(strsplit(query, " "))
  query <- query[!query %in% stopwords_en]
  query <- gsub("[0-9]", "", query)
  query <- gsub("[,.!?:;$%]", "", query)
  query <- query[!query == ""]
  d <- rep(0, 250)
  numb <- 0
  for(i in c(1:length(query))) {
    x <- df1[rownames(df1) == query[i],]
    if(nrow(x) == 0) {
      cat(query[i], ", ")
      x <- rep(0, 250)
      numb <- numb + 1
    }
    d <- d + x
  }
  d <- d/(length(query) - numb)
  d <- d[1,]
  rownames(d) <- 1
  return(d)
}

perc <- unlist(lapply(c(1:nrow(data)), count_dig))

wv1 <- wordVectors::read.binary.vectors("wordvec_ndcs_new.bin")
df1 <- data.frame(wv1@.Data)

bigrams_full <- rownames(df1)[grepl("_", rownames(df1))]
bigrams_full <- bigrams_full[!(grepl("[0-9]", bigrams_full))]
locations <- readRDS("embeddings.rds")
data$sentences <- gsub("-", " ", data$sentences)


create_query <- function(inp) {
  #unlist the input and remove stopwords
  inp <- unlist(str_split(inp, " "))
  inp <- inp[!inp %in% stopwords_en]
  if(length(inp) > 1) {
    ngrams <- list()
    comb <- lapply(c(2:length(inp)), function(y) combn(inp, y))
    for(i in c(1:length(comb))) {
      if(!is.null(ncol(comb[[i]]))) {
        for(x in c(1:(ncol(comb[[i]])))) {
          temp <- permn(comb[[i]][,x])
          for(h in c(1:length(temp))) {
            bigrams <- paste(temp[[h]], collapse="_")
            ngrams[length(ngrams) +1] <- bigrams
          }
        }
      } else {
        temp <- permn(comb[[i]])
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
  cosineSimilarity(t(as.matrix(vector)), as.matrix(input))
}

create_docmap <- function(thresh, inp_country) {
  topn <- data %>%
    arrange(desc(results)) %>%
    select(name, country, legible, results) %>%
    filter(results > thresh) %>%
    filter(country == inp_country) %>%
    group_by(name) %>%
    summarise(density=n())
  
  topn$name <- gsub("stm-documents/Rwanda/", "", topn$name)
  topn$name <- gsub("stm-documents/Malawi/", "", topn$name)
  topn$name <- gsub("stm-documents/Kenya/", "", topn$name)
  topn$name <- gsub("[.]txt", "", topn$name)
  
  allnames <- data %>%
    dplyr::group_by(country, name) %>%
    dplyr::filter(country==inp_country) %>%
    dplyr::summarise(total=n())
  
  allnames$name <- gsub("stm-documents/Rwanda/", "", allnames$name)
  allnames$name <- gsub("stm-documents/Malawi/", "", allnames$name)
  allnames$name <- gsub("stm-documents/Kenya/", "", allnames$name)
  allnames$name <- gsub("[.]txt", "", allnames$name)
  
  topn <- left_join(allnames, topn, by = "name")
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
    topn$name <- gsub("stm-documents", "", topn$name)
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
    arrange(desc(results)) %>%
    select(name, country, legible, results) %>%
    filter(country==target_country) %>%
    #    top_n(40) %>%
    filter(results > 0.45) %>%
    arrange(name, desc(results))
  
  topn$legible <- gsub("^([0-9])[.]", "\\1", topn$legible)
  topn$legible <- gsub("^[A-Z]{1}\\s+", "", topn$legible)
  topn$legible <- gsub("^([a-z])", "\\U\\1", perl=T, topn$legible)
  topn$legible <- gsub("(^\\d+)\\s+([a-z])", "\\1 \\U\\2", perl=T, topn$legible)
  topn$legible <- gsub("(^\\d+[.]?\\d+)\\s+([a-z])", "\\1 \\U\\2", perl=T, topn$legible)
  topn$legible <- gsub("^\\d+", "", topn$legible)
  topn$legible <- gsub("^\\.\\s+", "", topn$legible)
  topn$legible <- gsub("^\\.\\d+\\s+", "", topn$legible)

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
  cat("---", paste0('title: ', title), paste0('subtitle: Text extracted from ', target_country, ' Policy Documents'), "output:", "pdf_document:", "fig_caption: yes", "---", "!['Why won't this caption show up?'](plot1.png)", my_text, sep="  \n", file=filename)
  render(filename, pdf_document(), quiet=T)
  file.remove(filename) #cleanup
}

query <- create_query(query)
query_vector <- create_point(query)
cat("\n")
cat("Top 50 related words \n")
cat("If some are not relevant, add one or two of the relevant ones to the query \n\n")
unname(unlist(wv1 %>% closest_to(as.matrix(query_vector), n=50) %>% select(word)))
cat("\n\n")

create_results <- function() {
  cat(paste0("Running query on ", length(locations), " documents", "\n"))
  pb <- txtProgressBar(min = 0, max=length(locations), style = 3)
  run_query2 <- function(x, input) {
    setTxtProgressBar(pb, x)
    vector <- unlist(locations[x])
    cosineSimilarity(t(as.matrix(vector)), as.matrix(input))
  }
  results <- lapply(c(1:length(locations)), run_query2, query_vector)
  cat("\n")
  return(results)
  cat("\n")
  close(pb)
}


results <- create_results()
#results <- pblapply(c(1:length(locations)), run_query, query_vector)
data$results <- unlist(results)
data <- data[perc < 15,]
data <- data[-grep("\\d+$", data$sentences),]
data <- data[-grep("\\d+\\s+$", data$sentences),]
citation <- grepl("\\.,", data$sentences)
eg <- grepl("e\\.g\\.", data$sentences)
citation[eg == T] <- F
if(sum(citation) > 0) {
  data <- data[-which(citation == T),]
}
data <- data[-grep("[%]\\s+?", data$sentences),]

testing <- lapply(seq(0.45,0.55, 0.05), create_docmap, country)
testing <- do.call("rbind", testing)
testing <- testing[,-c(1,3,4,5)]
testing$thresh <- as.factor(testing$thresh)
testing <- gather(testing, key = name, value = amount, -thresh, -density2)
colnames(testing) <- c("density2", "thresh", "name2", "name")
testing$yes <- 0
ggplot(data=testing, aes(x=reorder(name,density2), y=yes))+
  geom_tile(aes(fill=density2))+
  coord_flip()+
  scale_fill_distiller(palette="BuPu", direction=1)+
  xlab("")+
  ylab("")+
  facet_grid(.~thresh)+
  theme(line = element_blank(), rect = element_blank(), axis.title = element_blank(), 
        legend.text = element_text(size = rel(0.8)), legend.title = element_text(hjust = 0), 
        legend.position="none",
        strip.text = element_text(size = rel(0.8)), axis.text.x=element_blank(), plot.margin = unit(c(0, 
                                                                                                      0, 0, 0), "lines"), complete = TRUE)

wd <- getwd()
ggsave(filename=paste0(wd, "/plot1.png"), last_plot(), width=7, height=5, units="in")

cat(paste0("\n", "Creating ", paste(query, collapse="_"), ".pdf", "\n"))
suppressWarnings(create_report(country))
cat(paste0(paste(query, collapse="_"), ".pdf"), "created", "\n")
file.remove("plot1.png")
file.remove("toprint.txt")
