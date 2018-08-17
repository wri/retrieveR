#' Reads in htmls to a data frame
#' @param path Path to the folder containing PDFs
#' @keywords wrapper
#' @export
#' @examples
#' data <- prep_htmls("path/to/folder")
#'
prep_htmls <- function(path) {
  read_html <- function(path) {
    files <- list.files(path)
    files <- files[grepl("html", files)]
    print(files)
    htmls <- lapply(X=files,
                    FUN=function(file){
                      .con <- file(description=paste(path, file, sep='/'))
                      .html <- readLines(.con)
                      close(.con)
                      #names(.html) <- file
                      .html
                    })

    htmls <- gsub("<(.|\n)*?>","",htmls)
    htmls <- gsub("\\\\","",htmls)
    names <- gsub("[.]html", "", files)
    dir.create(paste0(path, "/results"))
    for(i in c(1:length(htmls))) {
      write.table(htmls[i], paste0(path, "/results/", files[i], ".txt"), row.names = F, col.names = F, quote=F)
    }
    return(htmls)
  }

  names <- list.files(path)
  data <- read_html(path)
  make_data_frame <- function(id) {
    items <- strsplit(data[id], "\n")
    df <- data.frame(sentences = items,
                     name = rep(names[id], length(items)),
                     page = rep("None", length(items)),
                     country = rep(NA, length(items)))
    colnames(df) <- c("sentences", "name", "page", "country")
    df$sentences <- as.character(df$sentences)
    df$name <- as.character(df$name)
    df$sentences <- gsub('""', "", df$sentences)
    df$sentences <- gsub('^"|', "", df$sentences)
    df$sentences <- gsub('",$', "", df$sentences)
    df$sentences <- gsub('"', "", df$sentences)
    return(df)
  }
  dfs <- lapply(c(1:length(data)), make_data_frame)
  dfs <- do.call("rbind", dfs)
  return(dfs)
}
