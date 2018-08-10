#' Runs OCR on the input corpus
#' @keywords creation
#' @export
#' @examples
#' run_ocr()

run_ocr <- function(path) {
  if(is.na(rtika::tika_jar())){
    rtika::install_tika()
  }
  files <- list.files(path)
  files <- files[grepl("pdf", files)]
  split_files <- function(file) {
    dir <- gsub("[.]pdf", "", file)
    dir.create(paste0(path, "/", dir))
    file <- paste0(path, "/", file)
    tabulizer::split_pdf(file, outdir=paste0(path, "/", dir))
    gc()
    cat("Converting", file, "\n")
  }
  ocr <- function(name) {
    pages <- list.files(name)
    dir <- gsub("[0-9][.]pdf", "", name)
    for(i in c(1:length(pages))) {
      output <- rtika::tika_text(paste0(name, "/", pages[i]))
      file.remove(paste0(name, "/", pages[i]))
      write.table(output, paste0(name, "/", as.character(i), ".txt"), row.names=F, col.names=F, quote=F)
    }
  }
  l <- lapply(files, split_files)
  results <- list.dirs(path)
  results <- results[grepl("/", results)]
  l <- lapply(results, ocr)
}
