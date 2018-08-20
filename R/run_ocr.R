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
  dir.create(paste0(path, "/results"))
  output <- paste0(path, "/results")

  split_files <- function(file) {
    id <- which(files == file)
    setTxtProgressBar(pb, id)
    dir <- gsub("[.]pdf", "", file)
    dir.create(paste0(output, "/", dir))
    file <- paste0(path, "/", file)
    tryCatch(
      tabulizer::split_pdf(file, outdir=paste0(output, "/", dir)), error = function(e) {
        pages <- pdftools::pdf_info(file)$pages
        names <- seq(1:pages)
        names <- paste0(output, "/", dir, "/", names, ".png")
        pdftools::pdf_convert(file, filenames = names, dpi = 200, verbose = F) })
    gc()
  }

  ocr <- function(name) {
    id <- which(results == name)
    setTxtProgressBar(pb, id)
    pages <- list.files(name)
    pages <- paste0(name, "/", pages)
    dir <- gsub("[0-9][.]pdf|[0-9][.]png", "", name)
    item <- rtika::tika_text(pages)
    for(i in c(1:length(item))) {
      file.remove(pages[i])
      write.table(item[i], paste0(name, "/", as.character(i), ".txt"), row.names=F,
                  col.names=F, quote=F)
    }
  }

  cat("Splitting pdf files \n")
  pb <- txtProgressBar(min=0, max=length(files), style=3)
  l <- lapply(files, split_files)
  close(pb)
  results <- list.dirs(output)
  results <- results[-1]
  cat("Running OCR on the corpus \n")
  pb <- txtProgressBar(min=0, max=length(results), style=3)
  l <- lapply(results, ocr)
  close(pb)
}
