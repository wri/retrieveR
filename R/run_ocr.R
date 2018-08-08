#' Runs OCR on the input corpus
#' @keywords creation
#' @export
#' @examples
#' run_ocr()

#run_ocr <- function() {
#  download.file("https://raw.githubusercontent.com/wri/retrieveR/master/data-raw/ocr.py", destfile = "ocr.py")
#  reticulate::source_python("ocr.py")
#  file.remove("ocr.py")
#}

#run_ocr <- function(file) {
#  pages <- pdftools::pdf_info(file)$pages
#  dir.create(gsub("[.]pdf", "", file))
#  dir <- gsub("[.]pdf", "", file)
#  for(i in c(1:pages)) {
#    pdftools::pdf_convert(file, format="tiff", dpi=250, page = i, filenames=paste0(dir, "/", as.character(i), ".tiff"), verbose=F)
#    output <- rtika::tika_text(paste0(dir, "/", as.character(i), ".tiff"))
#    write.table(output, paste0(dir, "/", as.character(i), ".txt"), row.names=F, col.names=F, quote=F)
#  }
#}


run_ocr <- function(file) {
  dir.create(gsub("[.]pdf", "", file))
  dir <- gsub("[.]pdf", "", file)
  tabulizer::split_pdf(file, outdir=dir)
  pages <- length(list.files(dir))
  print(pages)
  for(i in c(1:pages)) {
    print(paste0(dir, "/", dir, as.character(i), ".pdf"))
    output <- rtika::tika_text(paste0(dir, "/", dir, as.character(i), ".pdf"))
    write.table(output, paste0(dir, "/", as.character(i), ".txt"), row.names=F, col.names=F, quote=F)
  }
}
