#' Runs OCR on the input corpus
#' @keywords creation
#' @export
#' @examples
#' run_ocr()

run_ocr <- function() {
  download.file("https://raw.githubusercontent.com/wri/retrieveR/master/data-raw/ocr.py", destfile = "ocr.py")
  reticulate::source_python("ocr.py")
  file.remove("ocr.py")
}
