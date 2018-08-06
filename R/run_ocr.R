#' Runs OCR on the input corpus
#' @keywords creation
#' @export
#' @examples
#' run_ocr()

run_ocr <- function() {
  reticulate::source_python("data-raw/ocr.py")
}