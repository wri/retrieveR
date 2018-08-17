#' Preps documents for querying
#' @param path Path to the folder containing PDFs
#' @param ocr True/False - whether or not to run OCR, defaults to true
#' @param clean True/False - whether or not to create the corpus dataframe, defaults to true
#' @param weights True/False - whether or not to calculate neural weights, defaults to true
#' @keywords wrapper
#' @export
#' @examples
#' corpus <- prep_documents("path/to/files")

prep_documents <- function(path, ocr = TRUE, clean = TRUE, weights = TRUE, type = "pdf") {
  if(type == "pdf") {
    if(ocr == T) {
      run_ocr(path)
    }
    if(clean == T) {
      corpus <- create_corpus(path)
    }
    if(weights == T) {
      create_locations(corpus)
    }
    return(corpus)
  }
  if(type == "html") {
    corpus <- prep_htmls(path)
    corpus <- create_corpus_html(corpus)
    create_locations(corpus)
    return(corpus)
  }
}
