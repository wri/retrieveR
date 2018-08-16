#' Install the wordVectors package from binary
#' @param
#' @param
#' @keywords creation
#' @export
#' @examples
#' install_windows()


install_windows <- function() {
  cat("Installing neural embedding package \n")
  download.file("https://github.com/wri/retrieveR/raw/master/data-raw/wordVectors_2.0.zip", destfile="wordVectors_2.0.zip", quiet=T, mode="wb")
  install.packages("wordVectors_2.0.zip", repos=NULL, type="binary")
  file.remove("wordVectors_2.0.zip")
  if(is.na(rtika::tika_jar())){
    cat("Installing OCR Java dependency \n")
    rtika::install_tika()
  }
  if(!file.exists("embeddings.bin")) {
    cat("Downloading pre-trained neural embedding")
    download_embeddings()
  }
}
