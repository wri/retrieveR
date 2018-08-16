#' Install the wordVectors package from source
#' @param
#' @param
#' @keywords creation
#' @export
#' @examples
#' install_mac()


install_mac <- function() {
  download.file("https://github.com/wri/retrieveR/raw/master/data-raw/wordVectors_2.0.tar.gz", destfile="wordVectors_2.0.tar.gz", quiet=T, mode="wb")
  install.packages("wordVectors_2.0.tar.gz", repos=NULL,
                   INSTALL_opts = c('--no-lock'))
  file.remove("wordVectors_2.0.tar.gz")
  if(is.na(rtika::tika_jar())){
    cat("Installing OCR Java dependency \n")
    rtika::install_tika()
  }
  if(!file.exists("embeddings.bin")) {
    cat("Downloading pre-trained neural embedding")
    download_embeddings()
  }
}
