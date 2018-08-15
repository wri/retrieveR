#' Install the wordVectors package from source
#' @param
#' @param
#' @keywords creation
#' @export
#' @examples
#' install()


install <- function() {
  download.file("https://github.com/wri/retrieveR/raw/master/data-raw/wordVectors_1.0.tar.gz", destfile="wordVectors_1.0.tar.gz", quiet=T, mode="wb")
  install.packages("wordVectors_1.0.tar.gz", repos=NULL, type="source")
  file.remove("wordVectors_1.0.tar.gz")
}
