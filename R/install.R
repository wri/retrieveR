#' Install the wordVectors package from source
#' @param
#' @param
#' @keywords creation
#' @export
#' @examples
#' install()


install <- function() {
  download.file("https://github.com/wri/retrieveR/raw/master/data-raw/wordVectors_1.0.tgz", destfile="wordVectors_1.0.tgz", quiet=T, mode="wb")
  install.packages("wordVectors_1.0.tgz", repos=NULL)
  file.remove("wordVectors_1.0.tgz")
}
