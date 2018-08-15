#' Install the wordVectors package from binary
#' @param
#' @param
#' @keywords creation
#' @export
#' @examples
#' install_windows()


install_mac <- function() {
  download.file("https://github.com/wri/retrieveR/raw/master/data-raw/wordVectors_2.0.zip", destfile="wordVectors_2.0.zip", quiet=T, mode="wb")
  install.packages("wordVectors_2.0.zip", repos=NULL, type="binary")
  file.remove("wordVectors_2.0.zip")
}
