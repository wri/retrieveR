#' Download neural embeddings pre-trained on 1,000+ environmental policy documents from 40 nations and 50+ NGOs.
#' @param path Location to save the downloaded embeddings to, defaults to "/embeddings.bin"
#' @keywords creation
#' @export
#' @examples
#' download_embeddings()

download_embeddings <- function(path = "embeddings.bin") {
	download.file("https://github.com/wri/retrieveR/raw/master/data-raw/embeddings.bin", destfile = path, mode="wb")
}
