#' Downloads example PDFs for use with the vignette
#' @keywords creation
#' @export
#' @examples
#' download_example()

download_example <- function() {
  dir.create("pdfs")
  download.file("https://github.com/wri/retrieveR/raw/master/data-raw/pdfs/Collective%20action%20for%20poverty%20reduction.pdf",
                destfile="pdfs/Collective action for poverty reduction.pdf", mode="wb", quiet=TRUE)
  download.file("https://github.com/wri/retrieveR/raw/master/data-raw/pdfs/Collective%20action%20for%20sustainable%20development.pdf",
                destfile="pdfs/Collective action for sustainable development.pdf", mode="wb", quiet=TRUE)
  download.file("https://github.com/wri/retrieveR/raw/master/data-raw/pdfs/Guidlines%20on%20land%20policy%20in%20africa.pdf",
                destfile = "pdfs/Guidelines on land policy in africa.pdf", mode="wb", quiet=TRUE)
  download.file("https://github.com/wri/retrieveR/raw/master/data-raw/pdfs/Markets%20to%20leverage%20investment%20in%20restoration.pdf",
                destfile = "pdfs/Markets to leverage investment in restoration.pdf", mode="wb", quiet=TRUE)
  download.file("https://github.com/wri/retrieveR/raw/master/data-raw/pdfs/Network%20approach%20to%20social%20capacity.pdf",
                destfile = "pdfs/Network approach to social capacity.pdf", mode="wb", quiet=TRUE)
  download.file("https://github.com/wri/retrieveR/raw/master/data-raw/pdfs/Political%20economy%20of%20reforestation.pdf",
                destfile = "pdfs/Political economy of reforestation.pdf", mode="wb", quiet=TRUE)
  download.file("https://github.com/wri/retrieveR/raw/master/data-raw/pdfs/Public-private%20partnership%20for%20restoration.pdf",
                destfile = "pdfs/Public private partnership for restoration.pdf", mode="wb", quiet=TRUE)
  download.file("https://github.com/wri/retrieveR/raw/master/data-raw/pdfs/Scaling%20biomass%20monitoring.pdf",
                destfile = "pdfs/Scaling biomass monitoring.pdf", mode="wb", quiet=TRUE)
  cat("Example files downloaded and placed in the 'pdfs' folder \n")
  openHTML <- function(x) browseURL(paste0('file://', file.path(getwd(), x)))
  download.file("https://github.com/wri/retrieveR/raw/master/vignettes/install.html", destfile = "demo.html", mode = "wb", quiet=T)
  openHTML("demo.html")
}
