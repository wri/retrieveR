#' Creates a Python 2.7 virtual environment for running OCR
#' @param name Name for the virtual environment, defaults to RetrieveR
#' @keywords creation
#' @export
#' @examples
#' create_virtualenv()

create_virtualenv <- function(name = "RetrieveR") {
  reticulate::virtualenv_create(name)
  reticulate::virtualenv_install(name, "PyPDF2")
  reticulate::virtualenv_install(name, "tika")
  reticulate::use_virtualenv(name)
}
