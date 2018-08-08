#' Creates a Python 2.7 virtual environment for running OCR
#' @param name Name for the virtual environment, defaults to RetrieveR
#' @keywords creation
#' @export
#' @examples
#' create_virtualenv()

create_virtualenv <- function(name = "RetrieveR", type="conda") {
  if(type == "virtual") {
    reticulate::virtualenv_create(name)
    reticulate::virtualenv_install(name, "PyPDF2")
    reticulate::virtualenv_install(name, "tika")
    reticulate::use_virtualenv(name)
  }
  else if(type == "conda") {
    if(!name %in% conda_list()$name) {
      reticulate::conda_create(name)
      reticulate::conda_install(name, "PyPDF2")
      reticulate::conda_install(name, "tika")
      reticulate::use_condaenv(name)
    }
  }
}
