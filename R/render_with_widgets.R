#' Query a corpus for a topic and create a report summarizing and containing the results
#' @param input_file Path to input file
#' @keywords report
#' @export
#' @examples
#' render_with_widgets


render_with_widgets <- function(input_file,
                                output_file = sub("\\.Rmd$", ".html", input_file, ignore.case = TRUE),
                                self_contained = TRUE,
                                deps_path = file.path(dirname(output_file), "deps")) {
  library(knitr)
  library(htmltools)
  library(base64enc)
  library(markdown)
  # Read input and convert to Markdown
  input <- readLines(input_file)
  md <- knit(text = input)
  # Get dependencies from knitr
  deps <- knit_meta()

  # Convert script dependencies into data URIs, and stylesheet
  # dependencies into inline stylesheets

  dep_scripts <-
    lapply(deps, function(x) {
      lapply(x$script, function(script) file.path(x$src$file, script))})
  dep_stylesheets <- 
    lapply(deps, function(x) {
      lapply(x$stylesheet, function(stylesheet) file.path(x$src$file, stylesheet))})
  dep_scripts <- unique(unlist(dep_scripts))
  dep_stylesheets <- unique(unlist(dep_stylesheets))
  if (self_contained) {
    dep_html <- c(
      sapply(dep_scripts, function(script) {
        sprintf('<script type="text/javascript" src="%s"></script>',
                dataURI(file = script))
      }),
      sapply(dep_stylesheets, function(sheet) {
        sprintf('<style>%s</style>',
                paste(readLines(sheet), collapse = "\n"))
      })
    )
  } else {
    if (!dir.exists(deps_path)) {
      dir.create(deps_path)
    }
    for (fil in c(dep_scripts, dep_stylesheets)) {
      file.copy(fil, file.path(deps_path, basename(fil)))
    }
    dep_html <- c(
        sprintf('<script type="text/javascript" src="%s"></script>',
                file.path(deps_path, basename(dep_scripts))),
        sprintf('<link href="%s" type="text/css" rel="stylesheet">',
                file.path(deps_path, basename(dep_stylesheets)))
    )
  }

  # Extract the <!--html_preserve--> bits
  preserved <- extractPreserveChunks(md)

  # Render the HTML, and then restore the preserved chunks
  html <- markdownToHTML(text = preserved$value, header = dep_html)
  html <- restorePreserveChunks(html, preserved$chunks)

  # Write the output
  writeLines(html, output_file)
}