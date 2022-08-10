#' Remove unecessary fields in .bib file
#'
#' @param path the path of the .bib file
#' @param pattern pattern for search files in `path`
#' @param rm.fields ([character] [vector]) list of fields to remove
#'
#' @return Function used for side effect.
#' @export
#'
clean_bibtex <- function(path = "./vignettes",
                         pattern = "*.bib",
                         rm.fields = c("abstract", "langid", "file", "keywords", "copyright", "annotation")) {
  files <- list.files(path = path, pattern = pattern)
  message("Found files to clean: ", paste(files, collapse = ", "))
  lapply(files, function(f) {
    s <- readLines(file.path(path, f))
    n <- length(s)
    for (rm.field in rm.fields) {
      s <- s[!grepl(paste0("^\\s*", rm.field), s)]
    }
    writeLines(s, file.path(path, f))
    message(n - length(s), " lines removed in ", f)
  })
  invisible()
}
