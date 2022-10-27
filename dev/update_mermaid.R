# From https://github.com/rich-iannone/DiagrammeR/issues/475#issue-1412818156

updateMermaid <- function(version = "") {
  url <- "https://cdn.jsdelivr.net/npm/mermaid@version/dist/mermaid.min.js"
  if (version != "") {
    stopifnot(grepl("^[[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+$", version))
    version <- paste0("@", version)
  }
  url <- gsub("@version", version, url)
  try(
    download.file(url,
                  system.file("htmlwidgets/lib/mermaid/dist/mermaid.slim.min.js",
                              package = "DiagrammeR"))
  )
}

updateMermaid()
