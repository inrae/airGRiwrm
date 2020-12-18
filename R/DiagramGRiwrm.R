#' Display a diagram representing the network structure of a GRiwrm object
#'
#' @param griwrm the GRiwrm object to display.
#' @param display if `TRUE` displays the diagram with `DiagrammeR::mermaid`, return the mermaid code otherwise.
#' @param orientation Orientation of the graph. "LR" by default.
#'
#' @details This function only works inside RStudio because the HTMLwidget produced by DiagrammeR
#' is not handled on some platforms
#'
#' @return Mermaid code of the diagram id display is `FALSE`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Display diagram
#' DiagramGRiwrm(griwrm)
#' # Is the same as
#' DiagrammeR::mermaid(DiagramGRiwrm(griwrm, display = FALSE), width = "100%", height = "100%")
#' }
#'
DiagramGRiwrm <- function(griwrm, display = TRUE, orientation = "LR") {
  if(Sys.getenv("RSTUDIO") != "1") {
    return()
  }
  g2 <- griwrm[!is.na(griwrm$down),]
  nodes <- paste(
    g2$id,
    "-->|",
    format(g2$length/1000, trim = TRUE, digits = 0),
    "km|",
    g2$down
  )
  styleSD <- paste("style", unique(g2$down), "fill:#cfc")
  styleDF <- paste("style", unique(g2$id[is.na(g2$model)]), "fill:#fcc")
  diagram <- paste(c(paste("graph", orientation), nodes, styleSD, styleDF), collapse = "\n")
  if(display) {
    DiagrammeR::mermaid(diagram, width = "100%", height = "100%")
  } else {
    return(diagram)
  }
}
