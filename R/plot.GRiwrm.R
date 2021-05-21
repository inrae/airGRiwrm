#' Display a diagram representing the network structure of a GRiwrm object
#'
#' @param x a GRiwrm object to display (See [CreateGRiwrm])
#' @param display if `TRUE` displays the diagram with [DiagrammeR::mermaid], return the mermaid code otherwise
#' @param orientation a [character] describing the orientation of the graph. Possible values are "LR" (left-right), "RL" (right-left), "TB" (top-bottom), or "BT" (bottom-top). "LR" by default
#' @param width The width of the resulting graphic in pixels (See [DiagrammeR::mermaid])
#' @param height The height of the resulting graphic in pixels (See [DiagrammeR::mermaid])
#' @param ... Other arguments and parameters you would like to send to JavaScript (See [DiagrammeR::mermaid])
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
#' plot.GRiwrm(griwrm)
#' # Is the same as
#' DiagrammeR::mermaid(plot.GRiwrm(griwrm, display = FALSE), width = "100%", height = "100%")
#' }
#'
plot.GRiwrm <- function(x, display = TRUE, orientation = "LR", width = "100%", height = "100%", ...) {
  if(Sys.getenv("RSTUDIO") != "1") {
    return()
  }
  if(!"DiagrammeR" %in% rownames(utils::installed.packages())) {
    stop("The 'DiagrammeR' package should be installed. Type: install.packages('DiagrammeR')")
  }
  g2 <- x[!is.na(x$down),]
  nodes <- paste(
    g2$id,
    "-->|",
    format(g2$length, trim = TRUE, digits = 0),
    "km|",
    g2$down
  )
  styleSD <- paste("style", unique(g2$down), "fill:#cfc")
  styleDF <- paste("style", unique(g2$id[is.na(g2$model)]), "fill:#fcc")
  diagram <- paste(c(paste("graph", orientation), nodes, styleSD, styleDF), collapse = "\n")
  if(display) {
    DiagrammeR::mermaid(diagram = diagram, width, height, ...)
  } else {
    return(diagram)
  }
}
