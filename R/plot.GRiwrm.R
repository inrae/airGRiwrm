#' Display of a diagram representing the network structure of a GRiwrm object
#'
#' @param x \[GRiwrm object\] data to display. See [CreateGRiwrm] for details
#' @param display [logical] if `TRUE` displays the diagram with [DiagrammeR::mermaid], returns the mermaid code otherwise
#' @param orientation [character] orientation of the graph. Possible values are "LR" (left-right), "RL" (right-left), "TB" (top-bottom), or "BT" (bottom-top). "LR" by default
#' @param width [numeric] width of the resulting graphic in pixels (See [DiagrammeR::mermaid])
#' @param height [numeric] height of the resulting graphic in pixels (See [DiagrammeR::mermaid])
#' @param ... Other arguments and parameters you would like to send to JavaScript (See [DiagrammeR::mermaid])
#'
#' @details This function only works inside RStudio because the HTMLwidget produced by DiagrammeR
#' is not handled on some platforms
#'
#' @return Mermaid code of the diagram if display is `FALSE`, otherwise the function returns the diagram itself.
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
  g2 <- x[!is.na(x$down),]
  nodes <- paste(
    g2$id,
    "-->|",
    round(g2$length, digits = 0),
    "km|",
    g2$down
  )
  styleSD <- paste("style", unique(g2$down), "fill:#cfc")
  if (length(g2$id[is.na(g2$model)]) > 0) {
    styleDF <- paste("style", unique(g2$id[is.na(g2$model)]), "fill:#fcc")
  } else {
    styleDF <- ""
  }
  diagram <- paste(c(paste("graph", orientation), nodes, styleSD, styleDF), collapse = "\n")
  if (display) {
    DiagrammeR::mermaid(diagram = diagram, width, height, ...)
  } else {
    return(diagram)
  }
}
