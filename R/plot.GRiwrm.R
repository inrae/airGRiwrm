#' Display of a diagram representing the network structure of a GRiwrm object
#'
#' @param x \[GRiwrm object\] data to display. See [CreateGRiwrm] for details
#' @param display [logical] if `TRUE` displays the diagram with [DiagrammeR::mermaid], returns the mermaid code otherwise
#' @param orientation [character] orientation of the graph. Possible values are "LR" (left-right), "RL" (right-left), "TB" (top-bottom), or "BT" (bottom-top). "LR" by default
#' @param width [numeric] width of the resulting graphic in pixels (See [DiagrammeR::mermaid])
#' @param height [numeric] height of the resulting graphic in pixels (See [DiagrammeR::mermaid])
#' @param box_colors [list] containing the color used for the different types of nodes
#' @param ... Other arguments and parameters you would like to send to JavaScript (See [DiagrammeR::mermaid])
#'
#' @details This function only works inside RStudio because the HTMLwidget produced by DiagrammeR
#' is not handled on some platforms
#'
#' @return Mermaid code of the diagram if display is `FALSE`, otherwise the function returns the diagram itself.
#'
#' @export
#'
#' @example man-examples/CreateGRiwrm.R
#'
plot.GRiwrm <- function(x,
                        display = TRUE,
                        orientation = "LR",
                        width = "100%",
                        height = "100%",
                        box_colors = c(UpstreamUngauged = "#eef",
                                       UpstreamGauged = "#aaf",
                                       IntermUngauged = "#efe",
                                       IntermGauged = "#afa",
                                       DirectInjection = "#faa"),
                        ...) {

  stopifnot(inherits(x, "GRiwrm"),
            is.logical(display),
            length(display) == 1,
            is.character(orientation),
            length(orientation) == 1,
            is.character(width),
            length(width) == 1,
            is.character(height),
            length(height) == 1,
            is.character(box_colors),
            length(setdiff(names(box_colors), c("UpstreamUngauged", "UpstreamGauged",
                                                "IntermUngauged",   "IntermGauged",
                                                "DirectInjection"))) == 0)
  g2 <- x[!is.na(x$down),]
  nodes <- paste(
    sprintf("id_%1$s[%1$s]", g2$id),
    "-->|",
    round(g2$length, digits = 0),
    "km|",
    sprintf("id_%1$s[%1$s]", g2$down)
  )
  node_class <- list(
    UpstreamUngauged = x$id[!x$id %in% x$down & x$model == "Ungauged"],
    UpstreamGauged = x$id[!x$id %in% x$down & x$model != "Ungauged" & !is.na(x$model)],
    IntermUngauged = x$id[x$id %in% x$down & x$model == "Ungauged"],
    IntermGauged = x$id[x$id %in% x$down & x$model != "Ungauged" & !is.na(x$model)],
    DirectInjection = x$id[is.na(x$model)]
  )
  node_class <- lapply(node_class, function(x) if(length(x) > 0) paste0("id_", x))
  node_class[sapply(node_class, is.null)] <- NULL
  node_class <- paste("class", sapply(node_class, paste, collapse = ","), names(node_class))
  css <- paste("classDef", names(box_colors), paste0("fill:", box_colors))
  diagram <- paste(c(paste("graph", orientation), nodes, node_class, css), collapse = "\n\n")
  if (display) {
    DiagrammeR::mermaid(diagram = diagram, width, height, ...)
  } else {
    return(diagram)
  }
}
