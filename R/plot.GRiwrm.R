#' Display of a diagram representing the network structure of a GRiwrm object
#'
#' @param x \[GRiwrm object\] data to display. See [CreateGRiwrm] for details
#' @param display [logical] if `TRUE` displays the diagram with [DiagrammeR::mermaid], returns the mermaid code otherwise
#' @param orientation [character] orientation of the graph. Possible values are "LR" (left-right), "RL" (right-left), "TB" (top-bottom), or "BT" (bottom-top). "LR" by default
#' @param width [numeric] width of the resulting graphic in pixels (See [DiagrammeR::mermaid])
#' @param height [numeric] height of the resulting graphic in pixels (See [DiagrammeR::mermaid])
#' @param box_colors [list] containing the color used for the different types of nodes
#' @param defaultClassDef [character] default style apply to all boxes
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
                                       IntermediateUngauged = "#efe",
                                       IntermediateGauged = "#afa",
                                       DirectInjection = "#faa",
                                       Reservoir = "#9de"),
                        defaultClassDef = "stroke:#333",
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
                                                "IntermediateUngauged",   "IntermediateGauged",
                                                "DirectInjection", "Reservoir"))) == 0)
  nodes <- sprintf("id_%1$s[%1$s]", x$id)
  g2 <- x[!is.na(x$down),]
  links <- paste(
    sprintf("id_%1$s", g2$id),
    "-->|",
    round(g2$length, digits = 0),
    "km|",
    sprintf("id_%1$s", g2$down)
  )
  x$nodeclass <- sapply(x$id, getNodeClass, griwrm = x)
  node_class <- lapply(unique(x$nodeclass), function(nc) {
    x$id[x$nodeclass == nc]
  })
  names(node_class) <- unique(x$nodeclass)
  node_class <- lapply(node_class, function(id) if (length(id) > 0) paste0("id_", id))
  node_class <- paste("class", sapply(node_class, paste, collapse = ","), names(node_class))
  css <- c(
    paste("classDef default", defaultClassDef),
    paste("classDef", names(box_colors), paste0("fill:", box_colors)),
    paste("classDef",
          paste0(names(box_colors[1:4]), "Diversion"),
          sprintf("fill:%s, stroke:%s, stroke-width:3px", box_colors[1:4], box_colors["DirectInjection"]))
  )
  if (length(getDiversionRows(g2)) > 0) {
    css <- c(css,
             paste("linkStyle",
                   getDiversionRows(g2) - 1,
                   sprintf("stroke:%s, stroke-width:2px,stroke-dasharray: 5 5;",
                           box_colors["DirectInjection"])))
  }
  diagram <- paste(c(paste("graph", orientation), nodes, links, node_class, css),
                   collapse = "\n\n")
  if (display) {
    DiagrammeR::mermaid(diagram = diagram, width, height, ...)
  } else {
    return(diagram)
  }
}

getNodeClass <- function(id, griwrm) {
  props <- getNodeProperties(id, griwrm)
  if (props$DirectInjection) {
    nc <- "DirectInjection"
  }  else if (props$Reservoir) {
    nc <- "Reservoir"
  }  else {
    nc <- paste0(props$position, props$calibration)
  }
  if (props$Diversion) nc <- paste0(nc, "Diversion")
  return(nc)
}
