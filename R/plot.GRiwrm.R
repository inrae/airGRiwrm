#' Display of a diagram representing the network structure of a GRiwrm object
#'
#' @param x \[GRiwrm object\] data to display. See [CreateGRiwrm] for details
#' @param display [logical] if `TRUE` plots the diagram, returns the mermaid code otherwise
#' @param orientation [character] orientation of the graph. Possible values are
#'        "LR" (left-right), "RL" (right-left), "TB" (top-bottom), or "BT" (bottom-top).
#' @param with_donors [logical] for drawing boxes around ungauged nodes and their donors
#' @param box_colors [list] containing the color used for the different types of nodes
#' @param defaultClassDef [character] default style apply to all boxes
#' @param header mermaid script to add before the generated script (init configuration)
#' @param footer mermaid script to add after the generated script
#' @param ... further parameters passed to [mermaid]
#'
#' @details This function only works inside RStudio because the HTMLwidget produced by DiagrammeR
#' is not handled on some platforms
#'
#' @return Mermaid code of the diagram if display is `FALSE`, otherwise the function returns the diagram itself.
#'
#' @export plot.GRiwrm
#' @export
#' @seealso [CreateGRiwrm()]
#'
#' @example man-examples/CreateGRiwrm.R
#'
plot.GRiwrm <- function(x,
                        display = TRUE,
                        orientation = "LR",
                        with_donors = TRUE,
                        box_colors = c(UpstreamUngauged = "#eef",
                                       UpstreamGauged = "#aaf",
                                       IntermediateUngauged = "#efe",
                                       IntermediateGauged = "#afa",
                                       Reservoir = "#9de",
                                       DirectInjection = "#faa"),
                        defaultClassDef = "stroke:#333",
                        header = "%%{init: {'theme': 'neutral'} }%%",
                        footer = NULL,
                        ...) {

  stopifnot(inherits(x, "GRiwrm"),
            is.logical(display),
            length(display) == 1,
            is.character(orientation),
            length(orientation) == 1,
            is.character(box_colors),
            length(setdiff(names(box_colors), c("UpstreamUngauged", "UpstreamGauged",
                                                "IntermediateUngauged",   "IntermediateGauged",
                                                "DirectInjection", "Reservoir"))) == 0)
  x <- sortGRiwrm4plot(x)
  nodes <- unlist(sapply(unique(x$donor), plotGriwrmCluster, x = x, with_donors = with_donors))
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
          paste0(names(box_colors[1:5]), "Diversion"),
          sprintf("fill:%s, stroke:%s, stroke-width:3px", box_colors[1:5], box_colors["DirectInjection"]))
  )
  if (length(getDiversionRows(g2)) > 0) {
    css <- c(css,
             paste("linkStyle",
                   getDiversionRows(g2) - 1,
                   sprintf("stroke:%s, stroke-width:2px,stroke-dasharray: 5 5;",
                           box_colors["DirectInjection"])))
  }
  diagram <- paste(c(header, paste("graph", orientation), nodes, links, node_class, css, footer),
                   collapse = "\n\n")
  class(diagram) <- c("mermaid", class(diagram))
  if (display) {
    plot(diagram, ...)
  } else {
    return(diagram)
  }
}

#' Order GRiwrm network grouping it by donor
#'
#' This sort algorithm respects the original order of nodes but reorder nodes
#' by donor groups by conserving the sort of first nodes by donor groups.
#'
#' @param g
#'
#' @return *GRiwrm*
#' @noRd
#'
sortGRiwrm4plot <- function(g) {
  class_g <- class(g)
  g <- g %>% group_by(.data$donor)
  r <- attr(g, "groups")$.rows
  r_min <- sapply(r, min)
  r <- unlist(r[order(r_min)])
  x <- g[r, ]
  class(x) <- class_g
  return(x)
}

#' Mermaid script for one donor cluster
#'
#' @param d donor id
#' @param x GRiwrm
#'
#' @return mermaid script
#' @noRd
#'
plotGriwrmCluster <- function(d, x, with_donors) {
  x <- x[getDiversionRows(x, TRUE), ]
  cluster_nodes <- sprintf("id_%1$s[%1$s]", x$id[is.na(d) | !is.na(x$donor) & x$donor == d])
  if (length(cluster_nodes) > 1 && with_donors && !is.na(d)) {
    s <- c(sprintf("subgraph donor_%1$s [%1$s]", d),
           cluster_nodes,
           "end")
  } else {
    s <- cluster_nodes
  }
  return(s)
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

#' Plot a mermaid diagram
#'
#' These functions download the diagram from https://mermaid.ink which generates the image.
#'
#' @details
#' Compared to the `diagrammeR::mermaid` function, the generated image or plot
#' is not a HTMLwidget and can be knit in pdf through latex and
#' moreover, its size can be controlled with `fig.width` and `fig.height`.
#'
#' If the generation failed (due to internet connection failure or syntax error
#' in mermaid script), the functions raises no error (see `mermaid` returned value).
#'
#' @param diagram Diagram in mermaid markdown-like language or file (as a connection or file name) containing a diagram specification
#' @param theme Mermaid theme (See https://mermaid.js.org/config/theming.html#available-themes)
#' @param format Image format (either `"jpg"`, or `"png"`, or `"svg"`)
#' @param dir.dest Destination folder for the downloaded image. This parameter is
#' ignored if `file.dest` contains a folder path.
#' @param file.dest Path to the downloaded image. It's combined with `dir.dest`
#' if it only contains the name of the file without a folder path.
#' @param link Link generated by [mermaid_gen_link]
#' @param server URL of the server used to generate the link
#'
#' @return
#' - `mermaid` returns the path to the downloaded image or `NA` if the download failed.
#' In this latter case, get the error message in the attribute "error".
#' - `mermaid_gen_link` returns the link to the web service which generates the diagram
#' - `plot.mermaid` produces a R plot with the mermaid diagram
#'
#' @rdname mermaid
#' @export
#'
#' @examples
#' diagram <- "flowchart LR\n  A --> B"
#' mermaid_gen_link(diagram)
#' f <- mermaid(diagram)
#' f
#' \dontrun{
#' # For displaying the diagram in Rmarkdown document
#' knitr::include_graphics(mermaid(diagram))
#' }
#'
#' # Clean temporary folder
#' unlink(f)
mermaid <- function(diagram,
                    format = "png",
                    theme = "default",
                    dir.dest = tempdir(),
                    file.dest = paste0(rlang::hash(link), ".", format),
                    link = mermaid_gen_link(diagram, theme = theme, format = format)) {
  if (!dir.exists(dir.dest)) dir.create(dir.dest, recursive = TRUE, showWarnings = FALSE)
  if (dirname(file.dest) == ".") file.dest <- file.path(dir.dest, file.dest)
  if (!file.exists(file.dest)) {
    ret <- tryCatch(
      utils::download.file(link, file.dest, quiet = TRUE, mode = "wb"),
      error = function(e) e
    )
    if (inherits(ret, "error")) {
      file.dest <- NA
      attr(file.dest, "error") <- ret$message
    }
  }
  return(file.dest)
}

#' Compress data in pako format
#'
#' @param data [character] data to compress
#'
#' @return Raw compresses data.
#' @source Translated from python script
#' https://github.com/mermaid-js/mermaid-live-editor/discussions/1291#discussioncomment-6837936
#' @noRd
pako_deflate <- function(data) {
  # compress = zlib.compressobj(9, zlib.DEFLATED, 15, 8,zlib.Z_DEFAULT_STRATEGY)
  compress <- zlib::compressobj(
    level = 9,
    method = zlib::zlib$DEFLATED,
    wbits = 15,
    memLevel = 8,
    strategy = zlib::zlib$Z_DEFAULT_STRATEGY
  )
  compressed_data <- compress$compress(charToRaw(data))
  compressed_data <- c(compressed_data, compress$flush())
return(compressed_data)
}

#' @rdname mermaid
#' @export
mermaid_gen_link <- function(diagram, theme = "default", format = "png", server = "https://mermaid.ink") {
  is_connection_or_file <- inherits(diagram[1], "connection") ||
    file.exists(diagram[1])
  if (is_connection_or_file) {
    diagram <- readLines(diagram, encoding = "UTF-8", warn = FALSE)
  }
  if (length(diagram) > 1) {
    diagram <- paste(diagram, collapse = "\n")
  }
  jGraph <-
    list(code = diagram,
         mermaid = list(theme = theme)) |> jsonlite::toJSON(auto_unbox = TRUE)
  deflated <- pako_deflate(jGraph)
  dEncode = gsub("\n", "", jsonlite::base64url_enc(deflated))
  mode <- ifelse(format != "svg", "img", "svg")
  link = sprintf("%s/%s/pako:%s", server, mode, dEncode)
  if (format != "svg") {
    link <- paste0(link, "?type=", format)
  }
  return(link)
}

#' Plot a PNG file
#'
#' @source From https://stackoverflow.com/a/28729601/5300212
#' @param path Path of the file
#' @param add [logical] Add the image to the existing plot
#' @param pic output of
#'
#' @return Nothing, used to side effect.
#' @noRd
#'
plot_png <- function(path, add = FALSE) {
  # read the file
  pic <- png::readPNG(path, native = TRUE)
  res <- dim(pic)[2:1] # get the resolution, [x, y]
  if (!add) {
    opar <- par(mar = c(0, 0, 0, 0))
    plot(
      1,
      1,
      xlim = c(1, res[1]),
      ylim = c(1, res[2]),
      asp = 1,
      type = 'n',
      xaxs = 'i',
      yaxs = 'i',
      xaxt = 'n',
      yaxt = 'n',
      xlab = '',
      ylab = '',
      bty = 'n'
    )
    par(opar)
  }
  graphics::rasterImage(pic, 1, 1, res[1], res[2], xpd = TRUE)
}

#' @param x [character] mermaid diagram dialect
#' @param add [logical] to add the diagram on the existing plot
#' @param ... Other argument passed to [mermaid]
#'
#' @return Nothing, used for side effect.
#' @export plot.mermaid
#' @export
#' @rdname mermaid
#'
#' @examples
#' s <- "flowchart LR
#' A -> B"
#' class(s) <- c("mermaid", class(s))
#' plot(s)
plot.mermaid <- function(x, add = FALSE, ...) {
  file_mmd <- mermaid(x, ...)
  if (is.na(file_mmd)) {
    warning("Mermaid diagram generation failed with error:\n",
            attr(file_mmd, "error"))
    return(invisible())
  }
  plot_png(file_mmd, add = add)
  unlink(file_mmd)
  invisible()
}
