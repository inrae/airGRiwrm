#' Plot of a `Qm3s` object (time series of simulated flows)
#'
#' This function plot time series of flow rate in m3/s. It's a method for object
#' of class "Qm3s" which can be directly called by `plot`. It can also be called
#' as a function `plot.Qm3s` if the first parameter has the good format.
#'
#' @param x [data.frame] with a first column with [POSIXt] dates and followings
#'        columns with flows at each node of the network
#' @param type [character] plot type (See [plot.default]), default "l"
#' @param xlab [character] label for the x axis, default to "Date"
#' @param ylab [character] label for the y axis, default to "Flow (m3/s)"
#' @param main [character] main title for the plot, default to "Simulated flows"
#' @param col [character] plotting color (See [par]), default to rainbow colors
#' @param legend [character] see parameter `legend` of [legend]. Set it to [NULL]
#'        to hide the legend
#' @param legend.cex [character] `cex` parameter for the text of the legend (See [par])
#' @param legend.x,legend.y Legend position, see `x` and `y` parameters in [graphics::legend]
#' @param lty [character] or [numeric] The line type (See [par])
#' @param ... Further arguments to pass to the [matplot] functions
#'
#' @return Screen plot window.
#'
#' @example man-examples/RunModel.GRiwrmInputsModel.R
#'
#' @importFrom graphics matplot
#' @export plot.Qm3s
#' @export
#'
plot.Qm3s <- function(x,
                      type = "l",
                      xlab = "Date",
                      ylab = expression("Flow rate (m"^"3"*"/s)"),
                      main = "Simulated flows",
                      col = grDevices::hcl.colors(ncol(x) - 1, "Zissou 1"),
                      legend = colnames(x)[-1],
                      legend.cex = 0.7,
                      legend.x = "topright",
                      legend.y = NULL,
                      lty = 1,
                      ...) {

  stopifnot(is.data.frame(x),
            inherits(x[, 1], "POSIXct"))

  matplot(
    x[, 1],
    x[, -1],
    type = type,
    lty = lty,
    xlab = xlab,
    ylab = ylab,
    main = main,
    col = col, ...
  )
  if (!is.null(legend)) {
    legend(x = legend.x,
           y = legend.y,
           legend = legend,
           cex = legend.cex,
           lty = lty,
           col = col)
  }
}

#' Coerce [data.frame]or content of a [data.frame] into a *Qm3s* object ready
#' for plotting
#'
#' @param ... A [data.frame] for a single argument, or the arguments of function [data.frame]
#'
#' @return A [data.frame] of class *Qm3s*
#' @export
#'
as.Qm3s <- function(...) {
  arg_list <- list(...)
  if (length(arg_list) == 1) {
    stopifnot(is.data.frame(arg_list[[1]]))
    df <- arg_list[[1]]
  } else {
    df <- data.frame(...)
  }
  class(df) <- c("Qm3s", class(df))
  return(df)
}
