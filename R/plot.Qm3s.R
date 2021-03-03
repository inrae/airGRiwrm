#' Plot a `Qm3s` object (time series of simulated flows)
#'
#' @param x a [data.frame] with a first column with [POSIXt] dates and followings columns with flows at each node of the network
#' @param type 1-character string (See [plot.default]), default "l"
#' @param xlab a label for the x axis, defaults to "Date"
#' @param ylab a label for the y axis, defaults to "Flow (m3/s)"
#' @param main a main title for the plot, defaults to "Simulated flows"
#' @param col plotting color (See [par]), defaults to rainbow colors
#' @param legend See parameter `legend` of [legend]
#' @param legend.cex `cex` parameter for the text of the legend (See [par])
#' @param lty The line type (See [par])
#' @param ... Further arguments to pass to [plot] or [legend] functions
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics matplot
#' @export
#'
plot.Qm3s <- function(x,
                      type = 'l',
                      xlab = "Date",
                      ylab = "Flow (m3/s)",
                      main = "Simulated flows",
                      col = rainbow(ncol(x) - 1),
                      legend = colnames(x)[-1],
                      legend.cex = 0.7,
                      lty = 1,
                       ...) {
  matplot(
    x$DatesR,
    x[, -1],
    type = type,
    lty = lty,
    xlab = xlab,
    ylab = ylab,
    main = main,
    col = col, ...
  )
  legend('topright',
         legend = legend,
         cex = legend.cex,
         lty = lty,
         col = col, ...)
}
