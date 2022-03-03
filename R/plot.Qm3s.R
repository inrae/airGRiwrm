#' Plot of a `Qm3s` object (time series of simulated flows)
#'
#' @param x [data.frame] with a first column with [POSIXt] dates and followings columns with flows at each node of the network
#' @param type [character] plot type (See [plot.default]), default "l"
#' @param xlab [character] label for the x axis, default to "Date"
#' @param ylab [character] label for the y axis, default to "Flow (m3/s)"
#' @param main [character] main title for the plot, default to "Simulated flows"
#' @param col [character] plotting color (See [par]), default to rainbow colors
#' @param legend [character] see parameter `legend` of [legend]. Set to [NULL] if display of the legend is not wanted
#' @param legend.cex [character] `cex` parameter for the text of the legend (See [par])
#' @param legend.x,legend.y Legend position, see `x` and `y` parameters in [graphics::legend]
#' @param lty [character] or [numeric] The line type (See [par])
#' @param ... Further arguments to pass to the [matplot] functions
#'
#' @return Screen plot window.
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics matplot
#' @export
#'
plot.Qm3s <- function(x,
                      type = "l",
                      xlab = "Date",
                      ylab = expression("Flow (m"^"3"*"/s)"),
                      main = "Simulated flows",
                      col = rainbow(ncol(x) - 1),
                      legend = colnames(x)[-1],
                      legend.cex = 0.7,
                      legend.x = "topright",
                      legend.y = NULL,
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
  if(!is.null(legend)) {
    legend(x = legend.x,
           y = legend.y,
           legend = legend,
           cex = legend.cex,
           lty = lty,
           col = col)
  }
}
