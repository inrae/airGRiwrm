#' Function which creates screen plots giving an overview of the model outputs in the GRiwrm network
#'
#' @param x \[object of class *GRiwrmOutputsModel*\] see [RunModel.GRiwrmInputsModel] for details
#' @param Qobs (optional) [matrix] time series of observed flows
#'        (for the same time steps than simulated) (mm/time step) with one column
#'        by hydrological model output named with the node ID (See [CreateGRiwrm] for details)
#' @param ... Further arguments for [airGR::plot.OutputsModel] and [plot]
#'
#' @return [list] of plots.
#'
#' @importFrom graphics plot par title
#' @export
#'
#' @inherit RunModel.GRiwrmInputsModel return examples
#'
plot.GRiwrmOutputsModel <- function(x, Qobs = NULL, ...) {

  ## define outer margins and a title inside it
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(oma = c(0, 0, 3, 0))

  lapply(
    names(x),
    function(id, OutputsModels) {
      plot(OutputsModels[[id]], Qobs = Qobs[,id], ...)
      title(main = id, outer = TRUE, line = 1.2, cex.main = 1.4)
    },
    OutputsModels = x
  )
  invisible(NULL)
}
