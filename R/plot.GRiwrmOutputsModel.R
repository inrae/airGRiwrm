#' Function which creates screen plots giving an overview of the models outputs in the GRiwrm network.
#'
#' @param x object of class *GRiwrmOutputsModel*, see [RunModel.GRiwrmInputsModel]
#' @param Qobs (optional) [numeric] [matrix] time series of observed flow
#'        (for the same time steps than simulated) (mm/time step) with one column
#'        by hydrological model output named with the node Id (See [GRiwrm])
#' @param ... Further arguments for [airGR::plot.OutputsModel] and [plot]
#'
#' @return [list] of plots
#'
#' @export
#'
#' @inherit RunModel.GRiwrmInputsModel return examples
#'
plot.GRiwrmOutputsModel <- function(x, Qobs = NULL, ...) {
  lapply(
    names(x),
    function(id, OutputsModels) {
      plot(OutputsModels[[id]], Qobs = Qobs[,id] , main = id)
    },
    OutputsModels = x
  )
}
