#' RunModel function for both **airGR** InputsModel and GRiwrmInputsModel object
#'
#' @param x \[object of class \emph{InputsModel} or \emph{GRiwrmInputsModel}\] see [CreateInputsModel] for details
#' @param ... further arguments passed to or from other methods
#'
#' @return Either a [list] of OutputsModel object (for GRiwrmInputsModel) or an OutputsModel object (for InputsModel)
#' @export
RunModel <- function(x, ...) {
  UseMethod("RunModel", x)
}
