#' RunModel function for both **airGR** InputsModel and GRiwrmInputsModel object
#'
#' @param InputsModel object of class \emph{InputsModel} or \emph{GRiwrmInputsModel}. See \code{\link{CreateInputsModel}} for details
#' @param ... further arguments passed to or from other methods
#'
#' @return Either a [list] of OutputsModel object (for GRiwrmInputsModel) or an OutputsModel object (for InputsModel)
#' @export
RunModel <- function(InputsModel, ...) {
  UseMethod("RunModel", InputsModel)
}
