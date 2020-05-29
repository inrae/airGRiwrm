#' RunModel function for both airGR and GriwrmInputsModel object
#'
#' @param InputsModel object of class \emph{InputsModel}, see \code{\link[airGR]{CreateInputsModel}} for details.
#' @param ... further arguments passed to or from other methods.
#'
#' @return
#' @export
RunModel <- function(InputsModel, ...) {
  UseMethod("RunModel", InputsModel)
}
