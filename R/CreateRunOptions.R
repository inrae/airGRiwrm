#' Create \emph{RunOptions} object for airGR and GR-IWRM. See \code{\link[airGR]{CreateOptions}} and \code{[CreateOptions.GRiwrmInputsModel]}.
#'
#' @param InputsModel object of class \emph{InputsModel} (see \code{\link[airGR]{CreateInputsModel}}) or \emph{GRiwrmInputsModel} (See \code{[CreateInputsModel.GRiwrm]}).
#' @param ... further arguments passed to or from other methods.
#'
#' @return Object of \emph{RunOptions} class family
#' @export
CreateRunOptions <- function(InputsModel, ...) {
  UseMethod("CreateRunOptions", InputsModel)
}
