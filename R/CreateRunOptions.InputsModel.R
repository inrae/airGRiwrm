#' Create \emph{RunOptions} object for airGR. See \code{\link[airGR]{CreateOptions}}.
#'
#' @param InputsModel object of class \emph{InputsModel}, see \code{\link[airGR]{CreateInputsModel}} for details.
#' @param ... further arguments passed to \code{\link[airGR]{CreateOptions}}.
#'
#' @return See \code{\link[airGR]{CreateOptions}}.
#' @export
CreateRunOptions.InputsModel <- function(InputsModel, ...) {

  airGR::CreateRunOptions(FUN_MOD = InputsModel$FUN_MOD,
                          InputsModel = InputsModel,
                          ...)
}
