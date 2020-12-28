#' Wrapper for \code{\link[airGR]{CreateRunOptions}} for one sub-basin.
#'
#' @param InputsModel object of class \emph{InputsModel}, see \code{\link[airGR]{CreateInputsModel}} for details.
#' @param ... Arguments passed to \code{\link[airGR]{CreateRunOptions}}
#' @export
CreateRunOptions.InputsModel <- function(InputsModel, ...) {

  airGR::CreateRunOptions(FUN_MOD = InputsModel$FUN_MOD,
                          InputsModel = InputsModel,
                          ...)
}
