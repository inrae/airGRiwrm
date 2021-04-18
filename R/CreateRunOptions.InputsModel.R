#' Wrapper for [airGR::CreateRunOptions] for one sub-basin.
#'
#' @param InputsModel object of class \emph{InputsModel}, see [airGR::CreateInputsModel] for details.
#' @param ... Arguments passed to [airGR::CreateRunOptions]
#' @export
CreateRunOptions.InputsModel <- function(InputsModel, ...) {

  airGR::CreateRunOptions(FUN_MOD = InputsModel$FUN_MOD,
                          InputsModel = InputsModel,
                          ...)
}
