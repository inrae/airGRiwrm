#' Wrapper to [airGR::CreateCalibOptions] for one sub-basin.
#'
#' @param InputsModel object of class \emph{InputsModel}. See [airGR::CreateInputsModel] for details
#' @param ... Arguments passed to [airGR::CreateCalibOptions]
#' @export
CreateCalibOptions.InputsModel <- function(InputsModel,
                               ...) {
  airGR::CreateCalibOptions(
    FUN_MOD = InputsModel$FUN_MOD,
    IsSD = !is.null(InputsModel$Qupstream),
    ...
  )
}
