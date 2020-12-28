#' Wrapper to \code{\link[airGR]{CreateCalibOptions}} for one sub-basin.
#'
#' @param InputsModel object of class \emph{InputsModel}. See \code{\link[airGR]{CreateInputsModel}} for details
#' @param ... Arguments passed to \code{\link[airGR]{CreateCalibOptions}}
#' @export
CreateCalibOptions.InputsModel <- function(InputsModel,
                               ...) {
  airGR::CreateCalibOptions(
    FUN_MOD = InputsModel$FUN_MOD,
    IsSD = !is.null(InputsModel$Qupstream),
    ...
  )
}
