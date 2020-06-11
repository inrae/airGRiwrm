#' Wrapper to \code{\link[airGR]{CreateCalibOptions}} for one sub-basin.
#'
#' @inherit airGR::CreateCalibOptions
#' @export
CreateCalibOptions.InputsModel <- function(InputsModel,
                               ...) {
  airGR::CreateCalibOptions(
    FUN_MOD = InputsModel$FUN_MOD,
    IsSD = !is.null(InputsModel$Qupstream),
    ...
  )
}
