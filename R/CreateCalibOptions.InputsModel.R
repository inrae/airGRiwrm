#' Wrapper to \code{\link[airGR]{CreateCalibOptions}}
#'
#' @param InputsModel object of class \emph{InputsModel}, see \code{\link[airGR]{CreateInputsModel}} for details.
#' @param ... further arguments passed to \code{\link[airGR]{CreateCalibOptions}}.
#'
#' @return \emph{CalibOptions} object.
#' @export
CreateCalibOptions.InputsModel <- function(InputsModel,
                               ...) {
  airGR::CreateCalibOptions(
    FUN_MOD = InputsModel$FUN_MOD,
    IsSD = !is.null(InputsModel$QobsUpstr),
    ...
  )
}
