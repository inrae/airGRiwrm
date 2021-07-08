#' @rdname CreateCalibOptions
#' @export
CreateCalibOptions.InputsModel <- function(InputsModel,
                               ...) {
  airGR::CreateCalibOptions(
    FUN_MOD = InputsModel$FUN_MOD,
    IsSD = !is.null(InputsModel$Qupstream),
    ...
  )
}
