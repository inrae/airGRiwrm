#' Wrapper for \code{\link[airGR]{CreateRunOptions}} for one sub-basin.
#'
#' @inherit airGR::CreateRunOptions
#' @export
CreateRunOptions.InputsModel <- function(InputsModel, ...) {

  airGR::CreateRunOptions(FUN_MOD = InputsModel$FUN_MOD,
                          InputsModel = InputsModel,
                          ...)
}
