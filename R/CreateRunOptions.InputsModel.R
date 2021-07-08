#' @rdname CreateRunOptions
#' @export
CreateRunOptions.InputsModel <- function(InputsModel, ...) {

  airGR::CreateRunOptions(FUN_MOD = InputsModel$FUN_MOD,
                          InputsModel = InputsModel,
                          ...)
}
