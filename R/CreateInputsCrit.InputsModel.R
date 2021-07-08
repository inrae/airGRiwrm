#' @rdname CreateInputsCrit
#' @export
CreateInputsCrit.InputsModel <- function(InputsModel,
                                         FUN_CRIT,
                                         ...) {
  airGR::CreateInputsCrit(FUN_CRIT = FUN_CRIT,
                          InputsModel = InputsModel,
                          ...)
}
