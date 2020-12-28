#' Wrapper to \code{\link[airGR]{CreateInputsCrit}} for one sub-basin.
#'
#' @inherit airGR::CreateInputsCrit
#' @param ... Further arguments for compatibility with S3 method
#' @export
CreateInputsCrit.InputsModel <- function(InputsModel,
                                         FUN_CRIT,
                                         ...) {

  airGR::CreateInputsCrit(FUN_CRIT = FUN_CRIT,
                          InputsModel = InputsModel,
                          ...)
}
