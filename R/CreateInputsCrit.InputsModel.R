#' Wrapper to \code{\link[airGR]{CreateInputsCrit}}
#'
#' @param InputsModel object of class \emph{InputsModel}, see \code{\link[airGR]{CreateInputsModel}} for details.
#' @param FUN_CRIT \[function (atomic or list)\] error criterion function (e.g. \code{\link[airGR]{ErrorCrit_RMSE}}, \code{\link[airGR]{ErrorCrit_NSE}})
#' @param ... further arguments passed to \code{\link[airGR]{CreateInputsCrit}}
#'
#' @return object of class \emph{InputsCrit} containing the data required to evaluate the model outputs. See \code{\link[airGR]{CreateInputsCrit}}
#' @export
CreateInputsCrit.InputsModel <- function(InputsModel,
                                         FUN_CRIT,
                                         ...) {

  airGR::CreateInputsCrit(FUN_CRIT = FUN_CRIT,
                          InputsModel = InputsModel,
                          ...)
}
