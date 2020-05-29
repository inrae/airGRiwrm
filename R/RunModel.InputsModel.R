#' Wrapper for \code{\link[airGR]{RunModel}} which performs a single model run with the provided function over the selected period.
#'
#' @param InputsModel object of class \emph{InputsModel}, see \code{\link[airGR]{CreateInputsModel}} for details.
#' @param RunOptions object of class \emph{RunOptions}, see \code{\link[airGR]{CreateRunOptions}} for details.
#' @param Param numeric vector of model parameters.
#' @param FUN_MOD hydrological model function (e.g. \code{\link[airGR]{RunModel_GR4J}}, \code{\link[airGR]{RunModel_CemaNeigeGR4J}}).
#'
#' @return
#' @export
RunModel.InputsModel <- function(InputsModel, RunOptions, Param, FUN_MOD = NULL, ...) {
  if(is.null(FUN_MOD)) {
    FUN_MOD <- InputsModel$FUN_MOD
  }
  airGR::RunModel(InputsModel, RunOptions, Param, FUN_MOD)
}
