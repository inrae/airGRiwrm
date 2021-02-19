#' Create \emph{GRiwrmInputsCrit} object for **airGRiwrm**.
#'
#' This function does the same operations as [airGR::CreateInputsCrit] for all sub-basins of the GRiwrm model.
#'
#' @param InputsModel  object of class \emph{GRiwrmInputsModel}, see \code{\link{CreateInputsModel.GRiwrm}} for details.
#' @param FUN_CRIT \[function (atomic or list)\] error criterion function (e.g. \code{\link[airGR]{ErrorCrit_RMSE}}, \code{\link[airGR]{ErrorCrit_NSE}})
#' @param RunOptions object of class \emph{GRiwrmRunOptions}, see \code{[CreateRunOptions.GRiwrm]} for details.
#' @param Qobs matrix or data frame containing observed flows. Column names correspond to nodes ID
#' @param ... further arguments passed to [airGR::CreateInputsCrit].
#'
#' @return Object of class \emph{GRiwrmInputsCrit} which is a list of `airGR::InputsCrit` objects (See [airGR::CreateInputsCrit])
#' @export
CreateInputsCrit.GRiwrmInputsModel <- function(InputsModel,
                                               FUN_CRIT = airGR::ErrorCrit_NSE,
                                               RunOptions,
                                               Qobs,
                                               ...) {
  InputsCrit <- list()
  class(InputsCrit) <- append(class(InputsCrit), "GRiwrmInputsCrit")

  for(IM in InputsModel) {
    InputsCrit[[IM$id]] <- CreateInputsCrit.InputsModel(
      InputsModel = IM,
      FUN_CRIT = FUN_CRIT,
      RunOptions = RunOptions[[IM$id]],
      Obs = Qobs[RunOptions[[IM$id]]$IndPeriod_Run, IM$id],
      ...
    )
  }

  return(InputsCrit)
}
