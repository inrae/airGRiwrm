#' Creation of the \emph{GRiwrmInputsCrit} object for the **airGRiwrm** ErrorCrit functions.
#'
#' This function does the same operations as [airGR::CreateInputsCrit] for all sub-basins of the GRiwrm model. It creates the InputsCrit object required to the ErrorCrit_ functions but for the **airGRiwrm** package. This function is used to define whether the user wants to calculate a single criterion, multiple criteria at the same time, or a composite criterion, which averages several criteria.
#'
#' @param InputsModel \[object of class \emph{GRiwrmInputsModel}\] see [CreateInputsModel.GRiwrm] for details
#' @param FUN_CRIT \[function (atomic or list)\] error criterion function (e.g. [airGR::ErrorCrit_RMSE], [airGR::ErrorCrit_NSE])
#' @param RunOptions \[object of class \emph{GRiwrmRunOptions}\] see [CreateRunOptions.GRiwrmInputsModel] for details
#' @param Obs [matrix] or [data.frame] series of observed flows. Column names must correspond to nodes ID
#' @param ... further arguments passed to [airGR::CreateInputsCrit]
#'
#' @return [list] Object of class \emph{GRiwrmInputsCrit} containing `airGR::InputsCrit` objects (See [airGR::CreateInputsCrit])
#' @export
CreateInputsCrit.GRiwrmInputsModel <- function(InputsModel,
                                               FUN_CRIT = airGR::ErrorCrit_NSE,
                                               RunOptions,
                                               Obs,
                                               ...) {
  InputsCrit <- list()
  class(InputsCrit) <- append(class(InputsCrit), "GRiwrmInputsCrit")

  for(IM in InputsModel) {
    InputsCrit[[IM$id]] <- CreateInputsCrit.InputsModel(
      InputsModel = IM,
      FUN_CRIT = FUN_CRIT,
      RunOptions = RunOptions[[IM$id]],
      Obs = Obs[, IM$id],
      ...
    )
  }

  return(InputsCrit)
}
