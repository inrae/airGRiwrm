#' Create \emph{GRiwrmRunOptions} object for running and calibrating model in **airGRiwrm**.
#'
#' @param InputsModel object of class \emph{GRiwrmInputsModel}, see [CreateInputsModel.GRiwrm] for details.
#' @param ... further arguments passed to [airGR::CreateRunOptions].
#'
#' @return \emph{GRiwrmRunOptions} object for running and calibrating model in **airGRiwrm**.
#' @export
#' @inherit RunModel.GRiwrmInputsModel return examples
#'
CreateRunOptions.GRiwrmInputsModel <- function(InputsModel, ...) {

  RunOptions <- list()
  class(RunOptions) <- append(class(RunOptions), "GRiwrmRunOptions")

  for(InputsModelBasin in InputsModel) {
    RunOptions[[InputsModelBasin$id]] <- CreateRunOptions(InputsModel = InputsModelBasin, ...)
  }
  return(RunOptions)
}
