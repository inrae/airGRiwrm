#' Creation of the \emph{GRiwrmRunOptions} object for running and calibrating a model in **airGRiwrm**.
#'
#' @param InputsModel \[object of class \emph{GRiwrmInputsModel}\] see [CreateInputsModel.GRiwrm] for details
#' @param ... further arguments passed to [airGR::CreateRunOptions]
#'
#' @return [list] object of class \emph{GRiwrmRunOptions} containing a \emph{RunOptions} (See [airGR::CreateRunoptions]) object by hydrological node
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
