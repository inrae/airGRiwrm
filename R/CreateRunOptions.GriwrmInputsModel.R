#' Create \emph{GriwrmRunOptions} object for running and calibrating model in GR-IWRM.
#'
#' @param InputsModel object of class \emph{GriwrmInputsModel}, see \code{\link{CreateInputsModel.Griwrm}} for details.
#' @param ... further arguments passed to \code{\link[airGR]{CreateOptions}}.
#'
#' @return \emph{GriwrmRunOptions} object for running and calibrating model in GR-IWRM.
#' @export
CreateRunOptions.GriwrmInputsModel <- function(InputsModels, ...) {

  RunOptions <- list()
  class(RunOptions) <- append(class(RunOptions), "GriwrmRunOptions")

  for(InputsModelBasin in InputsModels) {
    RunOptions[[InputsModelBasin$id]] <- CreateRunOptions(InputsModel = InputsModelBasin, ...)
  }
  return(RunOptions)
}
