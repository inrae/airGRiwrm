#' Create \emph{GRiwrmRunOptions} object for running and calibrating model in GR-IWRM.
#'
#' @param InputsModel object of class \emph{GRiwrmInputsModel}, see \code{\link{CreateInputsModel.GRiwrm}} for details.
#' @param ... further arguments passed to \code{\link[airGR]{CreateRunOptions}}.
#'
#' @return \emph{GRiwrmRunOptions} object for running and calibrating model in GR-IWRM.
#' @export
CreateRunOptions.GRiwrmInputsModel <- function(InputsModel, ...) {

  RunOptions <- list()
  class(RunOptions) <- append(class(RunOptions), "GRiwrmRunOptions")

  for(InputsModelBasin in InputsModel) {
    RunOptions[[InputsModelBasin$id]] <- CreateRunOptions(InputsModel = InputsModelBasin, ...)
  }
  return(RunOptions)
}
