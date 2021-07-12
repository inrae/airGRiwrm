#' @param IniStates (optional) [numeric] object or [list] of [numeric] object of class \emph{IniStates}, see [airGR::CreateIniStates] for details
#' @rdname CreateRunOptions
#' @export
CreateRunOptions.GRiwrmInputsModel <- function(InputsModel, IniStates = NULL, ...) {

  RunOptions <- list()
  class(RunOptions) <- append(class(RunOptions), "GRiwrmRunOptions")

  for(id in names(InputsModel)) {
    RunOptions[[id]] <- CreateRunOptions(InputsModel = InputsModel[[id]], IniStates = IniStates[[id]], ...)
  }
  return(RunOptions)
}
