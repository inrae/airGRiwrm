#' @param IniStates (optional) [numeric] object or [list] of [numeric] object of class \emph{IniStates}, see [airGR::CreateIniStates] for details
#' @rdname CreateRunOptions
#' @export
CreateRunOptions.GRiwrmInputsModel <- function(x, IniStates = NULL, ...) {

  RunOptions <- list()
  class(RunOptions) <- append(class(RunOptions), "GRiwrmRunOptions")

  for(id in names(x)) {
    RunOptions[[id]] <- CreateRunOptions(x[[id]], IniStates = IniStates[[id]], ...)
  }
  return(RunOptions)
}
