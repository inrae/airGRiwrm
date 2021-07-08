#' @rdname CreateRunOptions
#' @export
CreateRunOptions.GRiwrmInputsModel <- function(InputsModel, ...) {

  RunOptions <- list()
  class(RunOptions) <- append(class(RunOptions), "GRiwrmRunOptions")

  for(InputsModelBasin in InputsModel) {
    RunOptions[[InputsModelBasin$id]] <- CreateRunOptions(InputsModel = InputsModelBasin, ...)
  }
  return(RunOptions)
}
