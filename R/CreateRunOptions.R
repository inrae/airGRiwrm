#' Create \emph{RunOptions} object for **airGR** and **airGRiwrm**.
#'
#' See [airGR::CreateRunOptions] and [CreateRunOptions.GRiwrmInputsModel] for usage.
#'
#' @param InputsModel object of class \emph{InputsModel} (see [airGR::CreateInputsModel]) or \emph{GRiwrmInputsModel} (See [CreateInputsModel.GRiwrm]).
#' @param ... further arguments passed to or from other methods.
#'
#' @return Object of \emph{RunOptions} class family
#' @export
CreateRunOptions <- function(InputsModel, ...) {
  UseMethod("CreateRunOptions", InputsModel)
}
