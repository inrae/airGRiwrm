#' RunModel function for both airGR and GriwrmInputsModel object
#'
#' @param InputsModel
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
RunModel <- function(InputsModel, ...) {
  UseMethod("RunModel", InputsModel)
}
