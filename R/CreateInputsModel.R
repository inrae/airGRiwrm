#' Create InputsModel object for either **airGR** or **airGRiwrm**
#'
#' @param x First parameter determining which InputsModel object is created
#' @param ... further arguments passed to or from other methods.
#'
#' @return InputsModel or GRiwrmInputsObject object
#' @export
CreateInputsModel <- function(x, ...) {
  UseMethod("CreateInputsModel", x)
}
