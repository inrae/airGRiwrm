#' Generic function for creating `InputsModel` object for either **airGR** or **airGRiwrm**
#'
#' See the methods [CreateInputsModel.GRiwrm] for **airGRiwrm** and [airGR::CreateInputsModel] for **airGR**.
#'
#' @param x First parameter determining which InputsModel object is created
#' @param ... further arguments passed to or from other methods.
#'
#' @return InputsModel or GRiwrmInputsObject object
#' @rdname CreateInputsModel
#' @import airGR
#' @export
CreateInputsModel <- function(x, ...) {
  UseMethod("CreateInputsModel", x)
}

#' @rdname CreateInputsModel
#' @export
CreateInputsModel.default <- function(x,
                                      ...) {

  airGR::CreateInputsModel(FUN_MOD = x, ...)
}
