#' Create InputsModel object for either airGR or GRIWRM
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
CreateInputsModel <- function(x, ...) {
  UseMethod("CreateInputsModel", x)
}
