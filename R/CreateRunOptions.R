#' Title
#'
#' @param ...
#' @param InputsModel
#'
#' @return
#' @export
#'
#' @examples
CreateRunOptions <- function(InputsModel, ...) {
  UseMethod("CreateRunOptions", InputsModel)
}
