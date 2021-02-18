#' Id of sub-basins using SD model
#'
#' @param InputsModel `GRiwrmInputsModel` object
#'
#' @return [character] IDs of the sub-basins using SD model
#'
getSD_Ids <- function(InputsModel) {
  if(!inherits(InputsModel, "GRiwrmInputsModel")) {
    stop("Argument `InputsModel` should be of class GRiwrmInputsModel")
  }
  bSDs <- sapply(InputsModel, function (IM) {
    inherits(IM, "SD")
  })
  names(InputsModel)[bSDs]
}
