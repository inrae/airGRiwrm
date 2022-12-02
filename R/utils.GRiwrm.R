#' Function to obtain the ID of sub-basins using SD model
#'
#' @param InputsModel \[`GRiwrmInputsModel` object\]
#' @param add_diversion [logical] for adding upstream nodes with diversion
#'
#' @return [character] IDs of the sub-basins using SD model
#' @export
getSD_Ids <- function(InputsModel, add_diversion = FALSE) {
  if (!inherits(InputsModel, "GRiwrmInputsModel")) {
    stop("Argument `InputsModel` should be of class GRiwrmInputsModel")
  }
  bSDs <- sapply(InputsModel, function (IM) {
    inherits(IM, "SD") | IM$hasDiversion
  })
  names(InputsModel)[bSDs]
}

#' Function to obtain the ID of sub-basins not using SD model
#'
#' @param InputsModel \[`GRiwrmInputsModel` object\]
#' @param include_diversion [logical] for including diversion nodes
#'
#' @return [character] IDs of the sub-basins not using the SD model
#' @export
getNoSD_Ids <- function(InputsModel, include_diversion = TRUE) {
  if (!inherits(InputsModel, "GRiwrmInputsModel")) {
    stop("Argument `InputsModel` should be of class GRiwrmInputsModel")
  }
  bSDs <- sapply(InputsModel, function (IM) {
    !inherits(IM, "SD") & (include_diversion | !IM$hasDiversion)
  })
  names(InputsModel)[bSDs]
}


#' Check if a node is downstream another one
#'
#' @param InputsModel \[`GRiwrmInputsModel` object\] see [CreateInputsModel.GRiwrm] for details
#' @param current_node [character] with the id of the current node
#' @param down_node [character] with the id of the node for which we want to know if it is downstream `current_node`
#'
#' @return [logical] `TRUE` if the node with the id `down_node` is downstream the node with the id `current_node`
#' @export
#'
isNodeDownstream <- function(InputsModel, current_node, down_node) {
  current_down_node <- InputsModel[[current_node]]$down
  if (is.na(current_down_node)) return(FALSE)
  if (current_down_node == down_node) return(TRUE)
  return(isNodeDownstream(InputsModel, current_down_node, down_node))
}
