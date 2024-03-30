#' Function to obtain the ID of sub-basins using SD model
#'
#' @param InputsModel \[`GRiwrmInputsModel` object\]
#' @param add_diversions [logical] for adding upstream nodes with diversion
#'
#' @return [character] IDs of the sub-basins using SD model
#' @export
getSD_Ids <- function(InputsModel, add_diversions = FALSE) {
  if (!inherits(InputsModel, "GRiwrmInputsModel")) {
    stop("Argument `InputsModel` should be of class GRiwrmInputsModel")
  }
  bSDs <- sapply(InputsModel, function (IM) {
    inherits(IM, "SD") || (add_diversions & IM$hasDiversion)
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


#' Check if a node is downstream or upstream another one
#'
#' @param x \[`GRiwrmInputsModel` object\] (see [CreateInputsModel.GRiwrm]) or
#'        \[`GRiwrm` object\] (See [CreateGRiwrm])
#' @param current_node [character] with the id of the current node
#' @param candidate_node [character] with the id of the node for which we want
#'        to know if it is downstream or upstream `current_node`
#'
#' @return [logical] `TRUE` if the node with the id `down_candidate` is downstream
#'         or upstream the node with the id `current_node`
#' @export
#' @rdname isNodeDownstream
#'
isNodeDownstream <- function(x, current_node, candidate_node) {
  UseMethod("isNodeDownstream", x)
}

#' @export
#' @rdname isNodeDownstream
isNodeDownstream.GRiwrmInputsModel <- function(x, current_node, candidate_node) {
  isNodeDownstream(attr(x, "GRiwrm"), current_node, candidate_node)
}

#' @export
#' @rdname isNodeDownstream
isNodeDownstream.GRiwrm <- function(x, current_node, candidate_node) {
  current_down_node <- x$down[x$id %in% current_node]
  if (all(is.na(current_down_node))) return(FALSE)
  if (any(current_down_node == candidate_node)) return(TRUE)
  return(isNodeDownstream(x, current_down_node, candidate_node))
}

#' @export
#' @rdname isNodeDownstream
isNodeUpstream <- function(x, current_node, candidate_node) {
  !isNodeDownstream(x, current_node, candidate_node)
}
