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
  stopifnot(length(current_node) == 1)
  current_down_node <- x$down[x$id == current_node]
  if (all(is.na(current_down_node))) return(FALSE)
  current_down_node <- current_down_node[!is.na(current_down_node)]
  if (any(current_down_node == candidate_node)) return(TRUE)
  return(any(sapply(current_down_node, function(cdn) isNodeDownstream(x, cdn, candidate_node))))
}

#' Reduce the size of a GRiwrm by selecting the subset of nodes corresponding to a downstream node
#'
#' @param griwrm A *GRiwrm* object (See [CreateGRiwrm])
#' @param down_node The ID of the downstream node of the reduced *GRiwrm*
#' @param check [logical] Check the consistency of the reduced *GRiwrm*
#'
#' @return A *GRiwrm* object only containing nodes located upstream the given downstream node
#' @export
#'
#' @examples
#' data(Severn)
#' nodes <- Severn$BasinsInfo
#' nodes$model <- "RunModel_GR4J"
#' str(nodes)
#' # Mismatch column names are renamed to stick with GRiwrm requirements
#' rename_columns <- list(id = "gauge_id",
#'                        down = "downstream_id",
#'                        length = "distance_downstream")
#' griwrm_severn <- CreateGRiwrm(nodes, rename_columns)
#' griwrm_severn
#' # Network diagram with upstream basin nodes in blue, intermediate sub-basin in green
#' plot(griwrm_severn)
#' plot(reduceGRiwrm(griwrm_severn, "54032"))
#'
reduceGRiwrm <- function(griwrm, down_node, check = FALSE) {
  visited <- c()
  to_visit <- down_node

  while (length(to_visit) > 0) {
    current_node <- to_visit[1]

    visited <- c(visited, current_node)

    to_visit <- to_visit[-1]

    upstream_nodes <- griwrm$id[!is.na(griwrm$down) & griwrm$down == current_node]

    upstream_nodes <- upstream_nodes[!upstream_nodes %in% visited]
    to_visit <- unique(c(upstream_nodes, to_visit))
  }

  subgriwrm <- griwrm[griwrm$id %in% visited, ]
  subgriwrm[subgriwrm$id == down_node, c("down", "length")] <- NA
  subgriwrm[!subgriwrm$down %in% subgriwrm$id, c("down", "length")] <- NA

  if (check) subgriwrm <- CreateGRiwrm(subgriwrm, keep_all = TRUE)

  return(subgriwrm)
}


#' @export
#' @rdname isNodeDownstream
isNodeUpstream <- function(x, current_node, candidate_node) {
  UseMethod("isNodeUpstream", x)
}

#' @export
#' @rdname isNodeDownstream
isNodeUpstream.GRiwrm <- function(x, current_node, candidate_node) {
  if (candidate_node == current_node) return (FALSE)
  g <- reduceGRiwrm(x, current_node)
  return(candidate_node %in% g$id)
}

#' @export
#' @rdname isNodeDownstream
isNodeUpstream.GRiwrmInputsModel <- function(x, current_node, candidate_node) {
  isNodeUpstream(attr(x, "GRiwrm"), current_node, candidate_node)
}

#' Extract sub-network for calibration with ungauged nodes
#'
#' @inheritParams getNodeProperties
#' @param GaugedId [character], the Id of the downstream gauged node in the
#' ungauged cluster of sub-basins
#'
#' @return A [data.frame] of selected rows in `griwrm`.
#' @noRd
#'
getUngaugedCluster <- function(griwrm, GaugedId) {
  ### Set the reduced network of the basin containing ungauged nodes ###
  # Select nodes identified with the current node as donor gauged node
  g2 <- griwrm[getDiversionRows(griwrm, TRUE), ] # Remove duplicated by Diversions
  donorIds <- g2$id[!is.na(g2$donor) & g2$donor == GaugedId]
  # Remove receiver nodes that haven't GaugedId as downstream node
  donorIds <- c(
    GaugedId,
    donorIds[sapply(donorIds, function(x) isNodeDownstream(griwrm, x, GaugedId))]
  )
  gDonor <- griwrm %>% dplyr::filter(.data$id %in% donorIds)
  # Add upstream nodes for routing upstream flows
  upNodes <- griwrm %>%
    dplyr::filter(.data$down %in% gDonor$id,
                  !.data$id %in% gDonor$id) %>%
    dplyr::mutate(model = ifelse(!is.na(.data$model), NA, .data$model))
  upIds <- upNodes$id
  g <- rbind(upNodes, gDonor)
  class(g) <- c("GRiwrm", class(g))
  attr(g, "upIds") <- upIds
  # Set downstream nodes
  g$down[!g$down %in% g$id] <- NA
  return(g)
}
