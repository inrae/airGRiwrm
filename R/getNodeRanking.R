#' Sorting of the nodes from upstream to downstream for RunModel and Calibration
#'
#' @param griwrm \[object of class `GRiwrm`\] see [CreateGRiwrm] for details
#'
#' @return A [character] [vector] containing ordered node ids
#' @export
#' @seealso [CreateGRiwrm()]
#' @import dplyr
getNodeRanking <- function(griwrm) {
  if (!inherits(griwrm, "GRiwrm")) {
    stop("getNodeRanking: griwrm argument should be of class GRiwrm")
  }
  # Remove upstream nodes without model (direct flow connections)
  g <- griwrm[!is.na(griwrm$model), ]
  r <- c()
  o_r <- r
  oupIds <- character(0)
  while (nrow(g) > 0) {
    # Search for gauged ids or ungauged with upstream/sibling donor
    repeat {
      upIds <- unique(g$id[!g$id %in% g$down & (g$id == g$donor | !g$donor %in% g$id)])
      r <- c(r, upIds)
      g <- g[!g$id %in% upIds, ]
      if (identical(r, o_r)) break
      o_r <- r
    }
    #Search for ungauged ids
    upIds <- unique(g$id[!g$id %in% g$down & g$id != g$donor])
    if (!identical(oupIds, character(0)) && identical(upIds, oupIds)) {
      stop("Inconstancy detected in GRiwrm object: impossible to reach donor of ungauged node(s): '",
           paste(upIds, collapse = "', '"),
           "'")
    }
    oupIds <- upIds
    while (length(upIds) > 0) {
      upId <- upIds[1]
      #Browse the ungauged sub-network until the donor
      upDonor <- unique(g$donor[g$id == upId])
      cluster_nodes <- g$id[!is.na(g$donor) & g$donor == upDonor]
      g2 <- g[g$id %in% cluster_nodes, ]
      # Check if upstream nodes have already been processed
      immediate_upstream_nodes <- g$id[!is.na(g$down) & g$down %in% g2$id]
      immediate_upstream_nodes <- immediate_upstream_nodes[!immediate_upstream_nodes %in% g2$id]
      if (all(immediate_upstream_nodes %in% r) &&
          (upDonor %in% r || isNodeDownstream(g2, upId, upDonor))) {
        areNodesUpstreamDonor <- sapply(g2$id, function(id) isNodeDownstream(g2, id, upDonor))
        g2 <- g2[upDonor %in% r | g2$id == upDonor | areNodesUpstreamDonor, ]
        g2$donor <- g2$id
        ungaugedIds <- getNodeRanking(g2)
        r <- c(r, ungaugedIds)
        g <- g[!g$id %in% ungaugedIds, ]
        upIds <- upIds[!upIds %in% ungaugedIds]
      } else {
        upIds <- upIds[upIds != upId]
      }
    }
  }
  return(r)
}


#' Sort a GRiwrm network in upstream-downstream order ready for Calibration
#'
#' It Uses [getNodeRanking] for determining the best order for calibration and leaves
#' direct injection nodes at the tail of the list.
#'
#' @param x A *GRiwrm* object (See [CreateGRiwrm])
#' @inheritParams base::sort
#'
#' @return The sorted *GRiwrm* object in upstream-downstream order ready for Calibration
#' @export
#'
sort.GRiwrm <- function(x, decreasing = FALSE, ...) {
  sorted_id <- getNodeRanking(x)
  rank <- unlist(sapply(sorted_id, function(id) which(x$id == id)))
  direct_injection_rows <- which(is.na(x$model))
  if (length(direct_injection_rows) > 0) {
    rank <- c(rank,
              direct_injection_rows)
  }
  x <- x[rank, ]
  return(x)
}
