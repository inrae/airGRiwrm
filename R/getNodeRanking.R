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
  r <- c()
  oupIds <- character(0)
  g <- griwrm # GRiwrm with remaining nodes to rank
  while (nrow(g) > 0) {
    l <- getNodeRankingSub(g)
    r <- c(r, l$r)
    g <- l$g
    #Search for ungauged ids
    upIds <- unique(g$id[!is.na(g$donor) & !g$id %in% g$down & g$id != g$donor])
    if (!identical(oupIds, character(0)) && identical(upIds, oupIds)) {
      stop("Inconstancy detected in GRiwrm object: impossible to reach donor of ungauged node(s): '",
           paste(upIds, collapse = "', '"),
           "'")
    }
    oupIds <- upIds
    upDonors <- unique(g$donor[!is.na(g$donor) & g$id %in% upIds])
    if (length(upDonors) == 0) next
    upDonorsRanks <- sapply(
      upDonors,
      function(x) {
        length(which(sapply(upDonors, function(y) isNodeUpstream(g, x, y))))
      }
    )
    upDonors <- names(sort(upDonorsRanks))
    for (upDonor in upDonors) {
      g_cluster <- getUngaugedCluster(griwrm, upDonor)
      upIds_cluster <- attr(g_cluster, "upIds")
      if (any(upIds_cluster %in% g$id)) {
        warning("Ungauged node cluster '", upDonor,
                "': there are nodes located upstream that can't be calibrated: '",
                paste(upIds_cluster[upIds_cluster %in% g$id], collapse = "', '"),
                "'")
      }
      l <- getNodeRankingSub(g_cluster, donor = upDonor)
      if (nrow(l$g) > 0) stop("Error when ranking nodes in ungauged node cluster '",
                              upDonor, "', these nodes can't be ranked: '",
                              paste(l$g$id, collapse = "', '"), "'")
      r <- c(r, l$r)
      g <- g <- g[!g$id %in% l$r, ]
    }
  }
  return(r)
}

getNodeRankingSub <- function(griwrm, donor = NA) {
  r <- c()
  o_r <- r
  # Remove upstream nodes without model (direct flow connections)
  g <- griwrm[!is.na(griwrm$model), ]
  # Search for gauged ids or ungauged with upstream/sibling donor
  repeat {
    upIds <- unique(
      g$id[!g$id %in% g$down & (
        (is.na(donor) & !is.na(g$donor) &
          (g$id == g$donor | !g$donor %in% g$id))
        | (!is.na(donor) & !is.na(g$donor) & g$donor == donor)
      )]
    )
    r <- c(r, upIds)
    g <- g[!g$id %in% upIds, ]
    if (identical(r, o_r)) break
    o_r <- r
  }
  return(list(r = r, g = g))
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
