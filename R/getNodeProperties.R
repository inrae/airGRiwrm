#' Get list of properties of a GRiwrm network node
#'
#' @param id [character] Id of the node in the GRiwrm object
#' @param griwrm \[GRiwrm object\] describing the network of the semi-distributed model (See [CreateGRiwrm])
#'
#' @return A [list] with the following items:
#' - "position" ([character]): Position of the node in the network ("Upstream" or "Intermediate")
#' - "hydrology" ([character]): describe if the node is a "Gauged" or an "Ungauged" station
#'   modelled with an hydrological model, or a "DirectionInjection" node
#' - "Upstream" ([logical]): is the node an upstream node?
#' - "DirectInjection" ([logical]): is the node a Direct Injection node?
#' - "Diversion" ([logical]): is the node a Diversion node?
#'
#' @export
#'
#' @example man-examples/getNodeProperties.R
getNodeProperties <- function(id, griwrm) {
  stopifnot(inherits(griwrm, "GRiwrm"))
  g_div <- griwrm[getDiversionRows(griwrm), , drop = FALSE]
  g2 <- griwrm[getDiversionRows(griwrm, TRUE), , drop = FALSE]
  upstreamIds <- griwrm$id[!griwrm$id %in% griwrm$down]
  gaugedIds <- g2$id[!is.na(g2$model) & g2$model != "Ungauged"]
  divertedIds <- g_div$id
  p <- list(
    position = ifelse(id %in% upstreamIds, "Upstream", "Intermediate"),
    hydrology = ifelse(id %in% gaugedIds, "Gauged",
                       ifelse(is.na(g2$model[g2$id == id]),
                              "NA",
                              "Ungauged"))
  )
  p$Upstream <- p$position == "Upstream"
  p$DirectInjection <- is.na(g2$model[g2$id == id])
  p$Diversion <- id %in% divertedIds
  return(p)
}


#' Get the array of the properties of all the nodes in the network
#'
#' @inheritParams getNodeProperties
#'
#' @return A [data.frame] containing one line by node Id and one column by
#' property (See [getNodeProperties] for a complete list of properties)
#' @export
#'
#' @example man-examples/getNodeProperties.R
getAllNodesProperties <- function(griwrm) {
  stopifnot(inherits(griwrm, "GRiwrm"))
  uids <- griwrm$id[getDiversionRows(griwrm, TRUE)]
  nodeProperties <- lapply(uids,
                           getNodeProperties,
                           griwrm = griwrm)
  df <- do.call(rbind, lapply(nodeProperties, dplyr::bind_cols))
  df <- cbind(id = uids, df)
  rownames(df) <- uids
  return(df)
}
