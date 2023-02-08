#' Get list of properties of a GRiwrm network node
#'
#' @param id [character] Id of the node in the GRiwrm object
#' @param griwrm \[GRiwrm object\] describing the network of the semi-distributed model (See [CreateGRiwrm])
#'
#' @return A [list] with the following items:
#' - "position" ([character]): Position of the node in the network ("Upstream" or "Intermediate")
#' - "calibration" ([character]): describe if the node is a "Gauged", or an "Ungauged" station,
#'   modelled with an hydrological model, or "NA" otherwise
#' - "Upstream" ([logical]): is the node an upstream node?
#' - "DirectInjection" ([logical]): is the node a Direct Injection node?
#' - "Diversion" ([logical]): is the node a Diversion node?
#'
#' @export
#'
#' @example man-examples/getNodeProperties.R
getNodeProperties <- function(id, griwrm) {
  stopifnot(inherits(griwrm, "GRiwrm"))
  g2 <- griwrm[getDiversionRows(griwrm, TRUE), , drop = FALSE]
  upstreamIds <- griwrm$id[!griwrm$id %in% griwrm$down]
  model <- g2$model[g2$id == id]
  p <- list(
    position = ifelse(id %in% upstreamIds, "Upstream", "Intermediate"),
    DirectInjection = is.na(model),
    Diversion = "Diversion" %in% griwrm$model[griwrm$id == id],
    Reservoir = !is.na(model) && model == "RunModel_Reservoir"
  )
  if (p$DirectInjection) {
    p$calibration <- "NA"
  } else if (model == "Ungauged") {
    p$calibration <- "Ungauged"
  } else {
    p$calibration <- "Gauged"
  }
  p$Upstream <- p$position == "Upstream"
  p$RunOff <- !p$DirectInjection && !p$Reservoir
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
