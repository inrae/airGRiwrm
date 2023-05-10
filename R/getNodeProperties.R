#' Get list of properties of a GRiwrm network node
#'
#'
#' @param id [character] Id of the node in the GRiwrm object
#' @param griwrm \[GRiwrm object\] describing the network of the semi-distributed model (See [CreateGRiwrm])
#'
#' @return A [list] with the following items:
#' - "position" ([character]): Position of the node in the network ("Upstream" or "Intermediate")
#' - "DirectInjection" ([logical]): is the node a Direct Injection node?
#' - "Diversion" ([logical]): is the node a Diversion node?
#' - "Reservoir" ([logical]): is the node a Reservoir?
#' - "airGR" ([logical]): is the node contains an airGR model?
#' - "calibration" ([character]): describe if the node is a "Gauged", or an "Ungauged" station,
#'   (see details), or "NA" otherwise
#' - "Upstream" ([logical]): is the node an upstream node?
#' - "RunOff" ([logical]): is the node contains an hydrological model?
#'
#' @details
#' A "Gauged" node is either a node containing a model that is already
#' calibrated (parameters are already fixed) or a node containing a model where
#' observations are available for calibration.
#'
#' A "Ungauged" node is a node containing a model which derives its parameters from
#' another "donor" node.
#'
#' @export
#'
#' @example man-examples/getNodeProperties.R
getNodeProperties <- function(id, griwrm) {
  stopifnot(inherits(griwrm, "GRiwrm"),
            "donor" %in% names(griwrm))
  g2 <- griwrm[getDiversionRows(griwrm, TRUE), , drop = FALSE]
  upstreamIds <- griwrm$id[!griwrm$id %in% griwrm$down]
  model <- g2$model[g2$id == id]
  if (is.na(model)) {
    donor_model <- "DirectInjection"
  } else {
    donor_model <- g2$model[g2$id == g2$donor[g2$id == id]]
  }
  p <- list(
    position = ifelse(id %in% upstreamIds, "Upstream", "Intermediate"),
    DirectInjection = is.na(model),
    Diversion = "Diversion" %in% griwrm$model[griwrm$id == id],
    Reservoir = !is.na(model) && model == "RunModel_Reservoir",
    airGR = grepl("RunModel_", donor_model)
  )
  if (p$DirectInjection) {
    p$calibration <- "NA"
  } else {
    p$calibration <- ifelse(isNodeGauged(id, griwrm), "Gauged", "Ungauged")
  }
  p$Upstream <- p$position == "Upstream"
  p$RunOff <- !p$DirectInjection && !p$Reservoir && donor_model != "RunModel_Lag"
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


#' Does this node contains a gauged model?
#'
#' @inheritParams getNodeProperties
#'
#' @return `TRUE` if the node contains a gauged model, `FALSE` if not.
#' @noRd
#'
isNodeGauged <- function(id, griwrm, skip_reservoirs = FALSE) {
  g2 <- griwrm[getDiversionRows(griwrm, inverse = TRUE), , drop = FALSE]
  model <- g2$model[g2$id == id]
  ungaugedModels <- c("Ungauged")
  if (skip_reservoirs) ungaugedModels <- c(ungaugedModels, "RunModel_Reservoir")
  return(!is.na(model) && !model %in% ungaugedModels)
}
