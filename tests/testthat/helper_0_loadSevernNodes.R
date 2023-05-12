#' Load Severn example and format BasinsInfo for use in CreateGRiwrm
#'
#' @return [data.frame] with columns "id", "down", "length", "area"
#' @noRd
#'
#' @examples
#' nodes <- loadNodes
loadSevernNodes <- function() {
  data(Severn)
  nodes <-
    Severn$BasinsInfo[, c("gauge_id", "downstream_id", "distance_downstream", "area")]
  names(nodes) <- c("id", "down", "length", "area")
  nodes$model <- "RunModel_GR4J"
  return(nodes)
}
