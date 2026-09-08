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
    Severn$BasinsInfo[, c(
      "gauge_id",
      "downstream_id",
      "distance_downstream",
      "area"
    )]
  names(nodes) <- c("id", "down", "length", "area")
  nodes$model <- "RunModel_GR4J"
  return(nodes)
}

expect_equal_map <- function(x, y) {
  comp <- as.character(waldo::compare(x, y, list_as_map = TRUE))
  expect_equal(comp, character(0))
}

loadUngauged3Nodes <- function() {
  nodes <- loadSevernNodes()
  nodes <- nodes[!nodes$id %in% c("54002", "54057", "54029"), ]
  nodes[nodes$id == "54032", c("down", "length")] <- c(NA, NA)
  nodes$model[nodes$id == "54001"] <- "Ungauged"
  return(nodes)
}
