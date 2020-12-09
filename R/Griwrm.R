#' Generate a network description containing all hydraulic nodes and the description
#' of their connections
#'
#' @param db A tibble or a data frame containing at least columns with the id (column `id`), the id and the hydraulic distance to the node downstream (columns `down` and `length`) and a boolean determining whether the node is a rainfall run-off model or not (column `runoff`). The last downstream node should have fields `down` and `length` set to `NA`.
#' @param cols named list or vector for matching columns of `db` parameter. By default, mandatory columns names are: `id`, `down`, `length`. But other names can be handled with a named list or vector containing items defined as `"required name" = "column name in db"`.
#' @param keep_all Keep all column of `db` or keep only columns defined in `cols`
#'
#' @return `Griwrm` class object containing the description of diagram of the semi-distributed catchment model
#' @export
Griwrm <- function(db, cols = list(id = "id", down = "down", length = "length", model = "model"), keep_all = FALSE) {
  colsDefault <- list(id = "id", down = "down", length = "length", model = "model", area = "area")
  cols <- utils::modifyList(colsDefault, as.list(cols))
  db <- dplyr::rename(db, unlist(cols))
  if(!keep_all) {
    db <- dplyr::select(db, names(cols))
  }
  class(db) <- append(class(db), c("Griwrm", "Griwrm"))
  db
}

#' Sort the nodes from upstream to downstream.
#'
#' @param griwrm See \code{[Griwrm]}.
#'
#' @return vector with the ordered node names.
#' @export
getNodeRanking <- function(griwrm) {
  if(!inherits(griwrm, "Griwrm")) {
    stop("getNodeRanking: griwrm argument should be of class Griwrm")
  }
  # Remove nodes without model (direct flow connections treated as upstream flows only)
  griwrm <- griwrm[!is.na(griwrm$model),]
  # Rank 1
  rank <- setdiff(griwrm$id, griwrm$down)
  ranking <- rank
  # Next ranks
  while(any(griwrm$id %in% rank)) {
    rank <- griwrm$down[griwrm$id %in% rank]
    ranking <- c(ranking, rank)
  }
  ranking <- unique(ranking, fromLast = TRUE)
  ranking <- ranking[-length(ranking)]
}
