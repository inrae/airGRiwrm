#' Generate a network description containing all hydraulic nodes and the description
#' of their connections
#'
#' @details `db` is a [tibble] or a [data.frame] containing at least the columns containing at least:
#'
#'  * the id (column `id`),
#'  * the id and the hydraulic distance to the node downstream (columns `down` and `length`). The last downstream node should have fields `down` and `length` set to `NA`,
#'  * the hydrological model to use if so (column `model`).
#'
#' @param db a [tibble] or a [data.frame] containing the description of the network (See details)
#' @param cols named list or vector for matching columns of `db` parameter. By default, mandatory columns names are: `id`, `down`, `length`. But other names can be handled with a named list or vector containing items defined as `"required name" = "column name in db"`.
#' @param keep_all keep all column of `db` or keep only columns defined in `cols`.
#' @param convertArea calculate area of sub-basin. To use if provided areas correspond to the whole basin areas
#'
#' @return `GRiwrm` class object containing the description of diagram of the semi-distributed catchment model
#' @export
GRiwrm <- function(db,
                   cols = list(
                     id = "id",
                     down = "down",
                     length = "length",
                     model = "model"
                   ),
                   keep_all = FALSE,
                   convertAreas = TRUE) {
  colsDefault <-
    list(
      id = "id",
      down = "down",
      length = "length",
      model = "model",
      area = "area"
    )
  cols <- utils::modifyList(colsDefault, as.list(cols))
  db <- dplyr::rename(db, unlist(cols))
  if (!keep_all) {
    db <- dplyr::select(db, names(cols))
  }
  class(db) <- c("GRiwrm", class(db))
  db
  if (convertAreas) {
    db <- ConvertAreaSD(db)
  }
}

#' Sort the nodes from upstream to downstream.
#'
#' @param griwrm See \code{[GRiwrm]}.
#'
#' @return vector with the ordered node names.
#' @export
getNodeRanking <- function(griwrm) {
  if (!inherits(griwrm, "GRiwrm")) {
    stop("getNodeRanking: griwrm argument should be of class GRiwrm")
  }
  # Remove nodes without model (direct flow connections treated as upstream flows only)
  griwrm <- griwrm[!is.na(griwrm$model), ]
  # Rank 1
  rank <- setdiff(griwrm$id, griwrm$down)
  ranking <- rank
  # Next ranks
  while (any(griwrm$id %in% rank)) {
    rank <- griwrm$down[griwrm$id %in% rank]
    ranking <- c(ranking, rank)
  }
  ranking <- unique(ranking, fromLast = TRUE)
  ranking <- ranking[-length(ranking)]
}

ConvertAreaSD <- function(griwrm) {
  griwrm$area <- sapply(1:nrow(griwrm),
                        function(x) {
                          griwrm$area[x] - sum(griwrm$area[!is.na(griwrm$down) &
                                                             griwrm$id[x] == griwrm$down])
                        })
  return(griwrm)
}
