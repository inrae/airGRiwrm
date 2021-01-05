#' Generate a network description containing all hydraulic nodes and the description
#' of their connections
#'
#' @details `db` is a [data.frame] which at least contains in its columns:
#'
#'  * a node identifier (column `id`),
#'  * the identifier and the hydraulic distance to the downstream node ([character] columns `down` and [numeric] columns `length` in meters). The last downstream node should have fields `down` and `length` set to `NA`,
#'  * the area of the basin ([numeric] column `area` in km^2^)
#'  * the hydrological model to use if so ([character] column `model`).
#'
#' @param db a [data.frame] containing the description of the network (See details)
#' @param cols named list or vector for matching columns of `db` parameter. By default, mandatory columns names are: `id`, `down`, `length`. But other names can be handled with a named list or vector containing items defined as `"required name" = "column name in db"`
#' @param keep_all keep all column of `db` or keep only columns defined in `cols`
#'
#' @return `GRiwrm` class object containing the description of diagram of the semi-distributed catchment model
#' @export
GRiwrm <- function(db,
                   cols = list(
                     id = "id",
                     down = "down",
                     length = "length",
                     model = "model",
                     area = "area"
                   ),
                   keep_all = FALSE) {
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
  CheckColumnTypes(db,
                   list(id = "character",
                        down = "character",
                        length = "double",
                        model = "character",
                        area = "double"))
  rownames(db) <- db$id
  class(db) <- c("GRiwrm", class(db))
  db
}

#' Check the column types of a [data.frame]
#'
#' @param df [data.frame] to check
#' @param coltypes named [list] with the name of the columns to check as key and the required type as value
#'
#' @return [NULL] or throw an error if a wrong type is detected.
#' @examples
#' CheckColumnTypes(
#'   data.frame(string = c("A"), numeric = c(1)),
#'   list(string = "character", numeric = "double")
#' )
#'
CheckColumnTypes <- function(df, coltypes) {
  lapply(names(df), function(x) {
    if (typeof(df[[x]]) != coltypes[[x]]) {
      stop(
        sprintf(
          "The '%s' column is of type %s, a column of type %s is required",
          x,
          typeof(df[[x]]),
          coltypes[[x]]
        )
      )
    }
  })
  return(NULL)
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
  griwrm <- griwrm[!is.na(griwrm$model),]
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
