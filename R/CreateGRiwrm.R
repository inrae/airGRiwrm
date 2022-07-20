#' Generation of a network description containing all hydraulic nodes and the
#' description of their connections
#'
#' @details `db` is a [data.frame] which at least contains in its columns:
#'
#'  * a node identifier (column `id`),
#'  * the identifier and the hydraulic distance to the downstream node
#'  ([character] columns `down` and [numeric] columns `length` in km). The
#'  last downstream node should have fields `down` and `length` set to `NA`,
#'  * the area of the basin ([numeric] column `area` in km2)
#'  * the hydrological model to use or [NA] for using observed flow instead of a
#'  runoff model output ([character] column `model`)
#'
#' @param db [data.frame] description of the network (See details)
#' @param cols [list] or [vector] columns of `db`. By default, mandatory column
#' names are: `id`, `down`, `length`, `area` and `model`. Other names can be
#' handled with a named list or vector containing items defined as `"required
#' name" = "column name in db"` (See details)
#' @param keep_all [logical] indicating if all columns of `db` should be kept
#' or if only columns defined in `cols` should be kept
#'
#' @return [data.frame] of class `GRiwrm` describing the airGR semi-distributed
#' model network, with each line corresponding to a location on the river
#' network and with the following columns:
#'  * `id` ([character]): node identifier
#'  * `down` ([character]): identifier of the node downstream of the current
#'  node ([NA] for the most downstream node)
#'  * `length` ([numeric]): hydraulic distance to the downstream node in km
#'  ([NA] for the most downstream node)
#'  * `area` ([numeric]): total area of the basin starting from the current
#'  node location in km2
#'  * `model` ([character]): hydrological model to use ([NA] for using observed
#'  flow instead of a runoff model output)
#'
#' @aliases GRiwrm
#' @export
#' @inherit RunModel.GRiwrmInputsModel return examples
#'
CreateGRiwrm <- function(db,
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
  griwrm <- dplyr::rename(db, unlist(cols))
  if (!keep_all) {
    griwrm <- dplyr::select(griwrm, names(cols))
  }
  CheckColumnTypes(griwrm,
                   list(id = "character",
                        down = "character",
                        length = "double",
                        model = "character",
                        area = "double"),
                   keep_all)
  checkNetworkConsistency(griwrm)
  griwrm$gauged <- sapply(griwrm$id, getGaugedId, griwrm = griwrm)
  class(griwrm) <- c("GRiwrm", class(griwrm))
  griwrm
}


#' Check of the column types of a [data.frame]
#'
#' @param df [data.frame] to check
#' @param coltypes named [list] with the name of the columns to check as key and
#' the required type as value
#' @param keep_all [logical] if `df` contains extra columns
#'
#' @return [NULL] or error message if a wrong type is detected
#'
#' @examples
#' CheckColumnTypes(
#'   data.frame(string = c("A"), numeric = c(1), stringsAsFactors = FALSE),
#'   list(string = "character", numeric = "double")
#' )
#' @noRd
CheckColumnTypes <- function(df, coltypes, keep_all) {
  lapply(names(df), function(x) {
    if (x %in% names(coltypes)) {
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
    }
  })
  return(NULL)
}


#' Sorting of the nodes from upstream to downstream
#'
#' @param griwrm \[object of class `GRiwrm`\] see [CreateGRiwrm] for details
#'
#' @return [numeric] ordered node names
#' @noRd
getNodeRanking <- function(griwrm) {
  if (!inherits(griwrm, "GRiwrm")) {
    stop("getNodeRanking: griwrm argument should be of class GRiwrm")
  }
  # Remove upstream nodes without model (direct flow connections)
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
  # Remove intermediate nodes without model (direct flow connections)
  ranking <- ranking[ranking %in% griwrm$id]
  return(ranking)
}


checkNetworkConsistency <- function(db) {
  if(sum(is.na(db$down)) != 1 | sum(is.na(db$length)) != 1) {
    stop("One and only one node must have 'NA' in columns 'down' and 'length")
  }
  if(which(is.na(db$down)) != which(is.na(db$length))) {
    stop("The node with 'down = NA' must be the same as the one with 'length = NA'")
  }
  sapply(db$down[!is.na(db$down)], function(x) {
    if(!(x %in% db$id)) {
      stop("The 'down' id ", x, " is not found in the 'id' column")
    }
  })
}


#' Get the Id of the gauged model
#'
#' @param id [character] Id of the current node
#' @param griwrm See [CreateGRiwrm])
#'
#' @return [character] Id of the first node with a model
#'
#' @noRd
getGaugedId <- function(id, griwrm) {
  if(!is.na(griwrm$model[griwrm$id == id]) & griwrm$model[griwrm$id == id] != "Ungauged") {
    return(id)
  } else if(!is.na(griwrm$down[griwrm$id == id])){
    return(getGaugedId(griwrm$down[griwrm$id == id], griwrm))
  } else {
    stop("The model of the downstream node of a network cannot be `NA` or \"Ungauged\"")
  }
}

