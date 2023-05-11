#' Generation of a network description containing all hydraulic nodes and the
#' description of their connections
#'
#' @details `db` is a [data.frame] which at least contains in its columns:
#'
#'  * a node identifier (column `id`),
#'  * the identifier and the hydraulic distance to the downstream node
#'  ([character] columns `down` and [numeric] columns `length` in km). The
#'  last downstream node should have fields `down` and `length` set to `NA`,
#'  * the total area of the basin at the node location ([numeric] column `area` in km2).
#'  Direct injection node can have a null area defined by `NA`
#'  * the model to use ([character] column `model`), see section below for details
#'
#' ## Available models in airGRiwrm
#'
#' The "model" column should be filled by one of the following:
#'
#' * One of the hydrological models available in the *airGR* package defined by its
#' `RunModel` function (i.e.: `RunModel_GR4J`, `RunModel_GR5HCemaneige`...)
#' * `Ungauged` for an ungauged node. The sub-basin inherits hydrological model and
#' parameters from a "donor" sub-basin. By default the donor is the first gauged
#' node at downstream
#' * `NA` for injecting (or abstracting) a flow time series at the location of the node
#' (direct flow injection)
#' * `Diversion` for abstracting a flow time series from an existing node transfer it
#' to another node. As a `Diversion` is attached to an existing node, this node is
#' then described with 2 lines: one for the hydrological model and another one for the
#' diversion
#'
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
#'  * `donor` ([character]): node used as "donor" for the the model and the
#'  calibration parameters.
#'
#' @aliases GRiwrm
#' @export
#' @example man-examples/CreateGRiwrm.R
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
  class(griwrm) <- c("GRiwrm", class(griwrm))
  griwrm$donor <- setDonor(griwrm)
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
  # Remove intermediate nodes without model (direct flow connections)
  ranking <- ranking[ranking %in% griwrm$id]
  return(ranking)
}


checkNetworkConsistency <- function(db) {
  db2 <- db[getDiversionRows(db, TRUE), ]
  if (any(duplicated(db2$id))) {
    stop("Duplicated nodes detected: ",
         paste(db2$id[duplicated(db2$id)], collapse = "\n"),
         "\nNodes `id` must be unique (except for `Diversion` nodes)")
  }
  if (sum(is.na(db$down)) == 0) {
    stop("At least one node must be a network downstream node",
      " specified by 'down = NA'")
  }
  sapply(db$down[!is.na(db$down)], function(x) {
    if (!(x %in% db$id)) {
      stop("The 'down' id ", x, " is not found in the 'id' column")
    }
  })
  db3 <- db2[!is.na(db2$model), ]
  sapply(db$id[getDiversionRows(db)], function(x) {
    i <- which(db$id == x & db$model == "Diversion")[1]
    if (length(which(db3$id == x)) != 1) {
      nodeError(db[i, ],
                "A Diversion node must have the same `id` of one (and only one) node with a model")
    }
  })
  id_reservoirs <- db3$id[db3$model == "RunModel_Reservoir"]
  sapply(id_reservoirs, function(id) {
    if(length(db$id[!is.na(db$down) & db$down == id]) == 0) {
      stop("The reservoir ", id,
           " must have at least one upstream node as inflows.")
    }
  })
  apply(db, 1, checkNode, simplify = FALSE)
}

checkNode <- function(node) {
  node <- as.list(node)
  if (!is.na(node$model)) {
    if (node$model == "Diversion") {
      if (!is.na(node$area)) {
        nodeError(node, "A Diversion node must have its area equal to `NA`")
      }
    } else if (length(grep("RunModel_GR", node$model)) > 0 & is.na(node$area)) {
      # TODO This test should be extended to airGRplus models
      nodeError(node, "A node using an hydrological model must have a numeric area")
    }
  }
  if (is.na(node$down) & !is.na(node$length)) {
    nodeError(node, "A downstream end node defined by `down=NA` must have `length=NA`")
  }
  if (is.na(node$length) & !is.na(node$down)) {
    nodeError(node, "A node with a defined downstream node must have a numeric `length`")
  }
}

displayNodeDetails <- function(node) {
  s <- sapply(names(node), function(x) {
    sprintf("%s: %s", x, node[x])
  })
  paste("Error on the node:",
        paste(s, collapse = "\n"),
        sep = "\n")
}

nodeError <- function(node, s) {
  stop(displayNodeDetails(node), "\n", s)
}

#' Get the Id of the nearest gauged model at downstream
#'
#' @param id [character] Id of the current node
#' @param griwrm See [CreateGRiwrm])
#'
#' @return [character] Id of the first node with a model of `FALSE` if not found
#'
#' @noRd
getGaugedId <- function(id, griwrm) {
  if (isNodeGauged(id, griwrm, skip_reservoirs = TRUE)) {
    # Match with a gauged station!
    return(id)
  } else {
    # Otherwise we need to search downstream on the natural network
    g2 <- griwrm[getDiversionRows(griwrm, TRUE), ]
    id_down <- g2$down[g2$id == id]
    if (!is.na(id_down)) {
      return(getGaugedId(id_down, griwrm))
    } else {
      #If we already are at the downstream end, we have a problem...
      return(FALSE)
    }
  }
}

getDiversionRows <- function(griwrm, inverse = FALSE) {

  rows <- which(!is.na(griwrm$model) & griwrm$model == "Diversion")
  if (inverse) {
    if(length(rows) == 0) {
      rows <- seq.int(nrow(griwrm))
    } else {
      rows <- setdiff(seq.int(nrow(griwrm)), rows)
    }
  }
  return(rows)
}

setDonor <- function(griwrm) {
  sapply(seq(nrow(griwrm)), function(i) {
    id <- griwrm$id[i]
    model <- griwrm$model[i]
    if (is.na(model) || model == "Diversion") {
      # Diversion and Direct injection are "Non Applicable"
      return(NA)
    } else if(model == "RunModel_Reservoir" && is.na(griwrm$down[i])){
      # RunModel_Reservoir needs to be its own "donor" only if at downstream
      # Otherwise we search the first gauged station downstream to allow
      # calibration with ungauged upstream nodes
      return(id)
    }
    gaugedId <- getGaugedId(id, griwrm = griwrm)
    if (gaugedId == FALSE) {
      stop("No Gauged node found downstream the node '", id, "'")
    }
    return(gaugedId)
  })
}
