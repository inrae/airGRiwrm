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
#' An optional column `donor` can be used to manually define which sub-basin
#' will give its parameters to an ungauged node (See `Ungauged` model below).
#'
#' ## Available models in airGRiwrm
#'
#' The "model" column should be filled by one of the following:
#'
#' - One of the hydrological models available in the **airGR** package defined by its
#' `RunModel` function (i.e.: `RunModel_GR4J`, `RunModel_GR5HCemaneige`...)
#' - `RunModel_Reservoir` for simulating a reservoir (See: [RunModel_Reservoir])
#' - `Ungauged` for an ungauged node. The sub-basin inherits hydrological model and
#' parameters from a "donor" sub-basin. If not defined by the user in the column `donor`,
#' the donor is automatically set to the first gauged node at downstream.
#' This set of sub-basins with the same donor downstream then forms an ungauged
#' node cluster that will be calibrated at once.
#' - `NA` for injecting (or abstracting) a flow time series at the location of the node
#' (direct flow injection)
#' - `Diversion` for abstracting a flow time series from an existing node transfer it
#' to another node. As a `Diversion` is attached to an existing node, this node is
#' then described with 2 lines: one for the hydrological model and another one for the
#' diversion
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
#'  * `donor` ([character]): node used as model and calibration parameter "donor" for
#'  ungauged nodes. For other types of nodes, if the donor is different than the
#'  id, it indicates that the node is embedded in an ungauged node cluster.
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
                     area = "area",
                     model = "model",
                     donor = "donor"
                   ),
                   keep_all = FALSE) {

  stopifnot(inherits(db, "data.frame"))

  colsDefault <-
    list(
      id = "id",
      down = "down",
      length = "length",
      area = "area",
      model = "model",
      donor = "donor"
    )
  cols <- utils::modifyList(colsDefault, as.list(cols))

  if (is.null(db[[cols$donor]])) db[[cols$donor]] <- as.character(NA)

  griwrm <- dplyr::rename(db, unlist(cols))

  if (!keep_all) {
    griwrm <- dplyr::select(griwrm, names(cols))
  }

  CheckColumnTypes(griwrm,
                   list(id = "character",
                        down = "character",
                        length = "double",
                        model = "character",
                        area = "double",
                        donor = "character"),
                   keep_all)

  checkNetworkConsistency(griwrm)

  class(griwrm) <- c("GRiwrm", class(griwrm))

  # Set automatic downstream donors for ungauged nodes
  griwrm$donor <- setDonor(griwrm)

  griwrm <- sort(griwrm)

  return(griwrm)
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
      if (coltypes[[x]] == "double" && typeof(df[[x]]) == "integer") {
        df[[x]] <- as.double(df[[x]])
      }
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


checkNetworkConsistency <- function(db) {
  db2 <- db[getDiversionRows(db, TRUE), ]
  if (any(duplicated(db2$id))) {
    stop("Duplicated nodes detected: ",
         paste(db2$id[duplicated(db2$id)], collapse = "\n"),
         "\nNodes `id` must be unique (except for `Diversion` nodes)")
  }
  dbDiv <- db[getDiversionRows(db), ]
  if (any(duplicated(dbDiv$id))) {
    stop("Duplicated Diversion nodes detected: ",
         paste(dbDiv$id[duplicated(dbDiv$id)], collapse = "\n"),
         "\nThere can only be one Diversion per node")
  }
  if (sum(is.na(db$down)) == 0) {
    stop("At least one node must be a network downstream node",
      " specified by 'down = NA'")
  }
  lapply(which(!is.na(db$down)), function(i) {
    node <- db[i, ]
    if (!(node$down %in% db$id)) {
      nodeError(node, "The 'down' id ", node$down, " is not found in the 'id' column")
    }
  })
  db3 <- db2[!is.na(db2$model), ]
  # db3 only GR and Reservoir nodes (no Diversion, no DirectInjection)
  lapply(which(!is.na(db3$donor)), function(i) {
    node <- db3[i, ]
    if (!(node$donor %in% db2$id)) {
      nodeError(node, "The 'donor' id ", node$donor, " is not found in the 'id' column")
    }
    donor_model <- db2$model[db2$id == node$donor]
    if (is.na(donor_model) || donor_model %in% c("RunModel_Reservoir", "Ungauged")) {
      if (!(node$model == "RunModel_Reservoir" &&
            !is.na(donor_model) && donor_model == "RunModel_Reservoir")) {
        # This error is for GR and RunModel_Reservoir that are in an ungauged cluster
        nodeError(node, "The 'donor' node ", node$donor, " must be an hydrological model",
                  " (Found model = '", donor_model, "')")
      }
    }
  })
  sapply(db$id[getDiversionRows(db)], function(x) {
    i <- which(db$id == x & db$model == "Diversion")[1]
    if (length(which(db3$id == x)) != 1) {
      nodeError(db[i, ],
                "A Diversion node must have the same `id` of one (and only one) node with a model")
    }
  })
  id_reservoirs <- db3$id[db3$model == "RunModel_Reservoir"]
  sapply(id_reservoirs, function(id) {
    if (length(db$id[!is.na(db$down) & db$down == id]) == 0) {
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

nodeError <- function(node, ...) {
  stop(displayNodeDetails(node), "\n", ...)
}

#' Get the Id of the nearest gauged model at downstream
#'
#' @param id [character] Id of the current node
#' @param griwrm See [CreateGRiwrm])
#'
#' @return [character] Id of the first node with a model of `FALSE` if not found
#'
#' @noRd
getDonor <- function(id, griwrm) {
  if (isNodeGauged(id, griwrm, skip_reservoirs = TRUE)) {
    # Match with a gauged station!
    return(id)
  } else {
    # Otherwise we need to search downstream on the natural network
    g2 <- griwrm[getDiversionRows(griwrm, TRUE), ]
    node <- g2[g2$id == id, ]
    if (!is.na(node$down)) {
      return(getDonor(node$down, griwrm))
    } else if (length(getDiversionRows(griwrm)) > 0) {
      # Search on Diversion
      g3 <- griwrm[getDiversionRows(griwrm), ]
      node$down <- g3$down[g3$id == id]
      if (!is.na(node$down)) {
        return(getDonor(node$down, griwrm))
      }
    }
  }
  #If we already are at the downstream end, we have a problem...
  return(FALSE)
}

getDiversionRows <- function(griwrm, inverse = FALSE) {

  rows <- which(!is.na(griwrm$model) & griwrm$model == "Diversion")
  if (inverse) {
    if (length(rows) == 0) {
      rows <- seq.int(nrow(griwrm))
    } else {
      rows <- setdiff(seq.int(nrow(griwrm)), rows)
    }
  }
  return(rows)
}

setDonor <- function(griwrm) {
  oDonors <- griwrm$donor
  griwrm$donor <- sapply(seq(nrow(griwrm)), function(i) {
    if (!is.na(griwrm$donor[i])) {
      # Donor set by user
      return(griwrm$donor[i])
    }
    id <- griwrm$id[i]
    model <- griwrm$model[i]
    if (is.na(model) || model == "Diversion") {
      return(as.character(NA))
    }
    if (model != "Ungauged" &&
        (is.na(griwrm$down[i]) || !any(!is.na(griwrm$down) & griwrm$down == id))) {
      # Downstream or upstream gauged nodes can't be in ungauged node cluster
      return(id)
    }
    gaugedId <- getDonor(id, griwrm = griwrm)
    if (gaugedId == FALSE) {
      stop("No Gauged node found downstream the node '", id, "'")
    }
    return(gaugedId)
  })
  donors <- sapply(
    seq(nrow(griwrm)),
    FUN = function(i, g) {
      d <- refineDonor(i, g)
      if (!is.na(d) && (is.na(oDonors[i]) || d != oDonors[i]) && d != g$id[i]) {
        if (g$model[i] == "Ungauged") {
          message("Ungauged node '", g$id[i], "' automatically gets the node '",
                  d, "' as parameter donor")
        } else if (g$model[i] == "RunModel_Reservoir") {
          message("Node '", g$id[i], "' is included in the ungauged node cluster '",
                  d, "'")

        } else {
          warning("Node '", g$id[i], "' is included in the ungauged node cluster '",
                  d, "': it should have fixed parameters at Calibration")
        }
      }
      return(d)
    },
    g = griwrm)
  return(donors)
}

#' Correct donor for gauged nodes inside ungauged node clusters
#'
#' @param i rown number to process in `griwrm`
#' @param griwrm A *GRiwrm* object (See [CreateGRiwrm])
#'
#' @return [character] [vector] of donor ids
#' @noRd
refineDonor <- function(i, g) {
  if (is.na(g$model[i]) || g$model[i] == "Diversion") return(as.character(NA))
  if (g$model[i] == "Ungauged") return(g$donor[i])
  id <- g$id[i]
  if (is.na(g$donor[i])) g$donor[i] <- id
  # Search if the gauged node is in an ungauged node cluster
  # Search all ungauged nodes upstream
  g2 <- g[!is.na(g$model) & g$model != "Diversion", ] # Remove duplicates for node search
  upstreamUngaugedNodes <- sapply(g2$id, function(id2) {
    if (g2$model[g2$id == id2] == "Ungauged" && isNodeUpstream(g, id, id2)) {
      id2
    } else {
      NULL
    }
  })
  upstreamUngaugedNodes <- unlist(
    upstreamUngaugedNodes[!sapply(upstreamUngaugedNodes, is.null)]
  )
  if (!is.null(upstreamUngaugedNodes)) {
    donors <- setNames(g2$donor, g2$id)
    ungaugedDonors <- donors[upstreamUngaugedNodes]
    ungaugedDonors <- setdiff(ungaugedDonors, id)
    if (length(ungaugedDonors) > 0) {
      # Search for donor at downstream (then we are in an ungauged node cluster!)
      ungaugedDonors <- ungaugedDonors[sapply(ungaugedDonors, function(x) {
        isNodeDownstream(g, id, x)
      })]
    }
    if (length(ungaugedDonors) == 1) {
      return(ungaugedDonors)
    } else if (length(ungaugedDonors) > 1) {
      warning("The node '", id, "' is embedded in several ungauged node clusters: '",
           paste(ungaugedDonors, collapse = "', '"), "'\n",
           "Calibration of both ungauged node clusters is impossible")
    }
  }

  # No need to change the pre-defined donor (maybe forced for de Lavenne #157)
  return(g$donor[i])
}
