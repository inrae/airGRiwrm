#' Function which creates screen plots giving an overview of the model outputs in the GRiwrm network
#'
#' @details
#' For examples of use see topics [RunModel.GRiwrmInputsModel], [RunModel_Reservoir],
#' and [RunModel.Supervisor].
#'
#' The simulated discharge of a node integrates the flows of the whole
#' catchment of the node, while the meteorological time series stored in the
#' *OutputsModel* of the node (precipitation and potential evapotranspiration)
#' come from the meteorological data of its local sub-basin only. Therefore,
#' when these meteorological time series are available in the
#' *GRiwrmOutputsModel* object (i.e. requested in `Outputs_Sim`, see
#' [CreateRunOptions]), they are replaced on the fly, without modifying the
#' object, by their area-weighted mean over the whole catchment of the node
#' (the node and all its upstream nodes), so that the displayed meteorology
#' reflects the whole catchment (See issue inrae/airGRiwrm#178).
#'
#' When the ID of a node is shared with a Diversion node (two rows of the
#' network with the same `id`, see [CreateGRiwrm]), the local sub-basin area
#' of the shared ID cannot be computed and the node is simply excluded from
#' the weighted mean (graceful behaviour).
#'
#' @param x \[object of class *GRiwrmOutputsModel*\] see [RunModel.GRiwrmInputsModel] for details
#' @param Qobs (optional) [matrix] time series of observed flows
#'        (for the same time steps than simulated) (mm/time step) with one column
#'        by hydrological model output named with the node ID (See [CreateGRiwrm] for details)
#' @param Vobs (optional) [matrix] time series of observed or targeted storage
#'        (for the same time steps than simulated) \[m3\] with one column
#'        by hydrological model output named with the node ID (See [CreateGRiwrm] for details)
#' @param unit (optional) [character] flows unit ("m3/s" or "mm")
#' @param ... Further arguments for [airGR::plot.OutputsModel] and [plot]
#'
#' @return [list] of plots.
#'
#' @importFrom graphics plot par title
#' @export
#'
#'
plot.GRiwrmOutputsModel <- function(
  x,
  Qobs = NULL,
  Vobs = NULL,
  unit = "m3/s",
  ...
) {
  # Arguments checks
  stopifnot(
    is.null(Qobs) || is.matrix(Qobs) || is.data.frame(Qobs),
    is.character(unit),
    unit %in% c("mm", "m3/s")
  )

  griwrm <- attr(x, "GRiwrm")
  ## define outer margins and a title inside it
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(oma = c(0, 0, 3, 0))

  lapply(
    names(x),
    function(id) {
      Qobs_id <- NULL
      if (!is.null(Qobs)) {
        if (id %in% colnames(Qobs)) {
          Qobs_id <- Qobs[, id, drop = TRUE]
        } else {
          warning("Column \"", id, "\" not found in Qobs")
        }
      }
      Vobs_id <- NULL
      if (!is.null(Vobs)) {
        if (id %in% colnames(Vobs)) {
          Vobs_id <- Vobs[, id, drop = TRUE]
        }
      }

      # Display the meteorological data at the whole-catchment scale (#178).
      # A copy of the OutputsModel of the node is adjusted on the fly, the
      # original object is not modified.
      x_id <- adjustMeteoToCatchment(x[[id]], id, x, griwrm)

      BasinArea <- griwrm$area[
        griwrm$id == id & !is.na(griwrm$model) & griwrm$model != "Diversion"
      ]
      if (unit == "m3/s" && length(BasinArea) == 1 && !is.na(BasinArea)) {
        plot(
          x_id,
          Qobs = Qobs_id,
          Vobs = Vobs_id,
          BasinArea = BasinArea,
          ...
        )
      } else {
        plot(x_id, Qobs = Qobs_id, Vobs = Vobs_id, ...)
      }
      title(main = id, outer = TRUE, line = 1.2, cex.main = 1.4)
    }
  )
  invisible(NULL)
}

#' Replace, on a copy, the meteorological data of a node by their
#' whole-catchment mean
#'
#' The meteorological time series of the node (`Precip` and `PotEvap`) are
#' replaced, on a copy of its *OutputsModel* object, by their area-weighted
#' mean over the whole catchment of the node (see `catchmentMeteoMean`). The
#' original object is not modified. Time series that are not available in the
#' object are left unchanged.
#'
#' @param OM_id *OutputsModel* object of the node `id`
#' @param id [character] the ID of the node
#' @param OutputsModel *GRiwrmOutputsModel* object (or a list of *OutputsModel*
#'   objects, one by node)
#' @param griwrm A *GRiwrm* object (See [CreateGRiwrm])
#'
#' @return A copy of `OM_id` with the meteorological data at the
#'   whole-catchment scale.
#' @noRd
#'
adjustMeteoToCatchment <- function(OM_id, id, OutputsModel, griwrm) {
  nTS <- length(OM_id$DatesR)
  for (field in c("Precip", "PotEvap")) {
    if (is.null(OM_id[[field]])) {
      next
    }
    # Nodes with an available time series of the same length
    ids <- names(OutputsModel)[
      sapply(names(OutputsModel), function(i) {
        !is.null(OutputsModel[[i]][[field]]) &&
          length(OutputsModel[[i]][[field]]) == nTS
      })
    ]
    if (!id %in% ids) {
      next
    }
    meteo <- do.call(
      cbind,
      lapply(ids, function(i) OutputsModel[[i]][[field]])
    )
    colnames(meteo) <- ids
    OM_id[[field]] <- catchmentMeteoMean(griwrm, id, meteo)
  }
  return(OM_id)
}

#' Compute the area-weighted mean of a meteorological time series at the
#' whole-catchment scale
#'
#' For a given node, the area-weighted mean is computed over the whole
#' catchment of the node (the node and all its upstream nodes). Each node
#' contributes with its own time series weighted by the area of its local
#' sub-basin (see `getNodeLocalArea`), which is the part of the catchment for
#' which the meteorological data of the node are the "real" data.
#'
#' @param griwrm A *GRiwrm* object (See [CreateGRiwrm])
#' @param id [character] the ID of the node for which the whole-catchment
#'   mean is computed
#' @param meteo [matrix] of meteorological data with one row by time step and
#'   one column by node ID
#'
#' @return A [numeric] vector of the area-weighted mean of the meteorological
#'   data over the whole catchment of the node `id`. A node cannot contribute
#'   to the mean when its local sub-basin area is unknown or zero, or when its
#'   time series is not available; a missing value in a time series excludes
#'   the node from the mean at the concerned time steps. When no node can
#'   contribute, the time series of the node `id` itself is returned unchanged
#'   (graceful behaviour).
#' @noRd
#'
catchmentMeteoMean <- function(griwrm, id, meteo) {
  # Nodes of the whole catchment of `id` (the node and all its upstream
  # nodes) with an available time series
  ids <- reduceGRiwrm(griwrm, id)$id
  ids <- ids[ids %in% colnames(meteo)]
  # Area of the local sub-basin of each node
  areas <- sapply(ids, getNodeLocalArea, griwrm = griwrm)
  # Nodes that can contribute to the weighted mean: a known local sub-basin
  # area greater than zero
  Contributing <- !is.na(areas) & areas > 0
  if (!any(Contributing)) {
    # Not enough information to compute the mean: fall back to the
    # meteorological data of the node `id` itself
    if (id %in% colnames(meteo)) {
      return(meteo[, id, drop = TRUE])
    }
    return(rep(NA_real_, nrow(meteo)))
  }
  ContributingIds <- ids[Contributing]
  meteo <- meteo[, ContributingIds, drop = FALSE]
  w <- unname(areas[Contributing])
  # Weights applied column by node (sweep, not "meteo * w" which would
  # recycle the weights element-wise over the whole matrix)
  Weighted <- sweep(meteo, 2, w, FUN = `*`)
  # Missing values: the node does not contribute at the concerned time steps
  Numerator <- rowSums(Weighted, na.rm = TRUE)
  Denominator <- rowSums(sweep(!is.na(Weighted), 2, w, FUN = `*`))
  Mean <- Numerator / Denominator
  Mean[Denominator == 0] <- NA_real_
  return(Mean)
}
