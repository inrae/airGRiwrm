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
#' For a CemaNeige node, the same adjustment is applied to the per-layer
#' series of `CemaNeigeLayers` displayed by [airGR::plot.OutputsModel]: the
#' mean air temperature (`Temp`), the solid precipitation (`Psol`) and the
#' snow pack state (`SnowPack`) of each elevation layer are replaced by their
#' area-weighted mean over the whole catchment of the node, as well as the
#' mean over the layers that airGR derives from them. The other displayed
#' series are not adjusted because they are either already at the scale of the
#' whole catchment (the simulated discharge `Qsim`) or simulated at this scale
#' (the actual evapotranspiration `AE`).
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
#' @seealso [adjustMeteoToCatchment()] to compute statistics from the
#'   meteorological data at the whole-catchment scale.
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
      x_id <- adjustMeteoToCatchment(x[[id]], id, x)

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

#' Compute the meteorological data of a node at the whole-catchment scale
#'
#' @description
#' Replace, on a copy, the meteorological time series of a node by their
#' area-weighted mean over the whole catchment of the node (the node and all
#' its upstream nodes). The original object is not modified: the time series
#' of the object passed in `OutputsModel` are read only.
#'
#' @details
#' The meteorological data stored in the *OutputsModel* object of a node come
#' from the meteorological data of its local sub-basin only, whereas the
#' simulated discharge of the node integrates the flows of its whole
#' catchment. This function computes, for each time step, the area-weighted
#' mean of these data over the whole catchment, so that they can be used to
#' compute statistics at the catchment scale (or displayed, see
#' [plot.GRiwrmOutputsModel]).
#'
#' The following time series are replaced when they are available in the
#' *OutputsModel* object of the node:
#' - `Precip` and `PotEvap`;
#' - for a CemaNeige node, the per-layer series `Temp` (mean air temperature),
#'   `Psol` (solid precipitation) and `SnowPack` (snow pack state) stored in
#'   `OM_id$CemaNeigeLayers`, one item by elevation layer (See
#'   [CreateRunOptions]).
#'
#' Each node contributes to the mean with its own time series weighted by the
#' area of its local sub-basin: the part of the catchment for which the
#' meteorological data of the node are the "real" data (See
#' `getNodeLocalArea`). A node cannot contribute when its local sub-basin area
#' is unknown or zero, or when the concerned time series is not available; a
#' missing value in a time series excludes the node from the mean at the
#' concerned time steps. When no node can contribute, the time series of the
#' node `id` itself are returned unchanged (graceful behaviour). When the ID
#' of the node is shared with a Diversion node (two rows of the network with
#' the same `id`, see [CreateGRiwrm]), the local sub-basin area of the shared
#' ID cannot be computed and the node is simply excluded from the weighted
#' mean (graceful behaviour).
#'
#' The elevation layers of the CemaNeige nodes are matched by index (layer `i`
#' of a node with layer `i` of the other nodes) and a node with fewer layers
#' does not contribute to the upper layer indices. The elevation of a given
#' layer index slightly differs from one node to the other: this approximation
#' is assumed. `SnowPack` is a simulated state and `Psol` an output of the
#' snow module: both are adjusted as well, for the consistency of the
#' whole-catchment display of the snow and temperature series.
#'
#' The network is read from the `griwrm` argument, which defaults to the
#' "GRiwrm" attribute set on the *GRiwrmOutputsModel* object by
#' [RunModel.GRiwrmInputsModel]. When `OutputsModel` is a plain [list] without
#' this attribute, the default resolves to `NULL` and `griwrm` must be passed
#' explicitly.
#'
#' @param OM_id \[object of class *OutputsModel*\] the *OutputsModel* object of
#'   the node `id` (see [RunModel.GRiwrmInputsModel])
#' @param id \[character\] the ID of the node (See [CreateGRiwrm])
#' @param OutputsModel \[object of class *GRiwrmOutputsModel*\] the object
#'   produced by [RunModel.GRiwrmInputsModel], or a [list] of *OutputsModel*
#'   objects with one item by node (names are the node IDs). When a simple
#'   [list] is passed, it must carry the "GRiwrm" attribute or `griwrm` must
#'   be provided explicitly (see below)
#' @param griwrm \[object of class *GRiwrm*\] the network of the simulation
#'   (See [CreateGRiwrm]). Defaults to the "GRiwrm" attribute of
#'   `OutputsModel` (set by [RunModel.GRiwrmInputsModel]). When `OutputsModel`
#'   is a [list] which does not carry this attribute, the default resolves to
#'   `NULL` and `griwrm` must be passed explicitly, otherwise the catchment of
#'   the node cannot be identified.
#'
#' @return A copy of `OM_id` with the meteorological data at the
#'   whole-catchment scale.
#'
#' @export
#' @seealso [plot.GRiwrmOutputsModel()], [CreateGRiwrm()]
#'
#' @examples
#' ##############################################################
#' # Meteorological data at the catchment scale of a CemaNeige   #
#' # network: the network used here is a chain of 2 nodes built  #
#' # on the airGR example data set (L0123001)                    #
#' ##############################################################
#' data(L0123001, package = "airGR")
#'
#' # Network: Upstream (360 km2) -> Downstream (720 km2, i.e. a local
#' # sub-basin of 360 km2)
#' griwrm <- CreateGRiwrm(data.frame(
#'   id = c("Upstream", "Downstream"),
#'   down = c("Downstream", NA),
#'   length = c(0, NA),
#'   area = c(360, 720),
#'   model = rep("RunModel_CemaNeigeGR4J", 2)
#' ))
#'
#' # The 2 nodes share the meteorological data of the example data set
#' InputsPrecip <- matrix(BasinObs$P, ncol = 2, nrow = length(BasinObs$P))
#' InputsPotEvap <- matrix(BasinObs$E, ncol = 2, nrow = length(BasinObs$E))
#' InputsTemp <- matrix(BasinObs$T, ncol = 2, nrow = length(BasinObs$T))
#' InputsHypso <- matrix(rep(BasinInfo$HypsoData, 2), ncol = 2)
#' InputsZ <- rep(BasinInfo$HypsoData[51], 2)
#' colnames(InputsPrecip) <- colnames(InputsPotEvap) <- colnames(InputsTemp) <-
#'   colnames(InputsHypso) <- names(InputsZ) <- griwrm$id
#'
#' InputsModel <- CreateInputsModel(
#'   griwrm,
#'   DatesR = BasinObs$DatesR,
#'   Precip = InputsPrecip,
#'   PotEvap = InputsPotEvap,
#'   TempMean = InputsTemp,
#'   ZInputs = InputsZ,
#'   HypsoData = InputsHypso
#' )
#'
#' # Short run period (1 month) for a fast example. `Precip` and `PotEvap` are
#' # requested in `Outputs_Sim` because they are not part of the default
#' # outputs of a CemaNeige node
#' RunOptions <- CreateRunOptions(
#'   InputsModel,
#'   IndPeriod_Run = seq(1, 30),
#'   Outputs_Sim = c("DatesR", "Qsim", "CemaNeigeLayers", "Precip", "PotEvap")
#' )
#'
#' # `Downstream` receives the simulated flow of `Upstream`: its parameter
#' # vector starts with the celerity of the routing model (airGR::RunModel_Lag)
#' ParamCN <- c(
#'   X1 = 408.774, X2 = 2.646, X3 = 131.264, X4 = 1.174,
#'   CNX1 = 0.962, CNX2 = 2.249
#' )
#' Param <- list(Upstream = ParamCN, Downstream = c(1, ParamCN))
#' OutputsModels <- RunModel(InputsModel, RunOptions = RunOptions, Param = Param)
#'
#' # Area-weighted mean of the meteorological data over the whole catchment
#' # of the node `Downstream` (the local sub-basin areas are 360 km2 each, so
#' # the mean is the arithmetic mean of the 2 nodes here)
#' OutputsDownstream <- adjustMeteoToCatchment(
#'   OutputsModels[["Downstream"]],
#'   "Downstream",
#'   OutputsModels
#' )
#'
#' # Precipitation at the whole-catchment scale and per-layer temperature
#' head(OutputsDownstream$Precip)
#' head(OutputsDownstream$CemaNeigeLayers[[1]]$Temp)
adjustMeteoToCatchment <- function(
  OM_id,
  id,
  OutputsModel,
  griwrm = attr(OutputsModel, "GRiwrm")
) {
  nTS <- length(OM_id$DatesR)
  # Adjust only when at least one time series is really replaced, so that the
  # object of a node without upstream node (or without any available
  # contribution) is returned unchanged.
  Adjusted <- FALSE
  for (field in c("Precip", "PotEvap")) {
    if (is.null(OM_id[[field]])) {
      next
    }
    # Nodes that can contribute to the mean of this field
    ids <- ContributingIds(OutputsModel, griwrm, id, nTS, field)
    if (!id %in% ids || (length(ids) == 1 && ids == id)) {
      next
    }
    meteo <- do.call(
      cbind,
      lapply(ids, function(i) OutputsModel[[i]][[field]])
    )
    colnames(meteo) <- ids
    OM_id[[field]] <- catchmentMeteoMean(griwrm, id, meteo)
    Adjusted <- TRUE
  }
  # CemaNeige models store the layer series in OM_id$CemaNeigeLayers, one item
  # by elevation layer (See CreateRunOptions). The series listed below are the
  # ones displayed by airGR::plot.OutputsModel at the layer scale:
  #   - "Temp": mean air temperature of the layer
  #   - "Psol": solid precipitation of the layer
  #   - "SnowPack": snow pack state of the layer
  # They are similarly replaced by their area-weighted mean over the catchment
  # of the node so that the displayed snow and temperature series - including
  # their mean over the layers computed by airGR - are consistent with the
  # whole catchment of the node. Like precipitations, "SnowPack" (a simulated
  # state) and "Psol" (an output of the snow module) come from the local
  # sub-basin data of each node: they are adjusted here for display
  # consistency.
  # Layers are matched by index (layer i of a node with layer i of the other
  # nodes): the elevation of a given layer index differs slightly from one
  # node to the other, this approximation is assumed.
  # The fields are detected by presence, as airGR does, so that a partial set
  # of layer series is supported.
  if (!is.null(OM_id$CemaNeigeLayers) && length(OM_id$CemaNeigeLayers) > 0) {
    CemaNeigeFields <- c("Temp", "SnowPack", "Psol")
    CemaNeigeFields <- CemaNeigeFields[
      sapply(CemaNeigeFields, function(field) {
        !is.null(OM_id$CemaNeigeLayers[[1]][[field]])
      })
    ]
    for (field in CemaNeigeFields) {
      for (i in seq_along(OM_id$CemaNeigeLayers)) {
        # Nodes with layer `i` contributing to the mean of this field
        ids <- ContributingIds(OutputsModel, griwrm, id, nTS, field, iLayer = i)
        if (!id %in% ids || (length(ids) == 1 && ids == id)) {
          next
        }
        meteo <- do.call(
          cbind,
          lapply(ids, function(k) {
            OutputsModel[[k]]$CemaNeigeLayers[[i]][[field]]
          })
        )
        colnames(meteo) <- ids
        OM_id$CemaNeigeLayers[[i]][[field]] <- catchmentMeteoMean(
          griwrm,
          id,
          meteo
        )
        Adjusted <- TRUE
      }
    }
  }
  if (!Adjusted) {
    # No time series to adjust (no contributor or no meteo data in the
    # object): the object of the node is returned unchanged (graceful
    # behaviour)
    return(OM_id)
  }
  return(OM_id)
}

#' Get the IDs of the nodes of a *GRiwrmOutputsModel* object carrying an
#' available time series
#'
#' @param OutputsModel *GRiwrmOutputsModel* object (or a list of *OutputsModel*
#'   objects, one by node)
#' @param nTS [numeric] number of time steps of the simulation
#' @param field [character] name of the time series
#' @param iLayer [numeric] index of the elevation layer for a CemaNeige layer
#'   series (default: the time series is read at the root of the *OutputsModel*
#'   object of the node)
#'
#' @return [character] vector of the IDs of the nodes carrying the time series
#'   `field` with exactly `nTS` time steps. A node cannot contribute when it
#'   does not carry the field or the elevation layer, or when the length of the
#'   time series differs from `nTS` (graceful behaviour: such a node is simply
#'   excluded).
#' @noRd
#'
OutputsModelIds <- function(OutputsModel, nTS, field, iLayer = NULL) {
  names(OutputsModel)[
    sapply(names(OutputsModel), function(k) {
      x <- OutputsModel[[k]]
      if (is.null(iLayer)) {
        series <- x[[field]]
      } else {
        if (
          is.null(x$CemaNeigeLayers) || length(x$CemaNeigeLayers) < iLayer
        ) {
          return(FALSE)
        }
        series <- x$CemaNeigeLayers[[iLayer]][[field]]
      }
      return(!is.null(series) && length(series) == nTS)
    })
  ]
}

#' Get the IDs of the nodes that can contribute to the whole-catchment mean of
#' a node
#'
#' The candidates are the nodes of the whole catchment of `id` (the node and
#' all its upstream nodes) carrying an available time series (see
#' `OutputsModelIds`) whose local sub-basin area is known and greater than
#' zero. A node with an unknown or zero local sub-basin area (e.g. the
#' downstream end node of a network with a null local area, or a node sharing
#' its ID with a Diversion node, see [CreateGRiwrm]) cannot contribute and is
#' simply excluded (graceful behaviour).
#'
#' @param OutputsModel *GRiwrmOutputsModel* object (or a list of *OutputsModel*
#'   objects, one by node)
#' @param griwrm A *GRiwrm* object (See [CreateGRiwrm])
#' @param id [character] the ID of the node
#' @param nTS [numeric] number of time steps of the simulation
#' @param field [character] name of the time series
#' @param iLayer [numeric] index of the elevation layer for a CemaNeige layer
#'   series (default: the time series is read at the root of the *OutputsModel*
#'   object of the node)
#'
#' @return [character] vector of the IDs of the contributing nodes.
#' @noRd
#'
ContributingIds <- function(
  OutputsModel,
  griwrm,
  id,
  nTS,
  field,
  iLayer = NULL
) {
  # Nodes of the whole catchment of `id` (the node and all its upstream
  # nodes) with an available time series
  ids <- reduceGRiwrm(griwrm, id)$id
  ids <- intersect(ids, OutputsModelIds(OutputsModel, nTS, field, iLayer))
  # Area of the local sub-basin of each node
  areas <- sapply(ids, getNodeLocalArea, griwrm = griwrm)
  # Nodes that can contribute to the weighted mean: a known local sub-basin
  # area greater than zero
  return(ids[!is.na(areas) & areas > 0])
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
  ContributingNodes <- ids[Contributing]
  meteo <- meteo[, ContributingNodes, drop = FALSE]
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
