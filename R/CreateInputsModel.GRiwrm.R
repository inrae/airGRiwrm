#' Creation of an InputsModel object for a **airGRiwrm** network
#'
#' @param x \[GRiwrm object\] diagram of the semi-distributed model (See [CreateGRiwrm])
#' @param DatesR [POSIXt] vector of dates
#' @param Precip (optional) [matrix] or [data.frame] of [numeric] containing
#'        precipitation in \[mm per time step\]. Column names correspond to node IDs
#' @param PotEvap (optional) [matrix] or [data.frame] of [numeric] containing
#'        potential evaporation \[mm per time step\]. Column names correspond to node IDs
#' @param Qinf (optional) [matrix] or [data.frame] of [numeric] containing
#'        observed flows. It must be provided only for nodes of type "Direct
#'        injection" and "Diversion". See [CreateGRiwrm] for
#'        details about these node types. Unit is \[mm per time step\] for nodes
#'        with an area, and \[m3 per time step\] for nodes with `area=NA`.
#'        Column names correspond to node IDs. Negative flows are abstracted from
#'        the model and positive flows are injected to the model
#' @param Qobs (deprecated) use `Qinf` instead
#' @param Qmin (optional) [matrix] or [data.frame] of [numeric] containing
#'        minimum flows to let downstream of a node with a Diversion \[m3 per
#'        time step\]. Default is zero. Column names correspond to node IDs
#' @param Qrelease (optional) [matrix] or [data.frame] of [numeric] containing
#'        release flows by nodes using the model `RunModel_Reservoir` \[m3 per
#'        time step\]
#' @param PrecipScale (optional) named [vector] of [logical] indicating if the
#'        mean of the precipitation interpolated on the elevation layers must be
#'        kept or not, required to create CemaNeige module inputs, default `TRUE`
#'        (the mean of the precipitation is kept to the original value)
#' @param TempMean (optional) [matrix] or [data.frame] of time series of mean
#'        air temperature \[°C\], required to create the CemaNeige module inputs
#' @param TempMin (optional) [matrix] or [data.frame] of time series of minimum
#'        air temperature \[°C\], possibly used to create the CemaNeige module inputs
#' @param TempMax (optional) [matrix] or [data.frame] of time series of maximum
#'        air temperature \[°C\], possibly used to create the CemaNeige module inputs
#' @param ZInputs  (optional) named [vector] of [numeric] giving the mean
#'        elevation of the Precip and Temp series (before extrapolation) \[m\],
#'        possibly used to create the CemaNeige module input
#' @param HypsoData	(optional) [matrix] or [data.frame] containing 101 [numeric]
#'        rows: min, q01 to q99 and max of catchment elevation distribution \[m\],
#'        if not defined a single elevation is used for CemaNeige
#' @param NLayers (optional) named [vector] of [numeric] integer giving the number
#'        of elevation layers requested [-], required to create CemaNeige module
#'        inputs, default=5
#' @param IsHyst [logical] boolean indicating if the hysteresis version of
#'        CemaNeige is used. See details of [airGR::CreateRunOptions].
#' @param ... used for compatibility with S3 methods
#'
#' @details Meteorological data are needed for the nodes of the network that
#' represent a catchment simulated by a rainfall-runoff model. Instead of
#' [airGR::CreateInputsModel] that has [numeric] [vector] as time series inputs,
#' this function uses [matrix] or [data.frame] with the id of the sub-catchment
#' as column names. For single values (`ZInputs` or `NLayers`), the function
#' requires named [vector] with the id of the sub-catchment as name item. If an
#' argument is optional, only the column or the named item has to be provided.
#'
#' See [airGR::CreateInputsModel] documentation for details concerning each input.
#'
#' Number of rows of `Precip`, `PotEvap`, `Qinf`, `Qmin`, `TempMean`, `TempMin`,
#' `TempMax` must be the same of the length of `DatesR` (each row corresponds to
#' a time step defined in `DatesR`).
#'
#' For example of use of Direct Injection nodes, see vignettes
#' "V03_Open-loop_influenced_flow" and "V04_Closed-loop_regulated_withdrawal".
#'
#' For example of use of Diversion nodes, see example below and vignette
#' "V06_Modelling_regulated_diversion".
#'
#' @return A \emph{GRiwrmInputsModel} object which is a list of \emph{InputsModel}
#' objects created by [airGR::CreateInputsModel] with one item per modeled sub-catchment.
#' @export
#' @seealso [CreateGRiwrm()], [CreateRunOptions()], [RunModel.GRiwrmInputsModel()]
#' @example man-examples/RunModel.GRiwrmInputsModel.R
#'
CreateInputsModel.GRiwrm <- function(x, DatesR,
                                     Precip = NULL,
                                     PotEvap = NULL,
                                     Qinf = NULL,
                                     Qobs = NULL,
                                     Qmin = NULL,
                                     Qrelease = NULL,
                                     PrecipScale = TRUE,
                                     TempMean = NULL, TempMin = NULL,
                                     TempMax = NULL, ZInputs = NULL,
                                     HypsoData = NULL, NLayers = 5,
                                     IsHyst = FALSE, ...) {

  # Check and format inputs
  if (!is.null(Qobs) && !is.null(Qinf)) {
    stop("'Qobs' and 'Qinf' cannot be used together, use only 'Qinf' instead")
  }
  if (!is.null(Qobs)) {
    warning("The usage of 'Qobs' is deprecated, use 'Qinf' instead")
    Qinf <- Qobs
  }
  varNames <- c("Precip", "PotEvap", "TempMean", "Qinf", "Qmin",
                "TempMin", "TempMax", "ZInputs", "HypsoData", "NLayers")
  names(varNames) <- varNames
  lapply(varNames, function(varName) {
    v <- get(varName)
    if (!is.null(v)) {
      if (is.matrix(v) || is.data.frame(v)) {
        if (is.null(colnames(v))) {
          stop(sprintf(
            "'%s' must have column names",
            varName
          ))
        } else if (!all(colnames(v) %in% x$id)) {
          stop(sprintf(
            "'%s' column names must be included in 'id's of the GRiwrm object",
            varName
          ))
        } else if (any(duplicated(colnames(v)))) {
          stop(sprintf(
            "'%s' has duplicated column names: '%s'",
            varName,
            paste(colnames(v)[duplicated(colnames(v))], collapse = "', '")
          ))
        }
        if (!varName %in% c("ZInputs", "NLayers", "HypsoData") && nrow(v) != length(DatesR)) {
          stop(sprintf(
            "'%s' number of rows and the length of 'DatesR' must be equal",
             varName
          ))
        }
        if (varName %in% c("Precip", "PotEvap", "Qmin")) {
          if (any(is.na(v))) {
            stop(sprintf(
              "`NA` values detected in '%s'. Missing values are not allowed in InputsModel",
              varName
            ))
          }
          if (any(v < 0)) {
            stop(sprintf(
              "'%s' values must be positive or nul. Missing values are not allowed in InputsModel",
              varName
            ))
          }
        }
      } else if (!varName %in% c("ZInputs", "NLayers")) {
        stop(sprintf("'%s' must be a matrix or a data.frame", varName))
      }
    }
  })

  if (is.null(Qobs)) Qobs <- matrix(0, ncol = 0, nrow = length(DatesR))
  if (is.null(Qrelease)) Qrelease <- matrix(0, ncol = 0, nrow = length(DatesR))
  l <- updateQObsQrelease(g = x, Qobs = Qobs, Qrelease = Qrelease)
  Qobs <- l$Qobs
  Qrelease <- l$Qrelease
  checkQobsQrelease(x, "Qobs", Qobs)
  checkQobsQrelease(x, "Qrelease", Qrelease)

  diversionRows <- getDiversionRows(x)
  if (length(diversionRows) > 0) {
    warn <- FALSE
    if (is.null(Qmin)) {
      warn <- TRUE
    } else {
      Qmin <- as.matrix(Qmin)
      if (!all(colnames(Qmin) %in% x$id[diversionRows])) {
        stop(paste(
          "'Qmin' contains columns that does not match with IDs of Diversion nodes:\n",
          setdiff(colnames(Qmin), x$id[diversionRows])
        ))
      }
      if (is.null(colnames(Qmin))) {
        warn <- TRUE
      } else if (!all(x$id[diversionRows] %in% colnames(Qmin))) {
        warn <- TRUE
      }
      if (any(is.na(Qmin))) {
        stop("`NA` values are note allowed in 'Qmin'")
      }
    }
    if (warn) {
      warning(
        sprintf(
          "'Qmin' would include the following columns %s.\n Zero values are applied by default.",
          paste(x$id[diversionRows], collapse = ", ")
        )
      )
    }
    # Qmin completion for Diversion nodes with default zero values
    Qmin0 <- matrix(0, nrow = length(DatesR), ncol = length(diversionRows))
    colnames(Qmin0) <- x$id[diversionRows]
    if (is.null(Qmin)) {
      Qmin <- Qmin0
    } else {
      Qmin0[, colnames(Qmin)] <- Qmin
      Qmin <- Qmin0
    }
  }

  InputsModel <- CreateEmptyGRiwrmInputsModel(x)

  for (id in getNodeRanking(x)) {
    message("CreateInputsModel.GRiwrm: Processing sub-basin ", id, "...")

    InputsModel[[id]] <-
      CreateOneGRiwrmInputsModel(id = id,
                                 griwrm = x,
                                 DatesR = DatesR,
                                 Precip = getInputBV(Precip, id),
                                 PrecipScale,
                                 PotEvap = getInputBV(PotEvap, id),
                                 TempMean = getInputBV(TempMean, id),
                                 TempMin = getInputBV(TempMin, id),
                                 TempMax = getInputBV(TempMax, id),
                                 ZInputs = getInputBV(ZInputs, id),
                                 HypsoData = getInputBV(HypsoData, id),
                                 NLayers = getInputBV(NLayers, id, 5),
                                 Qobs = Qobs,
                                 Qmin = getInputBV(Qmin, id),
                                 Qrelease = Qrelease,
                                 IsHyst = IsHyst
                                 )
  }
  attr(InputsModel, "TimeStep") <- getModelTimeStep(InputsModel)
  return(InputsModel)
}


#' Creation of an empty InputsModel object for **airGRiwrm** nodes
#'
#' @param griwrm a `GRiwrm` object (See [CreateGRiwrm])
#'
#' @return \emph{GRiwrmInputsModel} empty object
#' @noRd
CreateEmptyGRiwrmInputsModel <- function(griwrm) {
  InputsModel <- list()
  class(InputsModel) <- c("GRiwrmInputsModel", class(InputsModel))
  # Save the GRiwrm in InputsModel for later use
  attr(InputsModel, "GRiwrm") <- griwrm
  return(InputsModel)
}


#' Create one InputsModel for a **airGRiwrm** node
#'
#' @param id string of the node identifier
#' @param griwrm See [CreateGRiwrm])
#' @param ... parameters sent to [airGR::CreateInputsModel]:
#'        - `DatesR` [vector] of dates required to create the GR model and CemaNeige module inputs
#'        - `Precip` [vector] time series of potential evapotranspiration (catchment average) (mm/time step)
#'        - `PotEvap` [vector] time series of potential evapotranspiration (catchment average) (mm/time step)
#' @param Qobs Matrix or data frame of numeric containing observed flow (mm/time step). Column names correspond to node IDs
#'
#' @return \emph{InputsModel} object for one.
#' @noRd
CreateOneGRiwrmInputsModel <- function(id, griwrm, DatesR, ..., Qobs, Qmin, Qrelease, IsHyst) {
  np <- getNodeProperties(id, griwrm)

  if (np$Diversion) {
    rowDiv <- which(griwrm$id == id & griwrm$model == "Diversion")
    diversionOutlet <- griwrm$down[rowDiv]
  }

  g2 <- griwrm[getDiversionRows(griwrm, TRUE), ]
  node <- g2[g2$id == id, ]
  FUN_MOD <- g2$model[g2$id == node$donor]

  # Set hydraulic parameters
  UpstreamNodeRows <- which(griwrm$down == id & !is.na(griwrm$down))
  Qupstream <- NULL
  LengthHydro <- NULL
  BasinAreas <- NULL

  if (length(UpstreamNodeRows) > 0) {
    # Sub-basin with hydraulic routing
    Qupstream <- NULL
    Qupstream <- as.matrix(cbind(
      Qobs[ , colnames(Qobs)[colnames(Qobs) %in% griwrm$id[UpstreamNodeRows]], drop = FALSE],
      Qrelease[ , colnames(Qrelease)[colnames(Qrelease) %in% griwrm$id[UpstreamNodeRows]], drop = FALSE]
    ))
    # Qupstream completion with zeros for all upstream nodes
    Qupstream0 <- matrix(0, nrow = length(DatesR), ncol = length(UpstreamNodeRows))
    colnames(Qupstream0) <- griwrm$id[UpstreamNodeRows]
    if (is.null(Qupstream) || ncol(Qupstream) == 0) {
      Qupstream <- Qupstream0
    } else {
      Qupstream0[, colnames(Qupstream)] <- Qupstream
      Qupstream <- Qupstream0
    }
    upstreamDiversion <- which(
      sapply(griwrm$id[UpstreamNodeRows],
             function(id) {
               getNodeProperties(id, griwrm)$Diversion
             })
    )
    if (length(upstreamDiversion) > 0) {
      Qupstream[, upstreamDiversion] <- - Qupstream[, upstreamDiversion]
    }
    LengthHydro <- griwrm$length[UpstreamNodeRows]
    names(LengthHydro) <- griwrm$id[UpstreamNodeRows]
    upstreamAreas <- sapply(UpstreamNodeRows, getNodeBasinArea, griwrm = griwrm)
    BasinAreas <- c(
        upstreamAreas,
        node$area - sum(upstreamAreas, na.rm = TRUE)
    )
    if (!is.na(node$area)) {
      if (BasinAreas[length(BasinAreas)] < 0) {
        stop(sprintf(
          "Area of the catchment %s must be greater than the sum of the areas of its upstream catchments",
          id
        ))
      }
    }
    names(BasinAreas) <- c(griwrm$id[UpstreamNodeRows], id)
  }

  FUN_MOD_REAL <- FUN_MOD
  if (np$Reservoir) {
    FUN_MOD <- "RunModel_Lag"
    FUN_MOD_REAL <- "RunModel_Reservoir"
  }
  # Set model inputs with the **airGR** function
  InputsModel <- CreateInputsModel(
    FUN_MOD,
    DatesR = DatesR,
    ...,
    Qupstream = Qupstream,
    LengthHydro = LengthHydro,
    BasinAreas = BasinAreas
  )

  # Add Identifiers of connected nodes in order to be able to update them with simulated flows
  InputsModel$id <- id
  InputsModel$down <- node$down
  if (length(UpstreamNodeRows) > 0) {
    InputsModel$UpstreamNodes <- griwrm$id[UpstreamNodeRows]
    InputsModel$UpstreamIsModeled <- !is.na(griwrm$model[UpstreamNodeRows])
    names(InputsModel$UpstreamIsModeled) <- InputsModel$UpstreamNodes
    InputsModel$UpstreamVarQ <- ifelse(!is.na(griwrm$model[UpstreamNodeRows]) &
                                    griwrm$model[UpstreamNodeRows] == "Diversion",
                                    "Qdiv_m3",
                                    "Qsim_m3")
    names(InputsModel$UpstreamVarQ) <- InputsModel$UpstreamNodes
  } else {
    InputsModel$BasinAreas <- node$area
  }

  # Add the model function
  InputsModel$FUN_MOD <- FUN_MOD
  featModel <- .GetFeatModel(InputsModel, IsHyst)
  # inUngaugedCluster: Ungauged node with downstream donor
  # including reservoirs between ungauged nodes and donor
  InputsModel$inUngaugedCluster <- (node$model == "Ungauged" || np$Reservoir) &&
                                   node$id != node$donor &&
                                   isNodeDownstream(griwrm, id, node$donor)
  # isReceiver: Ungauged node with not downstream donor
  InputsModel$isReceiver <- node$model == "Ungauged" &&
                            !isNodeDownstream(griwrm, id, node$donor)
  InputsModel$gaugedId <- node$donor
  InputsModel$hasUngaugedNodes <- hasUngaugedNodes(id, griwrm)
  InputsModel$model <-
    list(
      indexParamUngauged = ifelse(inherits(InputsModel, "SD"), 0, 1) +
        seq.int(featModel$NbParam),
      hasX4 = grepl("RunModel_(CemaNeige|)GR[456][HJ]", FUN_MOD),
      iX4 = ifelse(inherits(InputsModel, "SD"), 5, 4),
      IsHyst = featModel$IsHyst
    )
  InputsModel$hasDiversion <- np$Diversion
  InputsModel$isReservoir <- np$Reservoir

  # Add specific properties for Diversion and Reservoir nodes
  if (np$Diversion) {
    InputsModel$diversionOutlet <- diversionOutlet
    InputsModel$Qdiv <- -Qobs[, id, drop = TRUE]
    InputsModel$Qmin <- Qmin
  }
  if (np$Reservoir) {
    # Fill reservoir release with Qobs
    InputsModel$Qrelease <- Qrelease[, id, drop = TRUE]
  }

  # Add class for S3 process (Prequel of HYCAR-Hydro/airgr#60)
  class(InputsModel) <- c(FUN_MOD_REAL, class(InputsModel))

  return(InputsModel)
}


#' Check of time steps of the model for all nodes and return of the time step in seconds
#'
#' Function that is called inside [CreateInputsModel.GRiwrm] for defining the time step of the complete model
#'
#' @param InputsModel \[object of class `GRiwrmInputsModel`\]
#'
#' @return [numeric] time step in seconds
#' @noRd
getModelTimeStep <- function(InputsModel) {
  TS <- sapply(InputsModel, function(x) {
    if (inherits(x, "GR")) {
      if (inherits(x, "hourly")) {
        return(60 * 60)
      } else if (inherits(x, "daily")) {
        return(60 * 60 * 24)
      } else {
        stop("All models should be at hourly or daily time step")
      }
    }
    return(NA)
  })
  TS <- TS[!is.na(TS)]
  if (length(unique(TS)) != 1) {
    stop("Time steps of the model of all nodes should be identical")
  }
  return(unique(TS))
}

#' Select the node input for input arguments of [airGR::CreateInputsModel]
#'
#' @param x [matrix] [data.frame] or named [vector] the input argument
#' @param id [character] the id of the node
#' @param unset default value if the id is not found in `x`
#'
#' @return the selected column or value in respect to `id`
#' @noRd
getInputBV <- function(x, id, unset = NULL) {
  if (is.null(x)) {
    return(unset)
  }
  if (is.matrix(x) || is.data.frame(x)) {
    if (!id %in% colnames(x)) {
      return(unset)
    }
  } else {
    # vector (for ZInputs and NLayers)
    if (length(x) == 1 && is.null(names(x))) {
      return(x)
    } else if (!id %in% names(x)) {
      return(unset)
    } else {
      return(x[id])
    }
  }
  return(x[, id])
}


#' Check if current node contains ungauged nodes that shares its parameters
#'
#' @param id id [character] Id of the current node
#' @param griwrm See [CreateGRiwrm])
#'
#' @return A [logical], `TRUE` if the node `id` contains ungauged nodes.
#'
#' @noRd
hasUngaugedNodes <- function(id, griwrm) {
  g <- griwrm[!is.na(griwrm$model), ]
  idsWithCurrentAsDonor <- g$id[g$id != id & g$donor == id]
  if (length(idsWithCurrentAsDonor) == 0) return(FALSE)
  areNodesUpstream <- sapply(idsWithCurrentAsDonor,
                             function(x) isNodeUpstream(g, id, x))
  if (!any(areNodesUpstream)) return(FALSE)
  g_red <- g[g$id %in% idsWithCurrentAsDonor[areNodesUpstream], ]
  if (any(g_red$model == "Ungauged")) return(TRUE)
  return(FALSE)
}


#' function to extract model features partially copied from airGR:::.GetFeatModel
#' @importFrom utils tail
#' @noRd
.GetFeatModel <- function(InputsModel, IsHyst) {
  path <- system.file("modelsFeatures/FeatModelsGR.csv", package = "airGR")
  FeatMod <- read.table(path, header = TRUE, sep = ";", stringsAsFactors = FALSE)
  NameFunMod <- ifelse(test = FeatMod$Pkg %in% "airGR",
                       yes  = paste("RunModel", FeatMod$NameMod, sep = "_"),
                       no   = FeatMod$NameMod)
  IdMod <- which(sapply(NameFunMod, FUN = function(x) identical(InputsModel$FUN_MOD, x)))
  if (length(IdMod) < 1) {
    stop("'FUN_MOD' must be one of ", paste(NameFunMod, collapse = ", "))
  }
  FeatMod <- as.list(FeatMod[IdMod, ])
  FeatMod$IsSD <- inherits(InputsModel, "SD")
  if (FeatMod$IsSD) {
    FeatMod$NbParam <- FeatMod$NbParam + 1
  }
  FeatMod$IsHyst <- FALSE
  if (grepl("CemaNeige", FeatMod$NameMod)) {
    FeatMod$IsHyst <- IsHyst
    if (IsHyst) {
      FeatMod$NbParam <- FeatMod$NbParam + 2
    }
  }
  return(FeatMod)
}


#' Return the basin area of a node
#'
#' If the current node doesn't have an area (for example, for a Reservoir,
#' a Direct Injection, or a [airGR::RunModel_Lag]), this function return the sum
#' of the basin areas of the upstream nodes.
#'
#' @param i [integer] row number in the GRiwrm network
#' @param griwrm A GRiwrm
#'
#' @return The basin area of the node
#' @noRd
#'
getNodeBasinArea <- function(i, griwrm) {
  area <- griwrm$area[i]
  if (!is.na(area)) return(area)
  Diversions <- !is.na(griwrm$model) & griwrm$model == "Diversion"
  if (i %in% which(Diversions)) return(NA)
  UpstreamNodeRows <-
    which(griwrm$down == griwrm$id[i] & !is.na(griwrm$down) & !Diversions)
  if (length(UpstreamNodeRows) > 0) {
    upstreamAreas <- sapply(UpstreamNodeRows, getNodeBasinArea, griwrm = griwrm)
    return(sum(upstreamAreas, na.rm = TRUE))
  } else {
    return(NA)
  }

}
