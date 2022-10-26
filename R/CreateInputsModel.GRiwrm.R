#' Creation of an InputsModel object for a **airGRiwrm** network
#'
#' @param x \[GRiwrm object\] diagram of the semi-distributed model (See [CreateGRiwrm])
#' @param DatesR [POSIXt] vector of dates
#' @param Precip (optional) [matrix] or [data.frame] of [numeric] containing
#'        precipitation in \[mm per time step\]. Column names correspond to node IDs
#' @param PotEvap (optional) [matrix] or [data.frame] of [numeric] containing
#'        potential evaporation \[mm per time step\]. Column names correspond to node IDs
#' @param Qobs (optional) [matrix] or [data.frame] of [numeric] containing
#'        observed flows. It must be provided only for nodes of type "Direct
#'        injection" and "Diversion". See [CreateGRiwrm] for
#'        details about these node types. Unit is \[mm per time step\] for nodes
#'        with an area, and \[m3 per time step\] for nodes with `area=NA`.
#'        Column names correspond to node IDs. Negative flows are abstracted from
#'        the model and positive flows are injected to the model
#' @param Qmin (optional) [matrix] or [data.frame] of [numeric] containing
#'        minimum flows to let downstream of a node with a Diversion \[m3 per
#'        time step\]. Default is zero. Column names correspond to node IDs
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
#' Number of rows of `Precip`, `PotEvap`, `Qobs`, `Qmin`, `TempMean`, `TempMin`,
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
#' @example man-examples/RunModel.GRiwrmInputsModel.R
#'
CreateInputsModel.GRiwrm <- function(x, DatesR,
                                     Precip = NULL,
                                     PotEvap = NULL,
                                     Qobs = NULL,
                                     Qmin = NULL,
                                     PrecipScale = TRUE,
                                     TempMean = NULL, TempMin = NULL,
                                     TempMax = NULL, ZInputs = NULL,
                                     HypsoData = NULL, NLayers = 5, ...) {

  # Check and format inputs
  varNames <- c("Precip", "PotEvap", "TempMean", "Qobs", "Qmin",
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

  directFlowIds <- x$id[is.na(x$model) | x$model == "Diversion"]
  if (length(directFlowIds) > 0) {
    err <- FALSE
    if (is.null(Qobs)) {
      err <- TRUE
    } else {
      Qobs <- as.matrix(Qobs)
      if (is.null(colnames(Qobs))) {
        err <- TRUE
      } else if (!all(directFlowIds %in% colnames(Qobs))) {
        err <- TRUE
      }
    }
    if (err) stop(sprintf("'Qobs' column names must at least contain %s", paste(directFlowIds, collapse = ", ")))
  }
  if (!all(colnames(Qobs) %in% directFlowIds)) {
    stop("The following columns in 'Qobs' don't match with ",
         "Direction Injection or Diversion nodes: ",
         paste(setdiff(colnames(Qobs), directFlowIds), collapse = ", "))
    Qobs <- Qobs[directFlowIds, ]
  }
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
          paste(directFlowIds, collapse = ", ")
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

  # Qobs completion for at least filling Qupstream of all nodes by zeros
  Qobs0 <- matrix(0, nrow = length(DatesR), ncol = nrow(x))
  colnames(Qobs0) <- x$id
  if (is.null(Qobs)) {
    Qobs <- Qobs0
  } else {
    Qobs0[, colnames(Qobs)] <- Qobs
    Qobs <- Qobs0
  }

  for(id in getNodeRanking(x)) {
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
                                 Qmin = getInputBV(Qmin, id)
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
  # Update griwrm in case of manual change in model column
  griwrm$donor <- sapply(griwrm$id, getGaugedId, griwrm = griwrm)
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
CreateOneGRiwrmInputsModel <- function(id, griwrm, ..., Qobs, Qmin) {
  hasDiversion <- "Diversion" %in% getNodeProperties(id, griwrm)
  if (hasDiversion) {
    rowDiv <- which(griwrm$id == id & griwrm$model == "Diversion")
    hasDiversion <- TRUE
    diversionOutlet <- griwrm$down[rowDiv]
    griwrm <- griwrm[-rowDiv, ]
  }
  node <- griwrm[griwrm$id == id,]
  FUN_MOD <- griwrm$model[griwrm$id == griwrm$donor[griwrm$id == id]]

  # Set hydraulic parameters
  UpstreamNodeRows <- which(griwrm$down == id & !is.na(griwrm$down))
  Qupstream <- NULL
  LengthHydro <- NULL
  BasinAreas <- NULL

  if(length(UpstreamNodeRows) > 0) {
    # Sub-basin with hydraulic routing
    Qupstream <- as.matrix(Qobs[ , griwrm$id[UpstreamNodeRows], drop=FALSE])
    LengthHydro <- griwrm$length[UpstreamNodeRows]
    names(LengthHydro) <- griwrm$id[UpstreamNodeRows]
    BasinAreas <- c(
        griwrm$area[UpstreamNodeRows],
        node$area - sum(griwrm$area[UpstreamNodeRows], na.rm = TRUE)
    )
    if (BasinAreas[length(BasinAreas)] < 0) {
      stop(sprintf(
        "Area of the catchment %s must be greater than the sum of the areas of its upstream catchments",
        id
      ))
    }
    names(BasinAreas) <- c(griwrm$id[UpstreamNodeRows], id)
  }

  # Set model inputs with the **airGR** function
  InputsModel <- CreateInputsModel(
    FUN_MOD,
    ...,
    Qupstream = Qupstream,
    LengthHydro = LengthHydro,
    BasinAreas = BasinAreas
  )

  # Add Identifiers of connected nodes in order to be able to update them with simulated flows
  InputsModel$id <- id
  InputsModel$down <- node$down
  if(length(UpstreamNodeRows) > 0) {
    InputsModel$UpstreamNodes <- griwrm$id[UpstreamNodeRows]
    InputsModel$UpstreamIsModeled <- !is.na(griwrm$model[UpstreamNodeRows])
    InputsModel$UpstreamVarQ <- ifelse(!is.na(griwrm$model[UpstreamNodeRows]) &
                                    griwrm$model[UpstreamNodeRows] == "Diversion",
                                    "Qdiv_m3",
                                    "Qsim_m3")
  } else {
    InputsModel$BasinAreas <- node$area
  }

  # Add the model function
  InputsModel$FUN_MOD <- FUN_MOD
  featModel <- .GetFeatModel(InputsModel)
  InputsModel$isUngauged <- griwrm$model[griwrm$id == id] == "Ungauged"
  InputsModel$gaugedId <- griwrm$donor[griwrm$id == id]
  InputsModel$hasUngaugedNodes <- hasUngaugedNodes(id, griwrm)
  InputsModel$model <- list(indexParamUngauged = ifelse(inherits(InputsModel, "SD"), 0, 1) + seq.int(featModel$NbParam),
                            hasX4 = grepl("RunModel_GR[456][HJ]", FUN_MOD),
                            iX4 = ifelse(inherits(InputsModel, "SD"), 5, 4))
  InputsModel$hasDiversion <- hasDiversion
  if (hasDiversion) {
    InputsModel$diversionOutlet <- diversionOutlet
    InputsModel$Qdiv <- -Qobs[, id]
    InputsModel$Qmin <- Qmin
  }
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
    if (inherits(x, "hourly")) {
      TimeStep <- 60 * 60
    } else if (inherits(x, "daily")) {
      TimeStep <- 60 * 60 * 24
    } else {
      stop("All models should be at hourly or daily time step")
    }
  })
  if(length(unique(TS)) != 1) {
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
  if(is.null(x)) {
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
    } else if(!id %in% names(x)) {
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
  upIds <- griwrm$id[griwrm$down == id]
  upIds <- upIds[!is.na(upIds)]
  # No upstream nodes
  if(length(upIds) == 0) return(FALSE)
  # At least one upstream node is ungauged
  UngNodes <- griwrm$model[griwrm$id %in% upIds] == "Ungauged"
  UngNodes <- UngNodes[!is.na(UngNodes)]
  if(length(UngNodes) > 0 && any(UngNodes)) return(TRUE)
  # At least one node's model is NA need to investigate next level
  if(any(is.na(griwrm$model[griwrm$id %in% upIds]))) {
    g <- griwrm[griwrm$id %in% upIds, ]
    NaIds <- g$id[is.na(g$model)]
    out <- sapply(NaIds, hasUngaugedNodes, griwrm = griwrm)
    return(any(out))
  }
  return(FALSE)
}


#' function to extract model features partially copied from airGR:::.GetFeatModel
#' @importFrom utils tail
#' @noRd
.GetFeatModel <- function(InputsModel) {
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
  return(FeatMod)
}
