#' Creation of an InputsModel object for a **airGRiwrm** network
#'
#' @param x \[GRiwrm object\] diagram of the semi-distributed model (See [CreateGRiwrm])
#' @param DatesR [POSIXt] vector of dates
#' @param Precip (optional) [matrix] or [data.frame] frame of numeric containing precipitation in \[mm per time step\]. Column names correspond to node IDs
#' @param PotEvap (optional) [matrix] or [data.frame] frame of numeric containing potential evaporation \[mm per time step\]. Column names correspond to node IDs
#' @param Qobs (optional) [matrix] or [data.frame] frame of numeric containing observed flows in \[mm per time step\]. Column names correspond to node IDs
#' @param PrecipScale (optional) named [vector] of [logical] indicating if the mean of the precipitation interpolated on the elevation layers must be kept or not, required to create CemaNeige module inputs, default `TRUE` (the mean of the precipitation is kept to the original value)
#' @param TempMean (optional) [matrix] or [data.frame] of time series of mean air temperature \[°C\], required to create the CemaNeige module inputs
#' @param TempMin (optional) [matrix] or [data.frame] of time series of minimum air temperature \[°C\], possibly used to create the CemaNeige module inputs
#' @param TempMax (optional) [matrix] or [data.frame] of time series of maximum air temperature \[°C\], possibly used to create the CemaNeige module inputs
#' @param ZInputs  (optional) named [vector] of [numeric] giving the mean elevation of the Precip and Temp series (before extrapolation) \[m\], possibly used to create the CemaNeige module input
#' @param HypsoData	(optional) [matrix] or [data.frame] containing 101 [numeric] rows: min, q01 to q99 and max of catchment elevation distribution \[m\], if not defined a single elevation is used for CemaNeige
#' @param NLayers (optional) named [vector] of [numeric] integer giving the number of elevation layers requested [-], required to create CemaNeige module inputs, default=5
#' @param ... used for compatibility with S3 methods
#'
#' @details Meteorological data are needed for the nodes of the network that represent a catchment simulated by a rainfall-runoff model. Instead of [airGR::CreateInputsModel] that has [numeric] [vector] as time series inputs, this function uses [matrix] or [data.frame] with the id of the sub-catchment as column names. For single values (`ZInputs` or `NLayers`), the function requires named [vector] with the id of the sub-catchment as name item. If an argument is optional, only the column or the named item has to be provided.
#'
#' See [airGR::CreateInputsModel] documentation for details concerning each input.
#'
#' @return A \emph{GRiwrmInputsModel} object which is a list of \emph{InputsModel} objects created by [airGR::CreateInputsModel] with one item per modeled sub-catchment.
#' @export
#' @inherit RunModel.GRiwrmInputsModel return examples
#'
CreateInputsModel.GRiwrm <- function(x, DatesR,
                                     Precip = NULL,
                                     PotEvap = NULL,
                                     Qobs = NULL,
                                     PrecipScale = TRUE,
                                     TempMean = NULL, TempMin = NULL,
                                     TempMax = NULL, ZInputs = NULL,
                                     HypsoData = NULL, NLayers = 5, ...) {

  # Check and format inputs
  varNames <- c("Precip", "PotEvap", "TempMean", "Qobs",
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
          stop("'%s' number of rows and the length of 'DatesR' must be equal",
               varName)
        }

      } else if (!varName %in% c("ZInputs", "NLayers")) {
        stop(sprintf("'%s' must be a matrix or a data.frame", varName))
      }
    }
  })

  directFlowIds <- x$id[is.na(x$model)]
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


  InputsModel <- CreateEmptyGRiwrmInputsModel(x)

  # Qobs completion
  Qobs0 <- matrix(0, nrow = length(DatesR), ncol = nrow(x))
  colnames(Qobs0) <- x$id
  if (is.null(Qobs)) {
    Qobs <- Qobs0
  } else {
    Qobs0[, colnames(Qobs)] <- Qobs
    Qobs <- Qobs0
  }

  for(id in getNodeRanking(x)) {
    message("CreateInputsModel.GRiwrm: Treating sub-basin ", id, "...")
    if (x$area[x$id == id] > 0 && any(Qobs[, id] < 0, na.rm = TRUE)) {
      stop(sprintf("Negative flow found in 'Qobs[, \"%s\"]'. ", id),
           "Catchment flow can't be negative, use `NA` for flow data gaps.")
    }

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
                                 Qobs = Qobs
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
CreateOneGRiwrmInputsModel <- function(id, griwrm, ..., Qobs) {
  node <- griwrm[griwrm$id == id,]
  FUN_MOD <- griwrm$model[griwrm$id == id]

  # Set hydraulic parameters
  UpstreamNodes <- griwrm$id[griwrm$down == id & !is.na(griwrm$down)]
  Qupstream <- NULL
  LengthHydro <- NULL
  BasinAreas <- NULL

  if(length(UpstreamNodes) > 0) {
    # Sub-basin with hydraulic routing
    Qupstream <- as.matrix(Qobs[ , UpstreamNodes, drop=FALSE])
    LengthHydro <- griwrm$length[griwrm$id %in% UpstreamNodes]
    names(LengthHydro) <- UpstreamNodes
    BasinAreas <- c(
        griwrm$area[griwrm$id %in% UpstreamNodes],
        node$area - sum(griwrm$area[griwrm$id %in% UpstreamNodes], na.rm = TRUE)
    )
    if (BasinAreas[length(BasinAreas)] < 0) {
      stop(sprintf(
        "Area of the catchment %s must be greater than the sum of the areas of its upstream catchments",
        id
      ))
    }
    names(BasinAreas) <- c(UpstreamNodes, id)
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
  if(length(UpstreamNodes) > 0) {
    InputsModel$UpstreamNodes <- UpstreamNodes
    InputsModel$UpstreamIsRunoff <- !is.na(griwrm$model[match(UpstreamNodes, griwrm$id)])
  } else {
    InputsModel$BasinAreas <- node$area
  }

  # Add the model function
  InputsModel$FUN_MOD <- FUN_MOD

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
