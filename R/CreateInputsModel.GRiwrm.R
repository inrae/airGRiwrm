#' Create InputsModel object for a **airGRiwrm** network
#'
#' @param x GRiwrm object describing the diagram of the semi-distributed model (See [CreateGRiwrm])
#' @param DatesR Vector of POSIXt observation time steps
#' @param Precip Matrix or data frame of numeric containing precipitation in mm. Column names correspond to node IDs
#' @param PotEvap Matrix or data frame of numeric containing potential evaporation in mm. Column names correspond to node IDs
#' @param Qobs Matrix or data frame of numeric containing potential observed flow in mm. Column names correspond to node IDs
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
#' @return GRiwrmInputsModel object equivalent to **airGR** InputsModel object for a semi-distributed model (See [airGR::CreateInputsModel])
#' @export
#' @examples
#' #################################################################
#' # Run the `airGRRunModel_Lag` example in the GRiwrm fashion way #
#' #################################################################
#'
#' # Run airGR RunModel_Lag example for harvesting necessary data
#' library(airGR)
#' example(RunModel_Lag)
#' # detach the package because airGR overwrite airGRiwrm functions here
#' detach("package:airGR")
#'
#' # This example is a network of 2 nodes which can be describe like this:
#' db <- data.frame(id = c("Reservoir", "GaugingDown"),
#'                  length = c(LengthHydro, NA),
#'                  down = c("GaugingDown", NA),
#'                  area = c(NA, BasinInfo$BasinArea),
#'                  model = c(NA, "RunModel_GR4J"),
#'                  stringsAsFactors = FALSE)
#'
#' # Create GRiwrm object from the data.frame
#' griwrm <- CreateGRiwrm(db)
#' str(griwrm)
#'
#' # Formatting observations for the hydrological models
#' # Each input data should be a matrix or a data.frame with the good id in the name of the column
#' Precip <- matrix(BasinObs$P, ncol = 1)
#' colnames(Precip) <- "GaugingDown"
#' PotEvap <- matrix(BasinObs$E, ncol = 1)
#' colnames(PotEvap) <- "GaugingDown"
#'
#' # Observed flows are integrated now because we mix:
#' #  - flows that are directly injected in the model
#' #  - flows that could be used for the calibration of the hydrological models
#' Qobs = matrix(c(Qupstream, BasinObs$Qmm), ncol = 2)
#' colnames(Qobs) <- griwrm$id
#' str(Qobs)
#'
#' InputsModels <- CreateInputsModel(griwrm,
#'                             DatesR = BasinObs$DatesR,
#'                             Precip = Precip,
#'                             PotEvap = PotEvap,
#'                             Qobs = Qobs)
#' str(InputsModels)
#'
CreateInputsModel.GRiwrm <- function(x, DatesR,
                                     Precip,
                                     PotEvap = NULL,
                                     Qobs,
                                     PrecipScale = TRUE,
                                     TempMean = NULL, TempMin = NULL,
                                     TempMax = NULL, ZInputs = NULL,
                                     HypsoData = NULL, NLayers = 5, ...) {

  # Check and format inputs
  varNames <- c("Precip", "PotEvap", "TempMean",
                "TempMin", "TempMax", "ZInputs", "HypsoData", "NLayers")
  names(varNames) <- varNames
  lapply(varNames, function(varName) {
    v <- get(varName)
    if(!is.null(v)) {
      if(is.matrix(v) || is.data.frame(v)) {
        if(is.null(colnames(v))) {
          stop(sprintf(
            "'%s' must have column names",
            varName
          ))
        } else if(!all(colnames(v) %in% x$id)) {
          stop(sprintf(
            "'%s' column names must be included in 'id's of the GRiwrm object",
            varName
          ))
        }
      } else if (!varName %in% c("ZInputs", "NLayers")) {
        stop(sprintf("'%s' must be a matrix or a data.frame", varName))
      }
    }
  })

  InputsModel <- CreateEmptyGRiwrmInputsModel(x)
  Qobs[is.na(Qobs)] <- -99 # airGR::CreateInputsModel doesn't accept NA values

  for(id in getNodeRanking(x)) {
    message("CreateInputsModel.GRiwrm: Treating sub-basin ", id, "...")
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


#' Create an empty InputsModel object for **airGRiwrm** nodes
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
#' @param DatesR vector of dates required to create the GR model and CemaNeige module inputs
#' @param Precip time series of potential evapotranspiration (catchment average) (mm/time step)
#' @param PotEvap time series of potential evapotranspiration (catchment average) (mm/time step)
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


#' Check time steps of the model of all the nodes and return the time step in seconds
#'
#' This function is called inside [CreateInputsModel.GRiwrm] for defining the time step of the big model.
#'
#' @param InputsModel a `GRiwrmInputsModel`
#'
#' @return A [numeric] representing the time step in seconds
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
