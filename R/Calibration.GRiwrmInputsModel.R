#' @param useUpstreamQsim boolean describing if simulated (\code{TRUE}) or observed (\code{FALSE}) flows are used for calibration. Default is \code{TRUE}
#' @rdname Calibration
#' @export
Calibration.GRiwrmInputsModel <- function(InputsModel,
                                          RunOptions,
                                          InputsCrit,
                                          CalibOptions,
                                          useUpstreamQsim = TRUE,
                                          ...) {

  # Argument checks

  # We invoke the mandatory arguments here for avoiding
  # a messy error message on "get(x)" if an argument is missing
  InputsModel
  RunOptions
  InputsCrit
  CalibOptions

  # Checking argument classes
  vars2check <- c("InputsModel", "RunOptions", "InputsCrit", "CalibOptions")
  lapply(vars2check, function(x) {
    if (!inherits(get(x), paste0("GRiwrm", x))) {
      stop(sprintf("'%1$s' must be of class GRiwrm%1$s, type '?Create%1$s' for help", x))
    }
  })

  OutputsCalib <- list()
  class(OutputsCalib) <- append("GRiwrmOutputsCalib", class(OutputsCalib))

  OutputsModel <- list()
  class(OutputsModel) <- append("GRiwrmOutputsModel", class(OutputsModel))

  b <- sapply(InputsModel, function(IM) !IM$isUngauged)
  gaugedIds <- names(b[b])

  for(id in gaugedIds) {
    IM <- InputsModel[[id]]
    message("Calibration.GRiwrmInputsModel: Processing sub-basin ", id, "...")

    if (inherits(InputsCrit[[id]], "InputsCritLavenneFunction")) {
      IC <- getInputsCrit_Lavenne(id, OutputsModel, InputsCrit)
    } else {
      IC <- InputsCrit[[id]]
    }
    hasUngauged <- IM$hasUngauged
    if (hasUngauged) {
      l  <- updateParameters4Ungauged(id,
                                      InputsModel,
                                      RunOptions,
                                      CalibOptions,
                                      OutputsModel,
                                      useUpstreamQsim)
      IM <- l$InputsModel
      IM$FUN_MOD <- "RunModel_Ungauged"
      attr(RunOptions[[id]], "GRiwrmRunOptions") <- l$RunOptions
    } else {
      if (useUpstreamQsim && any(IM$UpstreamIsModeled)) {
        # Update InputsModel$Qupstream with simulated upstream flows
        IM <- UpdateQsimUpstream(IM, RunOptions[[id]], OutputsModel)
      }
    }

    if (!is.null(IM$isReservoir) && IM$isReservoir & any(is.na(CalibOptions[[id]]$FixedParam))) {
      stop("Parameters of `RunModel_Reservoir` nodes can't be calibrated",
           "Fix these parameters by using the command:\n",
           "`CalibOptions[[id_of_reservoir_node]]$FixedParam <- c(Vmax, celerity)`")
    }

    OutputsCalib[[id]] <- Calibration(
      InputsModel = IM,
      RunOptions = RunOptions[[id]],
      InputsCrit = IC,
      CalibOptions = CalibOptions[[id]],
      ...
    )

    if (hasUngauged) {
      # Select nodes with model in the sub-network
      g <- attr(IM, "GRiwrm")
      Ids <- g$id[g$donor == id & !is.na(g$model)]
      # Extract the X4 calibrated for the whole intermediate basin
      if(IM[[id]]$model$hasX4) {
        X4 <- OutputsCalib[[id]]$ParamFinalR[IM[[id]]$model$iX4] # Global parameter
        subBasinAreas <- calcSubBasinAreas(IM)
      }
      for (uId in Ids) {
        if(!IM[[uId]]$isReservoir) {
          # Add OutputsCalib for ungauged nodes
          OutputsCalib[[uId]] <- OutputsCalib[[id]]
          # Copy parameters and transform X4 relatively to the sub-basin area
          OutputsCalib[[uId]]$ParamFinalR <-
            OutputsCalib[[uId]]$ParamFinalR[IM[[uId]]$model$indexParamUngauged]
          if(IM[[id]]$model$hasX4) {
            OutputsCalib[[uId]]$ParamFinalR[IM[[uId]]$model$iX4] <-
              X4 * (subBasinAreas[uId] / sum(subBasinAreas, na.rm = TRUE)) ^ 0.3
          }
        } else {
          OutputsCalib[[uId]] <- Calibration(
            InputsModel = IM[[uId]],
            RunOptions = RunOptions[[uId]],
            InputsCrit = IC,
            CalibOptions = CalibOptions[[uId]],
            ...
          )
        }
      }
      IM <- IM[[id]]
    }

    if(useUpstreamQsim) {
      # Run the model for the sub-basin
      OutputsModel[[id]] <- RunModel(
        x = IM,
        RunOptions = RunOptions[[id]],
        Param = OutputsCalib[[id]]$ParamFinalR
      )
    }

  }

  return(OutputsCalib)

}

#' Create InputsCrit for De Lavenne regularization
#'
#' Internal function that run [airGR::CreateInputsCrit_Lavenne] on-the-fly with a priori upstream
#' sub-catchment parameters grabbed during network calibration process.
#'
#' @param id [character] the id of the current sub-catchment
#' @param OutputsModel \[GRiwrmOutputsModel\] object with simulation results of upstream sub-catchments run with calibrated parameters
#' @param InputsCrit \[InputsCritLavenneFunction\] object internally created by [CreateInputsCrit.GRiwrmInputsModel]
#'
#' @return \[InputsCrit\] object with De Lavenne regularization
#' @import airGR
#' @noRd
#'
getInputsCrit_Lavenne <- function(id, OutputsModel, InputsCrit) {
  if (!inherits(InputsCrit[[id]], "InputsCritLavenneFunction")) {
    stop("'InputsCrit[[id]]' must be of class InputsCritLavenneFunction")
  }
  AprioriId <- attr(InputsCrit[[id]], "AprioriId")
  AprCelerity <- attr(InputsCrit[[id]], "AprCelerity")
  Lavenne_FUN <- attr(InputsCrit[[id]], "Lavenne_FUN")
  AprParamR <- OutputsModel[[AprioriId]]$RunOptions$Param
  if(!inherits(OutputsModel[[AprioriId]], "SD")) {
    # Add default velocity parameter for a priori upstream catchment
    AprParamR <- c(AprCelerity, AprParamR)
  }
  if (attr(InputsCrit[[id]], "model")$hasX4) {
    featMod <- attr(InputsCrit[[id]], "model")
    AprParamR[featMod$iX4] <- AprParamR[featMod$iX4] * featMod$X4Ratio
  }
  AprCrit <- ErrorCrit(InputsCrit[[AprioriId]], OutputsModel[[AprioriId]])$CritValue
  return(Lavenne_FUN(AprParamR, AprCrit))
}


#' Reduce a GRiwrm list object (InputsModel, RunOptions...) for a reduced network
#'
#' @param griwrm See [CreateGRiwrm])
#' @param obj Either a *GRiwrmInputsModel*, *GRiwrmOptions*... object
#'
#' @return The object containing only nodes of the reduced model
#' @noRd
reduceGRiwrmObj4Ungauged <- function(griwrm, obj) {
  objAttributes <- attributes(obj)
  obj <- lapply(obj, function(o) {
    if(o$id %in% griwrm$id && !is.na(griwrm$model[griwrm$id == o$id])) {
      o
    } else {
      NULL
    }
  })
  obj[sapply(obj, is.null)] <- NULL
  objAttributes$names <- names(obj)
  attributes(obj) <- objAttributes
  return(obj)
}

updateParameters4Ungauged <- function(GaugedId,
                                      InputsModel,
                                      RunOptions,
                                      CalibOptions,
                                      OutputsModel,
                                      useUpstreamQsim) {

  ### Set the reduced network of the basin containing ungauged nodes ###
  # Select nodes identified with the current node as gauged node
  griwrm <- attr(InputsModel, "GRiwrm")
  gDonor <- griwrm[griwrm$donor == GaugedId, ]
  # Add upstream nodes for routing upstream flows
  upIds <- griwrm$id[griwrm$down %in% gDonor$id & !griwrm$id %in% gDonor$id]
  g <- rbind(griwrm[griwrm$id %in% upIds, ], gDonor)
  g$model[g$id %in% upIds] <- NA
  # Set downstream node
  g$down[!g$down %in% g$id] <- NA

  ### Modify InputsModel for the reduced network ###
  # Remove nodes outside of reduced network
  InputsModel <- reduceGRiwrmObj4Ungauged(g, InputsModel)
  # Copy fixed parameters for Reservoirs
  for (id in names(InputsModel)) {
    if (InputsModel[[id]]$isReservoir) {
      InputsModel[[id]]$FixedParam <- CalibOptions[[id]]$FixedParam
    }
  }
  # Update griwrm
  attr(InputsModel, "GRiwrm") <- g
  # Update Qupstream already modeled in the reduced network upstream nodes
  idIM <- unique(g$down[g$id %in% upIds])
  for (id in idIM) {
    if(useUpstreamQsim && any(InputsModel[[id]]$UpstreamIsModeled)) {
      # Temporarily switch off upstream nodes belonging to the donor basin
      UpIsModeledBackUp <- InputsModel[[idIM]]$UpstreamIsModeled
      ImUpIds <- InputsModel[[idIM]]$UpstreamNodes
      InputsModel[[idIM]]$UpstreamIsModeled[!ImUpIds %in% upIds] <- FALSE
      # Update InputsModel$Qupstream with simulated upstream flows
      InputsModel[[idIM]] <- UpdateQsimUpstream(InputsModel[[idIM]],
                                              RunOptions[[idIM]],
                                              OutputsModel)
      # Restore initial UpstreamIsModeled and switch off already modelled nodes
      InputsModel[[idIM]]$UpstreamIsModeled <- UpIsModeledBackUp
      InputsModel[[idIM]]$UpstreamIsModeled[ImUpIds %in% upIds] <- FALSE
    }
  }

  # Add class InputsModel for airGR::Calibration checks
  class(InputsModel) <- c("InputsModel", class(InputsModel))

  ### Modify RunOptions for the reduced network ###
  RunOptions <- reduceGRiwrmObj4Ungauged(g, RunOptions)
  return(list(InputsModel = InputsModel, RunOptions = RunOptions))
}


#' Compute the area of downstream sub-basins
#'
#' @param IM *GRiwrmInputsModel* object (See [CreateInputsModel.GRiwrm])
#'
#' @return [numeric] named [vector] of the area of the downstream sub-basins
#' @noRd
calcSubBasinAreas <- function(IM) {
  unlist(
    sapply(IM, function(x) {
      if(is.list(x)) as.numeric(x$BasinAreas[length(x$BasinAreas)])})
  )
}


#' RunModel for a sub-network of ungauged nodes
#'
#' The function simulates a network with one set of parameters
#' shared with ungauged nodes inside the basin.
#'
#' @details
#' The network should contains only one gauged station at downstream and other
#' nodes can be direct injection or ungauged nodes.
#'
#' This function works as functions similar to [airGR::RunModel_GR4J] except that
#' `InputsModel` is a *GRiwrmInputsModel* containing the network of ungauged nodes
#' and direct injection in the basin.
#'
#' `Param` is adjusted for each sub-basin using the method developed by
#' Lobligeois (2014) for GR models.
#'
#' @references Lobligeois, Florent. Mieux connaître la distribution spatiale des
#' pluies améliore-t-il la modélisation des crues ? Diagnostic sur 181 bassins
#' versants français. Phdthesis, AgroParisTech, 2014.
#' <https://pastel.archives-ouvertes.fr/tel-01134990/document>
#'
#' @inheritParams airGR::RunModel
#'
#' @inherit RunModel.GRiwrmInputsModel return return
#' @noRd
RunModel_Ungauged <- function(InputsModel, RunOptions, Param) {
  InputsModel$FUN_MOD <- NULL
  SBVI <- sum(calcSubBasinAreas(InputsModel), na.rm = TRUE)
  # Compute Param for each sub-basin
  P <- lapply(InputsModel, function(IM) {
    if (IM$isReservoir) {
      return(IM$FixedParam)
    }
    p <- Param[IM$model$indexParamUngauged]
    if(IM$model$hasX4) {
      p[IM$model$iX4] <- Param[IM$model$iX4] * (IM$BasinAreas[length(IM$BasinAreas)] / SBVI) ^ 0.3
    }
    return(p)
  })
  OM <- suppressMessages(
    RunModel.GRiwrmInputsModel(InputsModel, attr(RunOptions, "GRiwrmRunOptions"), P)
  )
  return(OM[[length(OM)]])
}
