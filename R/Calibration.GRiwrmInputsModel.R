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
    message("Calibration.GRiwrmInputsModel: Treating sub-basin ", id, "...")

    if (inherits(InputsCrit[[id]], "InputsCritLavenneFunction")) {
      IC <- getInputsCrit_Lavenne(id, OutputsModel, InputsCrit)
    } else {
      IC <- InputsCrit[[id]]
    }
    hasUngauged <- IM$hasUngauged
    if (hasUngauged) {
      Sdown <- IM$BasinAreas[length(IM$BasinAreas)]
      l  <- updateParameters4Ungauged(id,
                                      InputsModel,
                                      RunOptions,
                                      OutputsModel,
                                      useUpstreamQsim)
      IM <- l$InputsModel
      IM$FUN_MOD <- "RunModel_Ungauged"
      attr(RunOptions[[id]], "GRiwrmRunOptions") <- l$RunOptions
    } else {
      if(useUpstreamQsim && any(IM$UpstreamIsRunoff)) {
        # Update InputsModel$Qupstream with simulated upstream flows
        IM <- UpdateQsimUpstream(IM, RunOptions[[id]], OutputsModel)
      }
    }

    OutputsCalib[[id]] <- Calibration(
      InputsModel = IM,
      RunOptions = RunOptions[[id]],
      InputsCrit = IC,
      CalibOptions = CalibOptions[[id]],
      ...
    )

    if (hasUngauged) {
      # Add OutputsCalib for ungauged nodes
      g <- attr(InputsModel, "GRiwrm")
      ungaugedIds <- g$id[g$gauged == id & g$id != id & !is.na(g$model)]
      for (uId in ungaugedIds) {
        OutputsCalib[[uId]] <- OutputsCalib[[id]]
        PS <- attr(IM[[uId]], "ParamSettings")
        OutputsCalib[[uId]]$ParamFinalR <- OutputsCalib[[uId]]$ParamFinalR[PS$Indexes]
        if(PS$hasX4) {
          OutputsCalib[[uId]]$ParamFinalR[PS$iX4] <-
            OutputsCalib[[uId]]$ParamFinalR[PS$iX4] * (sum(IM[[uId]]$BasinAreas) / Sdown) ^ 0.3
        }
      }
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
    if(o$id %in% griwrm$id) {
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
                                      OutputsModel,
                                      useUpstreamQsim) {

  ### Set the reduced network of the basin containing ungauged nodes ###
  # Select nodes identified with the current node as gauged node
  griwrm <- attr(InputsModel, "GRiwrm")
  g <- griwrm[griwrm$gauged == GaugedId, ]
  # Add upstream nodes for routing upstream flows
  upIds <- griwrm$id[griwrm$down %in% g$id & !griwrm$id %in% g$id]
  g <- rbind(griwrm[griwrm$id %in% upIds, ], g)
  g$model[g$id %in% upIds] <- NA
  # Set downstream node
  g$down[!g$down %in% g$id] <- NA

  ### Modify InputsModel for the reduced network ###
  # Remove nodes outside of reduced network
  InputsModel <- reduceGRiwrmObj4Ungauged(g, InputsModel)
  # Update griwrm
  attr(InputsModel, "GRiwrm") <- g
  # Update Qupstream of reduced network upstream nodes
  g2 <- griwrm[griwrm$gauged == GaugedId,]
  upIds2 <- g2$id[!g2$id %in% g2$down]
  for (id in upIds2) {
    if(useUpstreamQsim && any(InputsModel[[id]]$UpstreamIsRunoff)) {
      # Update InputsModel$Qupstream with simulated upstream flows
      InputsModel[[id]] <- UpdateQsimUpstream(InputsModel[[id]],
                                              RunOptions[[id]],
                                              OutputsModel)
      InputsModel[[id]]$UpstreamIsRunoff <-
        rep(FALSE, length(InputsModel[[id]]$UpstreamIsRunoff))
    }
  }
  # Add extra info for Param processing
  nbParam <- RunOptions[[GaugedId]]$FeatFUN_MOD$NbParam
  for (id in names(InputsModel)) {
    attr(InputsModel[[id]], "ParamSettings") <-
      list(Indexes = ifelse(inherits(InputsModel[[id]], "SD"), 1, 2):nbParam,
           hasX4 = grepl("RunModel_GR[456][HJ]", InputsModel[[id]]$FUN_MOD),
           iX4 = ifelse(inherits(InputsModel[[id]], "SD"), 5, 4))
  }
  # Add class InputsModel for airGR::Calibration checks
  class(InputsModel) <- c("InputsModel", class(InputsModel))

  ### Modify RunOptions for the reduced network ###
  RunOptions <- reduceGRiwrmObj4Ungauged(g, RunOptions)
  return(list(InputsModel = InputsModel, RunOptions = RunOptions))
}

updateRunOptions4Ungauged <- function(id, RunOptions) {
  # Remove nodes outside of reduced network
  RunOptions <- lapply(RunOptions, function(RO) {
    if(RO$id %in% g$id) {
      IM
    } else {
      NULL
    }
  })
  return(RunOptions)
}


#' RunModel for a sub-network of ungauged nodes
#'
#' The function simulates a network with one set of parameters
#' shared with ungauded nodes inside the basin.
#'
#' @details
#' The network should contains only one gauged station at downstream and other
#' nodes can be direct injection or ungauged nodes.
#'
#' This function works as functions similar to [airGR::RunModel_GR4J] except that
#' `InputsModel` is a *GRiwrmInputsModel* containing the network of ungauged nodes
#' and direct injection in the basin.
#'
#' `Param` is adjusted for each sub-basin using the method developped by
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
  SBV <- sum(InputsModel[[length(InputsModel)]]$BasinAreas)
  # Compute Param for each sub-basin
  P <- lapply(InputsModel, function(IM) {
    PS <- attr(IM, "ParamSettings")
    p <- Param[PS$Indexes]
    if(PS$hasX4) {
      p[PS$iX4] <- Param[PS$iX4] * (IM$BasinAreas[length(IM$BasinAreas)] / SBV) ^ 0.3
    }
    return(p)
  })
  OM <- suppressMessages(
    RunModel.GRiwrmInputsModel(InputsModel, attr(RunOptions, "GRiwrmRunOptions"), P)
  )
  return(OM[[length(OM)]])
}
