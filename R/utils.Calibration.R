#' Create InputsCrit for De Lavenne regularization
#'
#' Internal function that runs [airGR::CreateInputsCrit_Lavenne] on-the-fly with a priori upstream
#' sub-catchment parameters grabbed during network calibration process.
#'
#' @param id [character] the id of the current sub-catchment
#' @param OutputsModel \[GRiwrmOutputsModel\] object with simulation results of upstream sub-catchments run with calibrated parameters
#' @param InputsCrit \[InputsCritLavenneFunction\] object internally created by [CreateInputsCrit.GRiwrmInputsModel]
#'
#' @return \[InputsCrit\] object with de Lavenne regularization
#' @import airGR
#' @noRd
#'
getInputsCrit_Lavenne <- function(
  InputsModel,
  RunOptions,
  OutputsModel,
  InputsCrit
) {
  id <- InputsModel$id
  if (!inherits(InputsCrit, "InputsCritLavenneFunction")) {
    stop("'InputsCrit' must be of class InputsCritLavenneFunction")
  }
  AprioriId <- attr(InputsCrit, "AprioriId")
  AprCelerity <- attr(InputsCrit, "AprCelerity")
  Lavenne_FUN <- attr(InputsCrit, "Lavenne_FUN")
  AprParamR <- OutputsModel[[AprioriId]]$RunOptions$Param
  if (!inherits(OutputsModel[[AprioriId]], "SD")) {
    # Add Celerity parameter if apriori is an upstream node
    AprParamR <- c(AprCelerity, AprParamR)
  }
  featMod <- attr(InputsCrit, "model")
  if (featMod$hasX4) {
    AprParamR[featMod$iX4] <- AprParamR[featMod$iX4] * featMod$X4Ratio
  }
  AprParamR <- AprParamR[featMod$indexParamUngauged]
  message(
    "Parameter regularization: get a priori parameters from node ",
    AprioriId,
    ": ",
    paste(round(AprParamR, 3), collapse = ", ")
  )
  OM_Apriori <- RunModel(InputsModel, RunOptions, AprParamR)
  AprCrit <- ErrorCrit(InputsCrit, OM_Apriori)$CritValue
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
    if (o$id %in% griwrm$id && any(!is.na(griwrm$model[griwrm$id == o$id]))) {
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

#' Set a reduced GRiwrm network for calibration of a sub-network with ungauged
#' hydrological nodes
#'
#' @inheritParams Calibration
#' @param GaugedId [character] Id of the gauged node
#' @param OutputsModel *GRiwrmOutputsModel* of the complete network
#'
#' @return A [list] containing the following items:
#' - `InputsModel`: a *GRiwrmInputsModel* of the reduced network
#' - `RunOptions`: a *GRiwrmRunOptions* of the reduced network
#' @noRd
#' @importFrom dplyr "%>%"
#' @importFrom rlang .data
#'
updateParameters4Ungauged <- function(
  GaugedId,
  InputsModel,
  RunOptions,
  CalibOptions,
  OutputsModel,
  useUpstreamQsim
) {
  g <- getUngaugedCluster(attr(InputsModel, "GRiwrm"), GaugedId)

  ### Modify InputsModel for the reduced network ###
  # Remove nodes outside of reduced network
  InputsModel <- reduceGRiwrmObj4Ungauged(g, InputsModel)
  # Copy fixed parameters for Reservoirs or other models
  for (id in names(InputsModel)) {
    if (id != GaugedId && InputsModel[[id]]$gaugedId == id) {
      if (any(is.na(CalibOptions[[id]]$FixedParam))) {
        stop(
          "Node '",
          id,
          "' located inside the ungauged node cluster '",
          GaugedId,
          "' must have its parameters fixed.\n",
          "Fix its parameters by assigning values to :",
          " `CalibOptions[['",
          id,
          "']]$FixedParam`"
        )
      }
      InputsModel[[id]]$FixedParam <- CalibOptions[[id]]$FixedParam
    }
  }
  # Update griwrm
  attr(InputsModel, "GRiwrm") <- g
  # Update Qupstream already modeled in the reduced network upstream nodes
  upIds <- attr(g, "upIds")
  idIM <- unique(g$down[g$id %in% upIds])
  for (id in idIM) {
    if (useUpstreamQsim && any(InputsModel[[id]]$UpstreamIsModeled)) {
      # Temporarily switch off upstream nodes belonging to the donor basin
      UpIsModeledBackUp <- InputsModel[[id]]$UpstreamIsModeled
      ImUpIds <- InputsModel[[id]]$UpstreamNodes
      InputsModel[[id]]$UpstreamIsModeled[!ImUpIds %in% upIds] <- FALSE
      # Update InputsModel$Qupstream with simulated upstream flows
      InputsModel[[id]] <- UpdateQsimUpstream(
        InputsModel[[id]],
        RunOptions[[id]],
        OutputsModel
      )
      # Restore initial UpstreamIsModeled and switch off already modeled nodes
      InputsModel[[id]]$UpstreamIsModeled <- UpIsModeledBackUp
      InputsModel[[id]]$UpstreamIsModeled[ImUpIds %in% upIds] <- FALSE
    }
  }

  # Add class RunModel_Ungauged and InputsModel for preprocessind
  # and processing airGR::Calibration
  class(InputsModel) <- c("Ungauged", "InputsModel", class(InputsModel))

  ### Modify RunOptions for the reduced network ###
  RunOptions <- reduceGRiwrmObj4Ungauged(g, RunOptions)
  return(list(InputsModel = InputsModel, RunOptions = RunOptions))
}


#' Compute the area of downstream sub-basins
#'
#' @param IM *GRiwrmInputsModel* object (See [CreateInputsModel.GRiwrm])
#'
#' @return [\numeric\] named [vector] of the area of the downstream sub-basins
#' @noRd
calcSubBasinAreas <- function(IM) {
  unlist(
    sapply(IM, function(x) {
      if (is.list(x)) as.numeric(x$BasinAreas[length(x$BasinAreas)])
    })
  )
}


#' RunModel for a sub-network of ungauged nodes
#'
#' The function simulates a network with one set of parameters
#' shared with ungauged nodes inside the basin.
#'
#' @details
#' The network should contain only one gauged station at downstream and other
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
#' <https://pastel.hal.science/tel-01134990/document>
#'
#' @inheritParams airGR::RunModel
#' @param ouput.all [logical] if `TRUE` returns the output of [RunModel.GRiwrmInputsModel],
#' returns the `OutputsModel` of the downstream node otherwise
#'
#' @inherit RunModel.GRiwrmInputsModel return return
#' @noRd
RunModel_Ungauged <- function(
  InputsModel,
  RunOptions,
  Param,
  output.all = FALSE
) {
  InputsModel$FUN_MOD <- NULL
  donor <- RunOptions$id
  # Compute Param for each sub-basin
  P <- lapply(InputsModel, function(IM) {
    if (IM$id == donor) {
      return(Param)
    } else if (IM$gaugedId == donor) {
      # Ungauged nodes
      return(transferGRparams(InputsModel, Param, donor, IM$id))
    } else {
      # Nodes with fixed params (Reservoir or other model with fixed params)
      return(IM$FixedParam)
    }
  })
  OM <- suppressMessages(
    RunModel.GRiwrmInputsModel(
      InputsModel,
      attr(RunOptions, "GRiwrmRunOptions"),
      P
    )
  )
  if (output.all) {
    return(OM)
  } else {
    return(OM[[length(OM)]])
  }
}

#' Transfer GR parameters from one donor sub-basin to a receiver sub-basin
#'
#' This function is used by `Calibration.GRiwrmInputsModel` for transferring parameters
#' to ungauged nodes and for providing a priori parameters in parameter
#' regularization (See [CreateInputsCrit]).
#'
#' @details
#' `donor` and `receiver` nodes should have the same GR model with the same snow
#' module configuration.
#'
#' The transfer takes care of:
#' - the presence/absence of hydraulic routing parameters between the donor and
#' the receiver (But `default_param` should be provided if the receiver has more
#' parameters than the donor)
#' - the transformation of the X4 parameters of GR models. Indeed, this parameter
#' is correlated to the catchment area.
#' Therefore, it has to be rescaled from the surface of the donor catchment to
#' the surface of the receiver catchment, following the relationship suggested
#' by Lobligeois (2014):
#' \eqn{X4_{receiver} = X4_{donor} \times (S_{receiver} / S_{donor})^{0.3}}.
#'
#' @param InputsModel Object of class [GRiwrmInputsModel][CreateInputsModel.GRiwrm],
#' see [CreateInputsModel.GRiwrm] for details
#' @param Param [numeric] vector of GR model parameters
#' @param donor [character] id of the node which gives its parameters
#' @param receiver [character] id of the node which receives the parameters from the donor
#' @param default_param [numeric] vector of GR model parameters if parameters are missing from the donor
#' @param verbose [logical] Add information message on donor and receiver
#'
#' @references
#' Lobligeois, F., 2014. Mieux connaitre la distribution spatiale des pluies améliore-t-il la modélisation des crues ?
#' Diagnostic sur 181 bassins versants français. Thèse de Doctorat, Irstea (Antony), AgroParisTech (Paris), 312 pp.
#' https://webgr.inrae.fr/Media/Files/biblio-theses/2014_lobligeois_these.pdf
#'
#' @return A [numeric] [vector] of transferred parameters
#' @export
#'
transferGRparams <- function(
  InputsModel,
  Param,
  donor,
  receiver,
  default_param = NULL,
  verbose = FALSE
) {
  missing_params <- setdiff(
    InputsModel[[receiver]]$model$indexParamUngauged,
    InputsModel[[donor]]$model$indexParamUngauged
  )
  if (verbose) {
    message(
      "Tranferring parameters from node '",
      donor,
      "' to node '",
      receiver,
      "'"
    )
  }
  if (length(missing_params) > 0) {
    if (is.null(default_param)) {
      stop(
        "Missing parameters in transfer between nodes '",
        donor,
        "' and '",
        receiver,
        "'\n",
        "Fix the missing parameters with the argument `FixedParam` of `CreateCalibOptions`"
      )
    }
    max_params <- max(
      max(InputsModel[[receiver]]$model$indexParamUngauged),
      max(InputsModel[[donor]]$model$indexParamUngauged)
    )
    if (length(default_param) < max_params) {
      stop(
        "Error in parameter transfer between nodes '",
        donor,
        "' and '",
        receiver,
        "'\n`default_params` should have a minimum length of ",
        max_params
      )
    }
    Param2 <- rep(
      as.numeric(NA),
      length(InputsModel[[receiver]]$model$indexParamUngauged)
    )
    Param2[InputsModel[[donor]]$model$indexParamUngauged] <- Param
    Param2[missing_params] <- default_param[missing_params]
    Param <- Param2
  }

  p <- Param
  if (
    length(Param) > length(InputsModel[[receiver]]$model$indexParamUngauged)
  ) {
    # Transfer from intermediate node to upstream node
    p <- p[InputsModel[[receiver]]$model$indexParamUngauged]
  }

  if (InputsModel[[receiver]]$model$hasX4) {
    donor_area <- InputsModel[[donor]]$BasinAreas[length(
      InputsModel[[donor]]$BasinAreas
    )]
    receiver_area <- InputsModel[[receiver]]$BasinAreas[length(
      InputsModel[[receiver]]$BasinAreas
    )]
    p[InputsModel[[receiver]]$model$iX4] <- max(
      Param[InputsModel[[donor]]$model$iX4] *
        (receiver_area / donor_area)^0.3,
      0.5
    )
  }
  if (verbose) {
    message(message(
      "\t     Param = ",
      paste(sprintf("%8.3f", p), collapse = ", ")
    ))
  }
  return(p)
}

#' Extract calibrated parameters
#'
#' Extract [list] of parameters from the output of [Calibration.GRiwrmInputsModel]
#' which can be directly used as argument `Param` of [RunModel.GRiwrmInputsModel],
#' [RunModel.Supervisor], and [RunModel.GRiwrmOutputsModel].
#'
#' @details
#' See vignettes and example of [RunModel_Reservoir] for examples of use.
#'
#' @param x Object of class [GRiwrmOutputsModel][Calibration.GRiwrmInputsModel] returned by [Calibration.GRiwrmInputsModel]
#'
#' @return A [list] containing representing the calibrated parameters
#' of each modeled node as a [numeric] [vector].
#'
#' @seealso [Calibration], [RunModel.GRiwrmInputsModel], [RunModel.Supervisor]
#'
#' @export
#'
extractParam <- function(x) {
  UseMethod("extractParam")
}

#' @export
#' @rdname extractParam
extractParam.GRiwrmOutputsCalib <- function(x) {
  lapply(x, "[[", "ParamFinalR")
}

#' @export
#' @rdname extractParam
extractParam.GRiwrmOutputsModel <- function(x) {
  lapply(x, function(o) o$RunOptions$Param)
}
