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
  if (!inherits(OutputsModel[[AprioriId]], "SD")) {
    # Add Celerity parameter if apriori is an upstream node
    AprParamR <- c(AprCelerity, AprParamR)
  }
  featMod <- attr(InputsCrit[[id]], "model")
  if (featMod$hasX4) {
    AprParamR[featMod$iX4] <- AprParamR[featMod$iX4] * featMod$X4Ratio
  }
  AprParamR <- AprParamR[featMod$indexParamUngauged]
  message("A priori parameters from node ", AprioriId, ": ", paste(round(AprParamR, 3), collapse = ", "))
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
updateParameters4Ungauged <- function(GaugedId,
                                      InputsModel,
                                      RunOptions,
                                      CalibOptions,
                                      OutputsModel,
                                      useUpstreamQsim) {

  ### Set the reduced network of the basin containing ungauged nodes ###
  # Select nodes identified with the current node as donor gauged node
  griwrm <- attr(InputsModel, "GRiwrm")
  donorIds <- griwrm$id[!is.na(griwrm$donor) & griwrm$donor == GaugedId]
  gDonor <- griwrm %>% dplyr::filter(.data$id %in% donorIds)
  # Add upstream nodes for routing upstream flows
  upNodes <- griwrm %>%
    dplyr::filter(.data$down %in% gDonor$id,
                  !.data$id %in% gDonor$id) %>%
    dplyr::mutate(model = ifelse(!is.na(.data$model), NA, .data$model))
  upIds <- upNodes$id
  g <- rbind(upNodes, gDonor)
  # Set downstream nodes
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
    if (useUpstreamQsim && any(InputsModel[[id]]$UpstreamIsModeled)) {
      # Temporarily switch off upstream nodes belonging to the donor basin
      UpIsModeledBackUp <- InputsModel[[id]]$UpstreamIsModeled
      ImUpIds <- InputsModel[[id]]$UpstreamNodes
      InputsModel[[id]]$UpstreamIsModeled[!ImUpIds %in% upIds] <- FALSE
      # Update InputsModel$Qupstream with simulated upstream flows
      InputsModel[[id]] <- UpdateQsimUpstream(InputsModel[[id]],
                                              RunOptions[[id]],
                                              OutputsModel)
      # Restore initial UpstreamIsModeled and switch off already modeled nodes
      InputsModel[[id]]$UpstreamIsModeled <- UpIsModeledBackUp
      InputsModel[[id]]$UpstreamIsModeled[ImUpIds %in% upIds] <- FALSE
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
      if (is.list(x)) as.numeric(x$BasinAreas[length(x$BasinAreas)])})
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
#' <https://pastel.hal.science/tel-01134990/document>
#'
#' @inheritParams airGR::RunModel
#' @param ouput.all [logical] if `TRUE` returns the output of [RunModel.GRiwrm],
#' returns the `OutputsModel` of the downstream node otherwise
#'
#' @inherit RunModel.GRiwrmInputsModel return return
#' @noRd
RunModel_Ungauged <- function(InputsModel, RunOptions, Param, output.all = FALSE) {
  InputsModel$FUN_MOD <- NULL
  donor <- RunOptions$id
  # Compute Param for each sub-basin
  P <- lapply(InputsModel, function(IM) {
    if (IM$id == donor) return(Param)
    if (IM$isReservoir) {
      return(IM$FixedParam)
    }
    return(transferGRparams(InputsModel, Param, donor, IM$id))
  })
  OM <- suppressMessages(
    RunModel.GRiwrmInputsModel(InputsModel, attr(RunOptions, "GRiwrmRunOptions"), P)
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
#' to ungauged nodes and
#'
#' @details
#' `donor` and `receiver` nodes should have the same GR model with the same snow
#' module configuration.
#'
#' The transfer takes care of:
#' - the presence/absence of hydraulic routing parameters between the donor and the receiver
#' - the transformation of the X4 parameters of GR models
#'
#' @param InputsModel A *GRiwrmInputsModel* object (See [CreateInputsModel.GRiwrm])
#' @param Param [numeric] vector of GR model parameters
#' @param donor [character] id of the node which gives its parameters
#' @param receiver [character] id of the node which receives the parameters from the donor
#' @param default_param [numeric] vector of GR model parameters if parameters are missing from the donor
#'
#' @return A [numeric] [vector] with transferred parameters
#' @export
#'
transferGRparams <- function(InputsModel, Param, donor, receiver, default_param = NULL) {
  missing_params <- setdiff(InputsModel[[receiver]]$model$indexParamUngauged,
                            InputsModel[[donor]]$model$indexParamUngauged)
  if (length(missing_params) > 0) {
    if (is.null(default_param)) {
      stop("Missing parameters in transfer between nodes '",
           donor, "' and '", receiver, "'\n",
           "Fix the missing parameters with the argument `FixedParam` of `CreateCalibOptions`")
    }
    max_params <- max(
      max(InputsModel[[receiver]]$model$indexParamUngauged),
      max(InputsModel[[donor]]$model$indexParamUngauged)
    )
    if (length(default_param) < max_params) {
      stop("Error in parameter transfer between nodes '", donor, "' and '",
           receiver, "'\n`default_params` should have a minimum length of ", max_params)
    }
    Param2 <- rep(as.numeric(NA), length(InputsModel[[receiver]]$model$indexParamUngauged))
    Param2[InputsModel[[donor]]$model$indexParamUngauged] <- Param
    Param2[missing_params] <- default_param[missing_params]
    Param <- Param2
  }

  p <- Param
  if (length(Param) > length(InputsModel[[receiver]]$model$indexParamUngauged)) {
    # Transfer from intermediate node to upstream node
    p <- p[InputsModel[[receiver]]$model$indexParamUngauged]
  }

  if (InputsModel[[receiver]]$model$hasX4) {
    donor_area <- InputsModel[[donor]]$BasinAreas[length(InputsModel[[donor]]$BasinAreas)]
    receiver_area <- InputsModel[[receiver]]$BasinAreas[length(InputsModel[[receiver]]$BasinAreas)]
    p[InputsModel[[receiver]]$model$iX4] <- max(
      Param[InputsModel[[donor]]$model$iX4] *
        (receiver_area / donor_area) ^ 0.3,
      0.5
    )
  }
  return(p)
}

#' Extract calibrated parameters
#'
#' Extract [list] of parameters from the output of [Calibration.GRiwrmInputsModel]
#' which can be directly used as argument `Param` of [RunModel.GRiwrmInputsModel]
#' and [RunModel.Supervisor].
#'
#' @details
#' See vignettes and example of [RunModel_Reservoir] for examples of use.
#'
#' @param x A *GRiwrmOutputsModel* object returned by [Calibration.GRiwrmInputsModel]
#'
#' @return A named [list] of [numeric] [vector] containing the calibrated parameters
#' of each modeled node.
#'
#' @seealso [Calibration], [RunModel.GRiwrmInputsModel], [RunModel.Supervisor]
#'
#' @export
#'
extractParam <- function(x) {
  stopifnot(inherits(x, "GRiwrmOutputsCalib"))
  lapply(x, "[[", "ParamFinalR")
}
