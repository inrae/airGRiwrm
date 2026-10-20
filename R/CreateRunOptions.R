#' Creation of the RunOptions object
#'
#' This function can be used either for a catchment (with an \emph{InputsModel} object) or for a network (with a \emph{GRiwrmInputsModel} object)
#'
#' @template param_x
#' @param InputsModel object of class \emph{InputsModel} (only used to be consistent
#'        with the original [airGR::CreateRunOptions] which has `FUN_MOD` as first
#'        parameter)
#'        see [airGR::CreateInputsModel] for details
#' @param ... arguments passed to [airGR::CreateRunOptions], see details
#'
#' @details See [airGR::CreateRunOptions] documentation for a complete list of arguments.
#'
#' If `x` argument is a \emph{GRiwrmInputsModel} object, `IniStates` must be a
#' list of [numeric] object of class \emph{IniStates} with one item per modeled sub-catchment.
#'
#' With a \emph{GRiwrmInputsModel} object, all arguments are applied on each
#' sub-catchments of the network.
#'
#' For examples of use see topics [RunModel.GRiwrmInputsModel], [RunModel_Reservoir],
#' and [RunModel.Supervisor].
#'
#' @return Depending on the class of `InputsModel` argument (respectively
#' \emph{InputsModel} and \emph{GRiwrmInputsModel} object), the returned value is respectively:
#' - a `RunOptions` object (See [airGR::CreateRunOptions])
#' - a `GRiwrmRunOptions` object which is a [list] of `RunOptions` objects with one item per modeled sub-catchment
#'
#' @rdname CreateRunOptions
#' @export
#' @seealso [CreateGRiwrm()], [CreateInputsModel.GRiwrm()], [RunModel.GRiwrmInputsModel()]
CreateRunOptions <- function(x, ...) {
  UseMethod("CreateRunOptions", x)
}

# Minimal default Outputs_Sim: the outputs required to propagate simulated
# flows to downstream nodes (Qsim, Qsim_m3), to warm-up semi-distributed
# nodes correctly (WarmUpQsim), to carry the model parameters (Param) and
# the model states at the end of the run (StateEnd), which are needed for
# De Lavenne parameter regularization and hot restart (#141).
MinimalOutputsSim <- c(
  "DatesR", "WarmUpQsim", "Qsim", "Qsim_m3", "QsimDown", "Param", "StateEnd"
)

# The CemaNeige layer output series computed by airGR's RunModel_CemaNeige
# (mirrors airGR:::.FortranOutputs(GR = NULL, isCN = TRUE)$CN).
CemaNeigeLayerOutputs <- c(
  "Pliq", "Psol", "SnowPack", "ThermalState", "Gratio",
  "PotMelt", "Melt", "PliqAndMelt", "Temp", "Gthreshold", "Glocalmax"
)

# "CemaNeigeLayers" is an airGRiwrm container name that stands for the full
# set of CemaNeige layer output series (CemaNeigeLayerOutputs). airGR does
# not know this name: it validates it in CreateRunOptions and selects the
# layer columns computed at run time through
# `FortranOutputsCemaNeige %in% RunOptions$Outputs_Sim` (a "CemaNeigeLayers"
# entry matches nothing, so no layer column would be computed). The
# container must therefore be expanded to its constituent names in every
# Outputs_* vector that is passed to airGR or used at run time.
# The full set must be requested: with a reduced set, the number of outputs
# sent to the CemaNeige Fortran routine (NOutputs) changes and airGR 1.7.9
# produces a different model run, which breaks the calibration of CemaNeige
# models with hysteresis (SCA-based criteria).
expand_CemaNeigeLayers <- function(outputs) {
  if (!is.character(outputs) || !any(outputs == "CemaNeigeLayers")) {
    return(outputs)
  }
  unique(c(setdiff(outputs, "CemaNeigeLayers"), CemaNeigeLayerOutputs))
}

#' @rdname CreateRunOptions
#' @export
CreateRunOptions.InputsModel <- function(x, ...) {
  dots <- CreateRunOptions_prepare_inputs(x, ...)
  # Add IsHyst in parameters if carried by InputsModel
  if (!is.null(x$model$IsHyst)) {
    dots$IsHyst <- x$model$IsHyst
  }
  # airGR 1.7.9 validates both Outputs_Sim and Outputs_Cal against the
  # model's supported outputs. Some names are computed by airGRiwrm
  # (not airGR) or are not in airGR's vocabulary at all:
  #   - Qsim_m3: always computed by airGRiwrm's complete_OutputsModel
  #   - Qsim, QsimDown: for Lag (routing) models, computed by
  #     airGRiwrm's RunModel_Lag_enhanced
  #   - QsimDown: only valid for SD inputs in airGR; airGRiwrm adds it for
  #     routing nodes regardless
  #   - CemaNeigeLayers: airGRiwrm container name, rejected by airGR
  #     CreateRunOptions for CemaNeige models; expanded to its constituent
  #     columns by expand_CemaNeigeLayers.
  # The expanded vectors are also restored in the returned object (instead
  # of the raw request), because airGR's RunModel_CemaNeige selects the
  # layer columns computed at run time from RunOptions$Outputs_Sim.
  Outputs_Sim_full <- dots$Outputs_Sim
  Outputs_Cal_full <- dots$Outputs_Cal
  dots$Outputs_Sim <- setdiff(
    expand_CemaNeigeLayers(Outputs_Sim_full),
    c("Qsim_m3", "QsimDown")
  )
  if (identical(dots$FUN_MOD, "RunModel_Lag")) {
    # Lag model doesn't support Qsim in airGR either
    dots$Outputs_Sim <- setdiff(dots$Outputs_Sim, "Qsim")
  }
  dots$Outputs_Cal <- expand_CemaNeigeLayers(Outputs_Cal_full)
  RunOptions <- CreateRunOptions_runAirGR(
    dots,
    "does not require .* Values? set to NA"
  )
  # Restore the requested outputs (with the CemaNeigeLayers container
  # expanded, see expand_CemaNeigeLayers) so the returned object drives
  # the run-time output selection.
  # The legacy string "all" must not be restored: airGR::CreateRunOptions
  # has already expanded it to the full explicit output vector, and
  # airGR::RunModel selects the run-time outputs from that vector (a
  # "all" string matches no output name and would drop every output).
  if (any(Outputs_Sim_full == "all")) {
    # Keep the full vector expanded by airGR::CreateRunOptions
  } else {
    RunOptions$Outputs_Sim <- expand_CemaNeigeLayers(Outputs_Sim_full)
  }
  # Only restore Outputs_Cal if it was explicitly set (non-NULL).
  # When not provided, airGR computes the appropriate default and
  # we should not overwrite it with NULL.
  if (!is.null(Outputs_Cal_full)) {
    if (any(Outputs_Cal_full == "all")) {
      # Keep the full vector expanded by airGR::CreateRunOptions
    } else {
      RunOptions$Outputs_Cal <- expand_CemaNeigeLayers(Outputs_Cal_full)
    }
  }
  return(RunOptions)
}

#' @rdname CreateRunOptions
#' @export
CreateRunOptions.character <- function(x, InputsModel, ...) {
  CreateRunOptions(x = InputsModel, FUN_MOD = x, ...)
}

#' @rdname CreateRunOptions
#' @export
CreateRunOptions.function <- function(x, InputsModel, ...) {
  CreateRunOptions(x = InputsModel, FUN_MOD = x, ...)
}

#' @rdname CreateRunOptions
#' @export
CreateRunOptions.RunModel_Reservoir <- function(x, ...) {
  dots <- CreateRunOptions_prepare_inputs(x, ...)
  # Bypass `Outputs_Sim` and `Outputs_Cal` because `RunModel_Reservoir` has
  # its own output set (Qinflows_m3, Vsim, Qsim_m3...) that is not in the
  # airGR generic list. Restore them in the returned object.
  Outputs_Sim <- dots$Outputs_Sim
  dots$Outputs_Sim <- "all"
  Outputs_Cal <- dots$Outputs_Cal
  dots$Outputs_Cal <- NULL
  # Bypass airGR::CreateRunOptions for reservoir models with `IniStates`
  # argument because it is not designed for this type of model and crashes
  warning_pattern <- "does not require .* Values? set to NA"
  if (!is.null(dots$IniStates)) {
    if (!is.numeric(dots$IniStates)) {
      stop("For reservoir models, `IniStates` must be a numeric vector")
    }
    IniStates <- dots$IniStates
    dots$IniStates <- NULL
    warning_pattern <- paste0(
      "(",
      warning_pattern,
      ")|model states initialisation not defined"
    )
  } else {
    IniStates <- NULL
  }
  RunOptions <- CreateRunOptions_runAirGR(dots, warning_pattern)
  if (!is.null(IniStates)) {
    RunOptions$IniStates <- IniStates
  }
  # Keep the full vector expanded by airGR::CreateRunOptions when the
  # legacy string "all" was requested (see CreateRunOptions.InputsModel)
  if (any(Outputs_Sim == "all")) {
    # keep the vector expanded by airGR::CreateRunOptions
  } else {
    RunOptions$Outputs_Sim <- Outputs_Sim
  }
  if (any(Outputs_Cal == "all")) {
    # keep the vector expanded by airGR::CreateRunOptions
  } else {
    RunOptions$Outputs_Cal <- Outputs_Cal
  }
  return(RunOptions)
}

CreateRunOptions_prepare_inputs <- function(x, ...) {
  dots <- list(...)
  dots$InputsModel <- x

  # Add FUN_MOD in parameters if carried by InputsModel
  FUN_MOD <- attr(x, "FeatFUN_MOD")$NameFunMod
  if ("FUN_MOD" %in% names(dots)) {
    if (!identical(match.fun(dots$FUN_MOD), match.fun(FUN_MOD))) {
      stop(
        "The parameter `FUN_MOD` differe from the one defined in `InputsModel`"
      )
    }
  }
  dots$FUN_MOD <- FUN_MOD

  # Set the minimal Outputs_Sim default when not explicitly provided by
  # user. This reduces memory by avoiding "all" which returns every
  # internal simulation variable (#141). Qsim_m3 and QsimDown are stripped
  # before calling airGR (they are airGRiwrm-specific), and restored
  # afterwards.
  if (!"Outputs_Sim" %in% names(dots)) {
    dots$Outputs_Sim <- MinimalOutputsSim
  }

  # For CemaNeige models, ensure Outputs_Sim includes "CemaNeigeLayers"
  # and "Precip", and Outputs_Cal includes "Gratio".
  # The "CemaNeigeLayers" container is expanded to the full set of
  # CemaNeige layer outputs in CreateRunOptions.InputsModel (see there for
  # why a reduced set must not be sent to airGR); SCA-based error criteria
  # then read OutputsModel$CemaNeigeLayers[[i]]$Gratio.
  # "Gratio" must also be in Outputs_Cal because airGR calibration
  # replaces Outputs_Sim with Outputs_Cal before the screening runs.
  # "Precip" is forced in Outputs_Sim because the default plot of a
  # CemaNeige node (airGR::plot.OutputsModel) requires it in the
  # OutputsModel to compute the precipitation panel axis as soon as the
  # per-layer solid precipitation ("Psol") is present (upstream airGR
  # `ylimP` limitation: with "Psol" but without "Precip", the default plot
  # crashes with "object 'ylimP' not found"). "Precip" is a recorded meteo
  # input, not a Fortran CemaNeige output: the CemaNeige NOutputs and the
  # model run are unchanged.
  # The Outputs_Sim check is unconditional because
  # CreateRunOptions.GRiwrmInputsModel always passes Outputs_Sim, so the
  # guard above would skip it.
  if (grepl("CemaNeige", FUN_MOD, fixed = TRUE)) {
    if (!"CemaNeigeLayers" %in% dots$Outputs_Sim) {
      dots$Outputs_Sim <- c(dots$Outputs_Sim, "CemaNeigeLayers")
    }
    if (!"Precip" %in% dots$Outputs_Sim) {
      dots$Outputs_Sim <- c(dots$Outputs_Sim, "Precip")
    }
    if (!"Outputs_Cal" %in% names(dots)) {
      # Use airGR's default CemaNeige Outputs_Cal augmented with
      # "CemaNeigeLayers" and "Gratio".
      # airGR default for CemaNeige is c("PliqAndMelt", "Qsim", "Param").
      dots$Outputs_Cal <- c(
        "CemaNeigeLayers",
        "Gratio",
        "PliqAndMelt",
        "Qsim",
        "Param"
      )
    } else {
      if (!"CemaNeigeLayers" %in% dots$Outputs_Cal) {
        dots$Outputs_Cal <- c(dots$Outputs_Cal, "CemaNeigeLayers")
      }
      if (!"Gratio" %in% dots$Outputs_Cal) {
        dots$Outputs_Cal <- c(dots$Outputs_Cal, "Gratio")
      }
    }
  }

  return(dots)
}

CreateRunOptions_runAirGR <- function(dots, warning_pattern) {
  RunOptions <- suppressWarningsRegex(
    do.call(airGR::CreateRunOptions, dots),
    pattern = warning_pattern
  )
  return(RunOptions)
}
