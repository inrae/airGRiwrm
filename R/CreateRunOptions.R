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

#' @rdname CreateRunOptions
#' @export
CreateRunOptions.InputsModel <- function(x, ...) {
  dots <- CreateRunOptions_prepare_inputs(x, ...)
  # Add IsHyst in parameters if carried by InputsModel
  if (!is.null(x$model$IsHyst)) {
    dots$IsHyst <- x$model$IsHyst
  }
  # airGR 1.7.9 validates both Outputs_Sim and Outputs_Cal against the
  # model's supported outputs. Some outputs are computed by airGRiwrm
  # (not airGR) and must be stripped before calling airGR::CreateRunOptions,
  # then restored afterwards:
  #   - Qsim_m3: always computed by airGRiwrm's complete_OutputsModel
  #   - Qsim, QsimDown: for Lag (routing) models, computed by
  #     airGRiwrm's RunModel_Lag_enhanced
  #   - QsimDown: only valid for SD inputs in airGR; airGRiwrm adds it for
  #     routing nodes regardless
  #   - CemaNeigeLayers: airGRiwrm-specific, not validated by airGR
  #     CreateRunOptions for CemaNeige models
  #   - Gratio: airGRiwrm-specific, not in airGR's output list
  Outputs_Sim_full <- dots$Outputs_Sim
  Outputs_Cal_full <- dots$Outputs_Cal
  # Strip airGRiwrm-specific and airGR-unsupported outputs from Outputs_Sim:
  dots$Outputs_Sim <- setdiff(
    Outputs_Sim_full,
    c("Qsim_m3", "QsimDown", "CemaNeigeLayers")
  )
  if (identical(dots$FUN_MOD, "RunModel_Lag")) {
    # Lag model doesn't support Qsim in airGR either
    dots$Outputs_Sim <- setdiff(dots$Outputs_Sim, "Qsim")
  }
  # Strip airGRiwrm-specific outputs from Outputs_Cal too:
  dots$Outputs_Cal <- setdiff(
    Outputs_Cal_full,
    c("CemaNeigeLayers", "Gratio")
  )
  RunOptions <- CreateRunOptions_runAirGR(
    dots,
    "does not require .* Values? set to NA"
  )
  # Always restore the full Outputs_Sim so downstream
  # code can check which outputs should be populated.
  RunOptions$Outputs_Sim <- Outputs_Sim_full
  # Only restore Outputs_Cal if it was explicitly set (non-NULL).
  # When not provided, airGR computes the appropriate default and
  # we should not overwrite it with NULL.
  if (!is.null(Outputs_Cal_full)) {
    RunOptions$Outputs_Cal <- Outputs_Cal_full
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
  RunOptions$Outputs_Sim <- Outputs_Sim
  RunOptions$Outputs_Cal <- Outputs_Cal
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

  # Set minimal Outputs_Sim default when not explicitly provided by user.
  # This reduces memory by avoiding "all" which returns every internal
  # simulation variable.  Qsim_m3 and QsimDown are stripped before calling
  # airGR (they are airGRiwrm-specific), and restored afterwards.
  if (!"Outputs_Sim" %in% names(dots)) {
    dots$Outputs_Sim <- c(
      "DatesR",
      "WarmUpQsim",
      "Qsim",
      "Qsim_m3",
      "QsimDown",
      "Param",
      "StateEnd"
    )
  }

  # For CemaNeige models, ensure both Outputs_Sim and Outputs_Cal include
  # "CemaNeigeLayers".
  # (airGR defaults don't include these, but SCA-based error criteria
  # need OutputsModel$CemaNeigeLayers[[1]]$Gratio).
  # CemaNeigeLayers must be in Outputs_Cal because calibration uses that
  # list (not Outputs_Sim) to decide what RunModel produces.
  # This check is unconditional because CreateRunOptions.GRiwrmInputsModel
  # always passes Outputs_Sim, so the guard above would skip it.
  if (grepl("CemaNeige", FUN_MOD, fixed = TRUE)) {
    if (!"CemaNeigeLayers" %in% dots$Outputs_Sim) {
      dots$Outputs_Sim <- c(dots$Outputs_Sim, "CemaNeigeLayers")
    }
    if (!"Outputs_Cal" %in% names(dots)) {
      # Use airGR's default CemaNeige Outputs_Cal augmented with
      # "CemaNeigeLayers" (contains Gratio) and "Gratio".
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
