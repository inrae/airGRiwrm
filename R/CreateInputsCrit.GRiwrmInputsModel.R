#' @rdname CreateInputsCrit
#' @import airGR
#' @importFrom utils tail read.table
#' @export
CreateInputsCrit.GRiwrmInputsModel <- function(InputsModel,
                                               FUN_CRIT = ErrorCrit_NSE,
                                               RunOptions,
                                               Obs,
                                               AprioriIds = NULL,
                                               k = 0.15,
                                               AprCelerity = 1,
                                               ...) {
  # Parameter checks

  # We invoke the mandatory arguments here for avoiding
  # a messy error message on "get(x)" if an argument is missing
  # We also list all arguments in order to check arguments even in "..."
  arguments <- c(as.list(environment()), list(...))

  # Checking argument classes
  lVars2Check <- list(InputsModel = "GRiwrmInputsModel",
                      RunOptions = "GRiwrmRunOptions",
                      Obs = c("matrix", "data.frame"))
  lapply(names(lVars2Check), function(argName) {
    b <- sapply(lVars2Check[[argName]], function(argClass) {
      !inherits(get(argName), argClass)
    })
    if (all(b)) {
      stop(sprintf("'%s' must be of class %s", argName, paste(lVars2Check[[argName]], collapse = " or ")))
    }
  })

  if (!is.null(AprioriIds)) {
    AprioriIds <- unlist(AprioriIds)
    if (!is.character(AprioriIds) || is.null(names(AprioriIds))) {
      stop("Argument 'AprioriIds' must be a named list or a named vector of characters")
    }
    if (length(unique(names(AprioriIds))) != length(names(AprioriIds))) {
      stop("Each name of AprioriIds items must be unique: duplicate entry detected")
    }
    if ("Weights" %in% names(arguments)) {
      stop("Argument 'Weights' cannot be used when using Lavenne criterion")
    }
    if (!"transfo" %in% names(arguments)) {
      stop("Argument 'transfo' must be defined when using Lavenne criterion (Using \"sqrt\" is recommended)")
    }
    lapply(names(AprioriIds), function(id) {
      if (!id %in% names(InputsModel)) {
        stop("'Each item of names(AprioriIds) must be an id of a modeled node:",
             " the id \"", id ,"\" is not in the list of the modeled nodes")
      }
      if (!AprioriIds[id] %in% names(InputsModel)) {
        stop("'Each item of AprioriIds must be an id of a modeled node:",
             " the id \"", AprioriIds[id] ,"\" is not in the list of the modeled nodes")
      }
      if (!AprioriIds[id] %in% names(InputsModel)[1:which(id == names(InputsModel))]) {
        stop("'AprioriIds': the node \"", AprioriIds[id],
             "\" is not calibrated before the node \"", id,"\".",
             "\nIf possible, set this apriori id as the donor of the node \"",
             id,"\" to force the calibration sequence order")
      }
      if (InputsModel[[AprioriIds[id]]]$inUngaugedCluster &
          InputsModel[[AprioriIds[id]]]$gaugedId == id) {
        stop("'AprioriIds': the node \"", AprioriIds[id],
             "\" is ungauged, use a gauged node instead")
      }
      if (!identical(InputsModel[[id]]$FUN_MOD, InputsModel[[AprioriIds[id]]]$FUN_MOD)) {
        stop("'AprioriIds': the node \"", AprioriIds[id],
             "\" must use the same hydrological model as the node \"", id,"\"")
      }
    })
  }

  InputsCrit <- list()
  class(InputsCrit) <- append("GRiwrmInputsCrit", class(InputsCrit))

  np <- getAllNodesProperties(attr(InputsModel, "GRiwrm"))
  gaugedIds <- np$id[np$calibration == "Gauged"]
  for (id in gaugedIds) {
    if (id %in% colnames(Obs)) {
      IM <- InputsModel[[id]]
      InputsCrit[[IM$id]] <- CreateInputsCrit.InputsModel(
        InputsModel = IM,
        FUN_CRIT = FUN_CRIT,
        RunOptions = RunOptions[[IM$id]],
        Obs = Obs[, IM$id],
        ...
      )
      if (!is.null(AprioriIds) && IM$id %in% names(AprioriIds)) {
        # De Lavenne regularization for this sub-catchment
        attr(InputsCrit[[IM$id]], "Lavenne_FUN") <-
          CreateLavenneFunction(
            InputsModel = IM,
            FUN_CRIT = FUN_CRIT,
            RunOptions = RunOptions[[IM$id]],
            Obs = Obs[, IM$id],
            k = k,
            ...
          )
        attr(InputsCrit[[IM$id]], "AprioriId") <- AprioriIds[IM$id]
        attr(InputsCrit[[IM$id]], "AprCelerity") <- AprCelerity
        attr(InputsCrit[[IM$id]], "model") <- IM$model
        if (IM$model$hasX4) {
          attr(InputsCrit[[IM$id]], "model")$X4Ratio <- max(
            (tail(IM$BasinAreas, 1) / tail(InputsModel[[AprioriIds[IM$id]]]$BasinAreas, 1))^0.3,
            0.5
          )
        }
        class(InputsCrit[[IM$id]]) <- c("InputsCritLavenneFunction", class(InputsCrit[[IM$id]]))
      }
    } else {
      message("No observations found for node \"", id, "\"\n",
              "You must fix the parameters of this node in CreateCalibOptions")
    }
  }
  return(InputsCrit)
}

#' Generate a `CreateInputsCrit_Lavenne` function which embeds know parameters
#'
#' The created function will be used in calibration for injecting necessary `AprParamR` and `AprCrit`
#' parameters, which can be known only during calibration process, in the call of `CreateInputsCrit_Lavenne`.
#'
#' @param InputsModel See [CreateInputsCrit] parameters
#' @param FUN_CRIT See [CreateInputsCrit] parameters
#' @param RunOptions See [CreateInputsCrit] parameters
#' @param Obs See [CreateInputsCrit] parameters
#' @param k See [CreateInputsCrit] parameters
#' @param ... further arguments for [airGR::CreateInputsCrit_Lavenne]
#'
#' @return A function with `AprParamR` and `AprCrit`
#' @noRd
#'
CreateLavenneFunction <- function(InputsModel, FUN_CRIT, RunOptions, Obs, k, ...) {
  # The following line solve the issue #57 by forcing the evaluation of all the parameters.
  # See also: https://stackoverflow.com/questions/69016698/is-there-a-bug-on-closures-embedded-in-a-list-in-r/69028161#69028161
  arguments <- c(as.list(environment()), list(...))
  function(AprParamR, AprCrit) {
    do.call(
      CreateInputsCrit_Lavenne,
      c(arguments, list(AprParamR = AprParamR, AprCrit = AprCrit))
    )
  }
}
