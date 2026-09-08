#' @rdname CreateInputsCrit
#' @import airGR
#' @importFrom utils tail read.table
#' @export
CreateInputsCrit.GRiwrmInputsModel <- function(
  InputsModel,
  FUN_CRIT = ErrorCrit_KGE2,
  RunOptions,
  Obs,
  AprioriIds = getDefaultAprioriIds(InputsModel),
  k = 0.15,
  AprCelerity = 1,
  ...
) {
  # Parameter checks

  # We invoke the mandatory arguments here for avoiding
  # a messy error message on "get(x)" if an argument is missing
  # We also list all arguments in order to check arguments even in "..."
  force(InputsModel)
  force(RunOptions)
  force(Obs)

  # Checking argument classes
  lVars2Check <- list(
    InputsModel = "GRiwrmInputsModel",
    RunOptions = "GRiwrmRunOptions",
    Obs = c("matrix", "data.frame")
  )
  lapply(names(lVars2Check), function(argName) {
    b <- sapply(lVars2Check[[argName]], function(argClass) {
      !inherits(get(argName), argClass)
    })
    if (all(b)) {
      stop(sprintf(
        "'%s' must be of class %s",
        argName,
        paste(lVars2Check[[argName]], collapse = " or ")
      ))
    }
  })

  if (!is.null(AprioriIds)) {
    AprioriIds <- as.list(AprioriIds)
    if (!all(sapply(AprioriIds, is.character)) || is.null(names(AprioriIds))) {
      stop(
        "Argument 'AprioriIds' must be a named list of character vectors or a named character vector"
      )
    }
    if (length(unique(names(AprioriIds))) != length(names(AprioriIds))) {
      stop(
        "Each name of AprioriIds items must be unique: duplicate entry detected"
      )
    }
    dots <- list(...)
    if ("Weights" %in% names(dots)) {
      stop("Argument 'Weights' cannot be used when using Lavenne criterion")
    }
    lapply(names(AprioriIds), function(id) {
      if (!id %in% names(InputsModel)) {
        stop(
          "'Each item of names(AprioriIds) must be an id of a modeled node:",
          " the id \"",
          id,
          "\" is not in the list of the modeled nodes"
        )
      }
      if (!all(AprioriIds[[id]] %in% names(InputsModel))) {
        stop(
          "'Each item of AprioriIds must be an id of a modeled node:",
          " one of the ids \"",
          AprioriIds[[id]],
          "\" is not in the list of the modeled nodes"
        )
      }
      sapply(AprioriIds[[id]], function(AprioriId) {
        if (
          !AprioriId %in% names(InputsModel)[1:which(id == names(InputsModel))]
        ) {
          stop(
            "'AprioriIds': the node \"",
            AprioriId,
            "\" is not calibrated before the node \"",
            id,
            "\".",
            "\nIf possible, set this apriori id as the donor of the node \"",
            id,
            "\" to force the calibration sequence order"
          )
        }
        if (
          InputsModel[[AprioriId]]$inUngaugedCluster &
            InputsModel[[AprioriId]]$gaugedId == id
        ) {
          stop(
            "'AprioriIds': the node \"",
            AprioriId,
            "\" is ungauged, use a gauged node instead"
          )
        }
        if (
          !identical(
            InputsModel[[id]]$FUN_MOD,
            InputsModel[[AprioriId]]$FUN_MOD
          )
        ) {
          stop(
            "'AprioriIds': the node \"",
            AprioriId,
            "\" must use the same hydrological model as the node \"",
            id,
            "\""
          )
        }
      })
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
        attr(InputsCrit[[IM$id]], "AprioriIds") <- AprioriIds[[IM$id]]
        attr(InputsCrit[[IM$id]], "AprCelerity") <- AprCelerity
        attr(InputsCrit[[IM$id]], "model") <- IM$model
        if (IM$model$hasX4) {
          attr(InputsCrit[[IM$id]], "model")$X4Ratio <- (dplyr::last(
            IM$BasinAreas
          ) /
            sapply(InputsModel[AprioriIds[[IM$id]]], function(x) {
              dplyr::last(x$BasinAreas)
            }))^0.3
        }
        class(InputsCrit[[IM$id]]) <- c(
          "InputsCritLavenneFunction",
          class(InputsCrit[[IM$id]])
        )
      }
    } else {
      message(
        "No observations found for node \"",
        id,
        "\"\n",
        "You must fix the parameters of this node in CreateCalibOptions"
      )
    }
  }
  return(InputsCrit)
}

#' Get default AprioriIds from direct upstream nodes of each node
#' @inheritParams CreateInputsCrit.GRiwrmInputsModel
#' @returns A [list] named with node Ids and containing the Ids of the upstream
#' nodes that can be used for apriori parameters.
#' @export
getDefaultAprioriIds <- function(InputsModel) {
  l <- lapply(
    setNames(nm = names(InputsModel)),
    getDefaultAprioriIds_node,
    InputsModel = InputsModel
  )
  l <- l[!sapply(l, is.null)]
  return(l)
}

#' @param original_Id Original Id if the call is done to by-pass a reservoir node
#' @noRd
getDefaultAprioriIds_node <- function(Id, InputsModel, original_Id = Id) {
  IM <- InputsModel[[Id]]
  if (Id == original_Id && IM$isReservoir) {
    return(NULL)
  }
  if (is.null(IM$UpstreamNodes)) {
    return(NULL)
  }
  AprioriIds <- IM$UpstreamNodes[IM$UpstreamIsModeled]
  if (length(AprioriIds) == 0) {
    return(NULL)
  }
  AprioriIds <- lapply(AprioriIds, function(AprioriId) {
    if (InputsModel[[AprioriId]]$isReservoir) {
      return(getDefaultAprioriIds_node(AprioriId, InputsModel, Id))
    }
    if (
      InputsModel[[AprioriId]]$inUngaugedCluster &
        InputsModel[[AprioriId]]$gaugedId == original_Id
    ) {
      return(NULL)
    }
    if (
      !IM$isReservoir &&
        !identical(
          IM$FUN_MOD,
          InputsModel[[AprioriId]]$FUN_MOD
        )
    ) {
      return(NULL)
    }
    return(AprioriId)
  }) %>%
    unlist() %>%
    unique()
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
CreateLavenneFunction <- function(
  InputsModel,
  FUN_CRIT,
  RunOptions,
  Obs,
  k,
  ...
) {
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
