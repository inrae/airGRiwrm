#' @rdname CreateInputsCrit
#' @export
CreateInputsCrit.GRiwrmInputsModel <- function(InputsModel,
                                               FUN_CRIT = airGR::ErrorCrit_NSE,
                                               RunOptions,
                                               Obs,
                                               AprioriIds = NULL,
                                               k = 0.15,
                                               ...) {
  # Parameter checks

  # We invoke the mandatory arguments here for avoiding
  # a messy error message on "get(x)" if an argument is missing
  InputsModel
  RunOptions
  Obs

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
    lapply(names(AprioriIds), function(id) {
      if (!id %in% names(InputsModel)) {
        stop("'Each item of names(AprioriIds) must be an id of a simulated node:",
             " the id \"", id ,"\" is unknown")
      }
      if (!AprioriIds[id] %in% names(InputsModel)) {
        stop("'Each item of AprioriIds must be an id of a simulated node:",
             " the id \"", id ,"\" is unknown")
      }
      if (! isNodeDownstream(InputsModel, AprioriIds[id], id)) {
        stop("'AprioriIds': the node \"", AprioriIds[id],
             "\" is not upstream the node \"", id,"\"")
      }
    })
  }

  InputsCrit <- list()
  class(InputsCrit) <- append("GRiwrmInputsCrit", class(InputsCrit))

  for(IM in InputsModel) {
    InputsCrit[[IM$id]] <- CreateInputsCrit.InputsModel(
      InputsModel = IM,
      FUN_CRIT = FUN_CRIT,
      RunOptions = RunOptions[[IM$id]],
      Obs = Obs[, IM$id],
      ...
    )
    if (!is.null(AprioriIds) && IM$id %in% names(AprioriIds)) {
      # De Lavenne regularisation for this sub-catchment
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
      class(InputsCrit[[IM$id]]) <- c("InputsCritLavenneFunction", class(InputsCrit[[IM$id]]))
    }
  }

  return(InputsCrit)
}

CreateLavenneFunction <- function(InputsModel, FUN_CRIT, RunOptions, Obs, k, ...) {
  function(AprParamR, AprCrit) {
    CreateInputsCrit_Lavenne(FUN_CRIT = FUN_CRIT,
                               InputsModel = InputsModel,
                               RunOptions = RunOptions,
                               Obs = Obs,
                               AprParamR = AprParamR,
                               AprCrit = AprCrit,
                               k = k,
                               ...)
  }
}
