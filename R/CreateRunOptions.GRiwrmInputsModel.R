#' @param IniStates (optional) object containing initial model states. Can be:
#'   * A single [`IniStates`][airGR::CreateIniStates] object when calling [CreateRunOptions.InputsModel]
#'   * A named [list] of [`IniStates`][airGR::CreateIniStates] objects when calling [CreateRunOptions.GRiwrmInputsModel], with names matching node IDs
#'
#'   See [airGR::CreateIniStates] for details
#'
#' @param Outputs_Sim (optional) outputs to be returned by the model run.
#'   Can be:
#'   * `"all"` to return all available simulated outputs for each node
#'     (previous default behaviour, but not recommended for large networks)
#'   * A [character] vector, applied to every node, to restrict the returned
#'     outputs (default: `c("DatesR", "WarmUpQsim", "Qsim", "Qsim_m3",
#'     "QsimDown", "Param", "StateEnd")`).
#'   * A named [list] (names matching node IDs) to define outputs per node,
#'     each item being a [character] vector.
#'
#'   See [airGR::CreateRunOptions] for the list of output names supported by
#'   each rainfall-runoff model.
#'
#' @details The default output selection has been reduced compared to
#' previous versions of the package (where it was `"all"`) in order to keep
#' the size of the resulting *GRiwrmOutputsModel* object reasonable for a
#' network with a large number of nodes (See issue inrae/airGRiwrm#141).
#' The default keeps only the outputs required to propagate simulated flows
#' to downstream nodes (`Qsim`, `Qsim_m3`), to warm-up semi-distributed
#' nodes correctly (`WarmUpQsim`), to carry the model parameters
#' (`Param`) and the model states at the end of the run (`StateEnd`) which
#' are needed for De Lavenne parameter regularization and hot restart.
#' For CemaNeige models, the `CemaNeigeLayers` output is added
#' automatically: it carries the CemaNeige layer output series required by
#' SCA-based error criteria (a reduced CemaNeige output set would change
#' the model run in airGR). Use `Outputs_Sim = "all"` to restore the
#' previous behaviour.
#'
#' @rdname CreateRunOptions
#' @export
CreateRunOptions.GRiwrmInputsModel <- function(
  x,
  IniStates = NULL,
  Outputs_Sim = MinimalOutputsSim,
  ...
) {
  if (!is.null(IniStates)) {
    if (!is.list(IniStates)) {
      stop("'IniStates' must be a list of IniStates objects")
    }
    if (is.null(names(IniStates))) {
      stop("'IniStates' must be a named list with names matching node IDs")
    }
    check_node_ids(IniStates, "IniStates", x)
  }

  if (is.character(Outputs_Sim)) {
    # Accept the legacy string "all" as well as a character vector of output names
    if (!identical(Outputs_Sim, "all") && any(Outputs_Sim == "all")) {
      stop(
        "The value 'all' in `Outputs_Sim` must be used alone, not with other output names"
      )
    }
  } else {
    if (!is.list(Outputs_Sim)) {
      stop(
        "'Outputs_Sim' must be either the string 'all', a character vector or a named list"
      )
    }
    if (is.null(names(Outputs_Sim))) {
      stop("'Outputs_Sim' must be a named list with names matching node IDs")
    }
    check_node_ids(Outputs_Sim, "Outputs_Sim", x)
    missing_ids <- setdiff(names(x), names(Outputs_Sim))
    if (length(missing_ids) > 0) {
      stop(
        "'Outputs_Sim' must define outputs for every node; missing: ",
        paste(missing_ids, collapse = ", ")
      )
    }
  }

  # Normalize to a per-node named list (a character vector applies to
  # every node)
  if (is.character(Outputs_Sim)) {
    Outputs_Sim <- setNames(
      rep(list(Outputs_Sim), length(names(x))),
      names(x)
    )
  }

  RunOptions <- list()
  class(RunOptions) <- append(class(RunOptions), "GRiwrmRunOptions")

  for (id in names(x)) {
    RunOptions[[id]] <- CreateRunOptions(
      x[[id]],
      IniStates = IniStates[[id]],
      Outputs_Sim = Outputs_Sim[[id]],
      ...
    )
    RunOptions[[id]]$id <- id
  }
  return(RunOptions)
}

# Check that the node IDs of a named list (IniStates, per-node Outputs_Sim)
# are valid node IDs of the network
check_node_ids <- function(obj, arg, x) {
  invalid_ids <- setdiff(names(obj), names(x))
  if (length(invalid_ids) > 0) {
    stop(
      "'", arg, "' contains invalid node IDs: ",
      paste(invalid_ids, collapse = ", "),
      ". Valid IDs are: ",
      paste(names(x), collapse = ", ")
    )
  }
}
