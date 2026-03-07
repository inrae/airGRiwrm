#' @param IniStates (optional) object containing initial model states. Can be:
#'   * A single [`IniStates`][airGR::CreateIniStates] object when calling [CreateRunOptions.InputsModel]
#'   * A named [list] of [`IniStates`][airGR::CreateIniStates] objects when calling [CreateRunOptions.GRiwrmInputsModel], with names matching node IDs
#'
#'   See [airGR::CreateIniStates] for details
#'
#' @param Outputs_Sim (optional) outputs to be returned by the model run. Can be:
#'   * `"all"` to return all available simulated outputs for each node
#'   * A named [list] (names matching node IDs) to define outputs per node
#'
#'   In the case of a [list], each item must be a [character] vector.
#'   See [airGR::CreateRunOptions] for the list of supported output names.
#'
#' @rdname CreateRunOptions
#' @export
CreateRunOptions.GRiwrmInputsModel <- function(
  x,
  IniStates = NULL,
  Outputs_Sim = c("DatesR", "WarmUpQsim", "Qsim", "Qsim_m3"),
  ...
) {
  if (!is.null(IniStates)) {
    if (!is.list(IniStates)) {
      stop("'IniStates' must be a list of IniStates objects")
    }
    if (is.null(names(IniStates))) {
      stop("'IniStates' must be a named list with names matching node IDs")
    }
    invalid_ids <- setdiff(names(IniStates), names(x))
    if (length(invalid_ids) > 0) {
      stop(
        "'IniStates' contains invalid node IDs: ",
        paste(invalid_ids, collapse = ", "),
        ". Valid IDs are: ",
        paste(names(x), collapse = ", ")
      )
    }
  }

  if (!is.character(Outputs_Sim)) {
    if (!is.list(Outputs_Sim)) {
      stop("'Outputs_Sim' must be either a character vector or a named list")
    }
    if (is.null(names(Outputs_Sim))) {
      stop("'Outputs_Sim' must be a named list with names matching node IDs")
    }
    invalid_output_ids <- setdiff(names(Outputs_Sim), names(x))
    if (length(invalid_output_ids) > 0) {
      stop(
        "'Outputs_Sim' contains invalid node IDs: ",
        paste(invalid_output_ids, collapse = ", "),
        ". Valid IDs are: ",
        paste(names(x), collapse = ", ")
      )
    }
  }

  RunOptions <- list()
  class(RunOptions) <- append(class(RunOptions), "GRiwrmRunOptions")

  for (id in names(x)) {
    RunOptions[[id]] <- CreateRunOptions(
      x[[id]],
      IniStates = IniStates[[id]],
      Outputs_Sim = if (is.character(Outputs_Sim)) {
        Outputs_Sim
      } else {
        Outputs_Sim[[id]]
      },
      ...
    )
    RunOptions[[id]]$id <- id
  }
  return(RunOptions)
}
