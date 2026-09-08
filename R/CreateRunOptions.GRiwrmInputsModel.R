#' @param IniStates (optional) object containing initial model states. Can be:
#'   * A single [`IniStates`][airGR::CreateIniStates] object when calling [CreateRunOptions.InputsModel]
#'   * A named [list] of [`IniStates`][airGR::CreateIniStates] objects when calling [CreateRunOptions.GRiwrmInputsModel], with names matching node IDs
#'
#'   See [airGR::CreateIniStates] for details
#' @rdname CreateRunOptions
#' @export
CreateRunOptions.GRiwrmInputsModel <- function(x, IniStates = NULL, ...) {
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
  RunOptions <- list()
  class(RunOptions) <- append(class(RunOptions), "GRiwrmRunOptions")

  for (id in names(x)) {
    RunOptions[[id]] <- CreateRunOptions(
      x[[id]],
      IniStates = IniStates[[id]],
      ...
    )
    RunOptions[[id]]$id <- id
  }
  return(RunOptions)
}
