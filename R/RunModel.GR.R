#' Run of a rainfall-runoff model on a sub-basin
#'
#' Run ONLY the rainfall-runoff model, upstream flow routing and Diversions are
#' not processed.
#'
#' @details
#' This function runs [airGR::RunModel] (without lag) and add an item `Qsim_m3`
#' to the returned *OutputsModel* object.
#'
#' @param x \[object of class `InputsModel`\] `InputsModel` for [airGR::RunModel]
#' @param RunOptions \[object of class *RunOptions*\] see [airGR::CreateRunOptions] for details
#' @param Param [numeric] vector of model parameters (See details for SD lag model)
#' @param ... further arguments passed to or from other methods
#'
#' @inherit airGR::RunModel description details return
#' @noRd
#'
RunModel.GR <- function(x, RunOptions, Param, ...) {

  if (inherits(x, "SD")) {
    # Lag model take one parameter at the beginning of the vector
    iFirstParamRunOffModel <- 2
    RunOptions$FeatFUN_MOD$NbParam <- RunOptions$FeatFUN_MOD$NbParam - 1
  } else {
    # All parameters
    iFirstParamRunOffModel <- 1
  }
  # Avoiding Error in `FUN_MOD(x, RunOptions, Param)`: NA/NaN/Inf in foreign function call (arg 7)
  RunOptions$IniStates[is.na(RunOptions$IniStates)] <- 0

  FUN_MOD <- match.fun(x$FUN_MOD)
  OutputsModel <- FUN_MOD(x, RunOptions = RunOptions,
          Param = Param[iFirstParamRunOffModel:length(Param)])
  OutputsModel <- complete_OutputsModel(OutputsModel, RunOptions, x$BasinAreas)

  return(OutputsModel)
}
