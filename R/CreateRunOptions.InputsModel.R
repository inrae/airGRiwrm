#' Title
#'
#' @param FUN_MOD
#' @param InputsModel
#' @param IndPeriod_WarmUp
#' @param IndPeriod_Run
#' @param IniStates
#' @param IniResLevels
#' @param Imax
#' @param Outputs_Cal
#' @param Outputs_Sim
#' @param MeanAnSolidPrecip
#' @param IsHyst
#' @param warnings
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
CreateRunOptions.InputsModel <- function(InputsModel,
                                         IndPeriod_WarmUp = NULL, IndPeriod_Run,
                                         IniStates = NULL, IniResLevels = NULL, Imax = NULL,
                                         Outputs_Cal = NULL, Outputs_Sim = "all",
                                         MeanAnSolidPrecip = NULL, IsHyst = FALSE,
                                         warnings = TRUE, verbose = TRUE) {
  airGR::CreateRunOptions(FUN_MOD = InputsModel$FUN_MOD,
                          InputsModel = InputsModel,
                          IndPeriod_WarmUp = IndPeriod_WarmUp,
                          IndPeriod_Run = IndPeriod_Run,
                          IniStates = IniStates,
                          IniResLevels = IniResLevels,
                          Imax = Imax,
                          Outputs_Cal = Outputs_Cal,
                          Outputs_Sim = Outputs_Sim,
                          MeanAnSolidPrecip = MeanAnSolidPrecip,
                          IsHyst = IsHyst,
                          warnings = warnings,
                          verbose = verbose)
}
