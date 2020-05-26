#' Wrapper for the airGR::CreateInputsModel function
#'
#' @param FUN_MOD
#' @param DatesR
#' @param Precip
#' @param PrecipScale
#' @param PotEvap
#' @param TempMean
#' @param TempMin
#' @param TempMax
#' @param ZInputs
#' @param HypsoData
#' @param NLayers
#' @param QobsUpstr
#' @param LengthHydro
#' @param BasinAreas
#' @param verbose
#'
#' @return
#' @import airGR
#' @export
#'
#' @examples
CreateInputsModel.default <- function(FUN_MOD,
                                      DatesR,
                                      Precip, PrecipScale = TRUE,
                                      PotEvap = NULL,
                                      TempMean = NULL, TempMin = NULL, TempMax = NULL,
                                      ZInputs = NULL, HypsoData = NULL, NLayers = 5,
                                      QobsUpstr = NULL, LengthHydro = NULL, BasinAreas = NULL,
                                      verbose = TRUE) {
  airGR::CreateInputsModel(FUN_MOD, DatesR, Precip, PrecipScale, PotEvap,
                           TempMean, TempMin, TempMax, ZInputs, HypsoData, NLayers,
                           QobsUpstr, LengthHydro, BasinAreas, verbose)
}
