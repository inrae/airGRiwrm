#' Title
#'
#' @param ginet
#' @param girop
#' @param gits
#' @param IndPeriod_Run
#' @param IndPeriod_WarmUp
#'
#' @return
#' @import airGR
#' @export
#'
#' @examples
RunModelGriwrm <- function(ginet, girop, gits, IndPeriod_Run, IndPeriod_WarmUp = NULL, verbose = TRUE) {

  OutputsModels <- list()

  for(id in getNodeRanking(ginet)) {
    if(verbose) cat("*** Treating sub-basin", id, "... ***\n")

    # Set InputsModel and RunOptions
    lIO <- SetAirGrInputsAndOptions(
      id, ginet, girop, gits, OutputsModels,
      IndPeriod_Run, IndPeriod_WarmUp
    )

    # Prepare param for upstream sub-basin or basin with hydraulic routing
    Param <- unlist(girop$params[girop$id == id])

    # Run the model for the sub-basin
    OutputsModels[[id]] <- RunModel(
      FUN_MOD = girop$model[girop$id == id],
      InputsModel = lIO$InputsModel,
      RunOptions = lIO$RunOptions,
      Param = Param
    )

  }
  OutputsModels
}

#' Title
#'
#' @param id
#' @param ginet
#' @param girop
#' @param gits
#' @param IndPeriod_Run
#' @param IndPeriod_WarmUp
#'
#' @return
#' @import airGR
#' @export
#'
#' @examples
SetAirGrInputsAndOptions <- function(id, ginet, girop, gits, OutputsModels, IndPeriod_Run, IndPeriod_WarmUp = NULL) {
  node <- ginet[ginet$id == id,]
  FUN_MOD <- girop$model[girop$id == id]
  # Set hydraulic parameters
  UpstrNodes <- ginet$id[ginet$down == id & !is.na(ginet$down)]
  if(length(UpstrNodes) == 0) {
    # Upstream sub-basin
    QobsUpstr <- NULL
    LengthHydro <- NULL
    BasinAreas <- NULL
  } else {
    # Sub-basin with hydraulic routing
    for(i in 1:length(UpstrNodes)) {
      QobsUpstr1 <- matrix(
        c(
          rep(0, length(IndPeriod_WarmUp)),
          OutputsModels[[UpstrNodes[i]]]$Qsim
        ), ncol = 1
      )
      if(i == 1) {
        QobsUpstr <- QobsUpstr1
      } else {
        QobsUpstr <- cbind(QobsUpstr, QobsUpstr1)
      }
    }
    LengthHydro <- matrix(ginet$length[girop$id %in% UpstrNodes] , nrow = 1)
    BasinAreas <- matrix(
      c(
        girop$area[girop$id %in% UpstrNodes],
        girop$area[girop$id == id] - sum(girop$area[girop$id %in% UpstrNodes])
      ),
      nrow = 1
    )
  }
  # Set model inputs
  InputsModel <- CreateInputsModel(
    FUN_MOD = FUN_MOD,
    DatesR = gits$date,
    Precip = gits[[id]]$Precip,
    PotEvap = gits[[id]]$PotEvap,
    QobsUpstr = QobsUpstr,
    LengthHydro = LengthHydro,
    BasinAreas = BasinAreas
  )
  # Set model options
  RunOptions <- CreateRunOptions(
    FUN_MOD = FUN_MOD,
    InputsModel = InputsModel, IndPeriod_Run = IndPeriod_Run,
    IniStates = NULL, IniResLevels = NULL, IndPeriod_WarmUp = IndPeriod_WarmUp
  )
  list(InputsModel = InputsModel, RunOptions = RunOptions)
}
