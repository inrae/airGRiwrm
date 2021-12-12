#' Set up a data sample for Cemaneige tests
#'
#' A basin of 3 nodes with 2 identical upstream basins and a nul area downstream basin with a lag of 0 time step
#'
#' @return [list] with:
#' - `BasinInfo`
#' - `griwrm`
#' - `Precip`
#' - `TempMean`
#' - `PotEvap`
#' - `Qobs`
#' - `HypsoData`
#' @noRd
#'
#' @examples
setUpCemaNeigeData <- function() {

  data(L0123001)

  # Formatting observations for the hydrological models
  # Each input data should be a matrix or a data.frame with the good id in the name of the column
  ids <- c("Up1", "Up2", "Down")
  l <- lapply(c("P", "T", "E", "Qmm"), function(x) {
    m <- matrix(data = rep(BasinObs[[x]], 3), ncol = 3)
    colnames(m) <- ids
    return(m)
  })
  l$DatesR <- BasinObs$DatesR
  names(l) <- c("Precip", "TempMean", "PotEvap", "Qobs")
  l$HypsoData <- matrix(data = rep(BasinInfo$HypsoData, 3), ncol = 3)
  colnames(l$HypsoData) <- ids
  l$ZInputs <- rep(BasinInfo$HypsoData[51], 3)
  names(l$ZInputs) <- ids

  db <- data.frame(id = ids,
                   length = c(0, 0, NA),
                   down = c("Down", "Down", NA),
                   area = c(rep(BasinInfo$BasinArea, 2), BasinInfo$BasinArea * 2),
                   model = rep("RunModel_CemaNeigeGR4J", 3),
                   stringsAsFactors = FALSE)

  # Create GRiwrm object from the data.frame
  l$griwrm <- CreateGRiwrm(db)

  l$DatesR <- BasinObs$DatesR

  return(l)
}
