# Small network with dam example

# Network
n_rsrvr <- loadSevernNodes()

# Reduce the network
n_rsrvr <- n_rsrvr[n_rsrvr$id %in% c("54095", "54001"),]
n_rsrvr$down[n_rsrvr$id == "54001"] <- NA
n_rsrvr$length[n_rsrvr$id == "54001"] <- NA
# Insert a dam downstream the location the gauging station 54095
# The dam is a direct injection node
n_rsrvr$down[n_rsrvr$id == "54095"] <- "Dam"
n_rsrvr$length[n_rsrvr$id == "54095"] <- 0
n_rsrvr <- rbind(
  n_rsrvr,
  data.frame(
    id = "Dam",
    down = "54001",
    length = 42,
    area = NA,
    model = "RunModel_Reservoir"
  )
)

# Model input
Qobs_rsrvr <- data.frame(Dam = rep(0, 11536))

get_nodes_derived_reservoir <- function(n_rsrvr) {
  nodes_Severn <- loadSevernNodes()
  n_rsrvr$model[n_rsrvr$id == "54095"] <- "Diversion"
  n_rsrvr$area[n_rsrvr$id == "54095"] <- NA
  nodes_Severn$model[nodes_Severn$id == "54095"] <- "Ungauged"
  n_rsrvr <- rbind(n_rsrvr, nodes_Severn[nodes_Severn$id == "54095", ])
  return(n_rsrvr)
}
n_derived_rsrvr <- get_nodes_derived_reservoir(n_rsrvr)

getGriwrmDerivedReservoirUngauged <- function(donorByDerivation, inconsistent = FALSE) {
  nodes <- loadSevernNodes()
  nodes <-
    nodes[nodes$id %in% c("54095", "54001", "54029", "54032"),]
  nodes[nodes$id == "54032", c("down", "length")] <- c(NA, NA)
  nodes$model[nodes$id == "54095"] <- "Ungauged"
  nodes <- rbind(nodes,
                 data.frame(
                   id = c("54095", "Dam"),
                   down = c("54029", "54029"),
                   length = c(10, 0),
                   area = rep(NA, 2),
                   model = c("Diversion", "RunModel_Reservoir")
                 ))
  nodes$down[nodes$id == "54095" &
               nodes$model == "Diversion"] <- "Dam"
  g <- CreateGRiwrm(nodes)
  if (donorByDerivation) g$donor[g$id == "54095"] <- "54029"
  if (!inconsistent) g$donor[g$id == "Dam"] <- "54029"
  return(g)
}

testDerivedUngauged <- function(donorByDerivation) {
  g <- getGriwrmDerivedReservoirUngauged(donorByDerivation)
  Qobs2 <- matrix(-1E9, ncol = 2, nrow = 11536)
  colnames(Qobs2) <- c("54095", "Dam")
  Qobs2[, "54095"] <- -1E9
  Qobs2[, "Dam"] <- 1E9
  e <- setupRunModel(griwrm = g, runRunModel = FALSE, Qobs2 = Qobs2)
  for (x in ls(e)) assign(x, get(x, e))

  CalibOptions <- CreateCalibOptions(InputsModel,
                                     FixedParam = list(Dam = c(650E6, 1)))
  e <- runCalibration(g, Qobs2 = Qobs2, CalibOptions = CalibOptions)
  for (x in ls(e)) assign(x, get(x, e))
  expect_equal(Param[["54095"]][1:3],
               Param[[ifelse(donorByDerivation, "54029", "54001")]][2:4])
}
