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

