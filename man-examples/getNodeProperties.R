###############################################################################
# Severn network with :                                                       #
# - a Diversion on the node "54001" which transfer flows to the node "540029" #
# - node 54002 as a Direct Injection node                                     #
###############################################################################
data(Severn)
nodes <- Severn$BasinsInfo
nodes$model <- "RunModel_GR4J"
str(nodes)
nodes <- nodes[, c("gauge_id", "downstream_id", "distance_downstream", "model", "area")]
# Add a Diversion node from node "54001" to "54029"
nodes <- rbind(nodes,
               data.frame(
                 gauge_id = "54001",
                 downstream_id = "54029",
                 distance_downstream = 20,
                 model = "Diversion",
                 area = NA
               ))
# Set node '54002' as a Direct Injection node
nodes$model[nodes$id == "54002"] <- NA
# Mismatch column names are renamed to stick with GRiwrm requirements
rename_columns <- list(id = "gauge_id",
                       down = "downstream_id",
                       length = "distance_downstream")
# Create GRiwrm object and display properties
griwrm <- CreateGRiwrm(nodes, rename_columns)

str(getNodeProperties("54001", griwrm))

getAllNodesProperties(griwrm)
