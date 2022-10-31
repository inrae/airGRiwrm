library(airGRiwrm)

#########################################
# Network of 2 nodes distant of 150 km: #
#########################################
# - an upstream reservoir modelled as a direct flow injection (no model)
# - a gauging station downstream a catchment of 360 km² modelled with GR4J
db <- data.frame(id = c("Reservoir", "GaugingDown"),
                 length = c(150, NA),
                 down = c("GaugingDown", NA),
                 area = c(NA, 360),
                 model = c(NA, "RunModel_GR4J"),
                 stringsAsFactors = FALSE)
griwrm_basic <- CreateGRiwrm(db)
griwrm_basic
# Network diagram with direct flow node in red, intermediate sub-basin in green
plot(griwrm_basic)

###################################################
# GR4J semi-distributed model of the Severn River #
###################################################
data(Severn)
nodes <- Severn$BasinsInfo
nodes$model <- "RunModel_GR4J"
str(nodes)
# Mismatch column names are renamed to stick with GRiwrm requirements
rename_columns <- list(id = "gauge_id",
                       down = "downstream_id",
                       length = "distance_downstream")
griwrm_severn <- CreateGRiwrm(nodes, rename_columns)
griwrm_severn
# Network diagram with upstream basin nodes in blue, intermediate sub-basin in green
plot(griwrm_severn)

####################################################################
# Severn network with an ungauged station at nodes 54029 and 54001 #
####################################################################
# By default the first gauged node at downstream is used for parameter calibration (54032)
nodes_ungauged <- nodes
nodes_ungauged$model[nodes_ungauged$gauge_id %in% c("54029", "54001")] <- "Ungauged"
griwrm_ungauged <- CreateGRiwrm(nodes_ungauged, rename_columns)
# The `donor` column define which node is used for parameter calibration
griwrm_ungauged
# Network diagram with gauged nodes of vivid color, and ungauged nodes of dull color
plot(griwrm_ungauged)

#######################################################
# Severn network with a Diversion on the node "54029" #
# which transfer flows to the node "54001"            #
#######################################################
nodes_div <- nodes[, c("gauge_id", "downstream_id", "distance_downstream", "model", "area")]
nodes_div <- rbind(nodes_div, data.frame(gauge_id = "54029",
                                         downstream_id = "54001",
                                         distance_downstream = 20,
                                         model = "Diversion",
                                         area = NA))
griwrm_div <- CreateGRiwrm(nodes_div, rename_columns)
# Network diagram figures Diversion node by a red frame and a red arrow
plot(griwrm_div)
