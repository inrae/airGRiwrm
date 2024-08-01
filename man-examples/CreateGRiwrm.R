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
\dontrun{
plot(griwrm_basic)
}

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
\dontrun{
plot(griwrm_severn)
}

####################################################################
# Severn network with an ungauged station at nodes 54029 and 54001 #
####################################################################
nodes_ungauged <- nodes
nodes_ungauged$model[nodes_ungauged$gauge_id %in% c("54029", "54001")] <- "Ungauged"
# By default the first gauged node at downstream is used for parameter calibration (54032)
# Add a `donor`column for defining manually an upstream or sibling donor
nodes_ungauged$donor <- as.character(NA)
nodes_ungauged$donor[nodes_ungauged$id == "54001"] <- "54095"
griwrm_ungauged <- CreateGRiwrm(nodes_ungauged, rename_columns)
griwrm_ungauged
# Network diagram with gauged nodes of vivid color, and ungauged nodes of dull color
\dontrun{
plot(griwrm_ungauged)
}

###########################################################
# Severn network with a Diversion on the node "54029"     #
# to a reservoir which transfer flows to the node "54001" #
# and a withdrawal on the reservoir                       #
###########################################################
nodes_div <- nodes[, c("gauge_id", "downstream_id", "distance_downstream", "model", "area")]
nodes_div <- rbind(
  nodes_div,
  data.frame(gauge_id            = c("54029"    , "Reservoir"         , "Irrigation_Pump"),
             downstream_id       = c("Reservoir", "54001"             , "Reservoir"      ),
             distance_downstream = c(10         , 5                   , 0                ),
             model               = c("Diversion", "RunModel_Reservoir", NA      ),
             area                = c(NA         , NA                  , NA))
)
griwrm_div <- CreateGRiwrm(nodes_div, rename_columns)
# Network diagram figures Diversion node by a red frame and a red arrow
\dontrun{
plot(griwrm_div, orientation = "TB")
}

# It's also possible to custom the diagram's look with mermaid directives
# (See details in plot.GRiwrm help topic)
\dontrun{
plot(
  griwrm_div,
  header = "%%{init: {'flowchart': {'nodeSpacing': 30, 'rankSpacing': 30, 'curve': 'linear'}}}%%"
)
}
