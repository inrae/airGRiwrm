% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CreateGRiwrm.R
\name{CreateGRiwrm}
\alias{CreateGRiwrm}
\alias{GRiwrm}
\title{Generation of a network description containing all hydraulic nodes and the
description of their connections}
\usage{
CreateGRiwrm(
  db,
  cols = list(id = "id", down = "down", length = "length", area = "area", model =
    "model", donor = "donor"),
  keep_all = FALSE
)
}
\arguments{
\item{db}{\link{data.frame} description of the network (See details)}

\item{cols}{\link{list} or \link{vector} columns of \code{db}. By default, mandatory column
names are: \code{id}, \code{down}, \code{length}, \code{area} and \code{model}. Other names can be
handled with a named list or vector containing items defined as \code{"required name" = "column name in db"} (See details)}

\item{keep_all}{\link{logical} indicating if all columns of \code{db} should be kept
or if only columns defined in \code{cols} should be kept}
}
\value{
\link{data.frame} of class \code{GRiwrm} describing the airGR semi-distributed
model network, with each line corresponding to a location on the river
network and with the following columns:
\itemize{
\item \code{id} (\link{character}): node identifier
\item \code{down} (\link{character}): identifier of the node downstream of the current
node (\link{NA} for the most downstream node)
\item \code{length} (\link{numeric}): hydraulic distance to the downstream node in km
(\link{NA} for the most downstream node)
\item \code{area} (\link{numeric}): total area of the basin starting from the current
node location in km2
\item \code{model} (\link{character}): hydrological model to use (\link{NA} for using observed
flow instead of a runoff model output)
\item \code{donor} (\link{character}): node used as model and calibration parameter "donor" for
ungauged nodes. For other types of nodes, if the donor is different than the
id, it indicates that the node is embedded in an ungauged node cluster.
}
}
\description{
Generation of a network description containing all hydraulic nodes and the
description of their connections
}
\details{
\code{db} is a \link{data.frame} which at least contains in its columns:
\itemize{
\item a node identifier (column \code{id}),
\item the identifier and the hydraulic distance to the downstream node
(\link{character} columns \code{down} and \link{numeric} columns \code{length} in km). The
last downstream node should have fields \code{down} and \code{length} set to \code{NA},
\item the total area of the basin at the node location (\link{numeric} column \code{area} in km2).
Direct injection node can have a null area defined by \code{NA}
\item the model to use (\link{character} column \code{model}), see section below for details
}

An optional column \code{donor} can be used to manually define which sub-basin
will give its parameters to an ungauged node (See \code{Ungauged} model below).
\subsection{Available models in airGRiwrm}{

The "model" column should be filled by one of the following:
\itemize{
\item One of the hydrological models available in the \strong{airGR} package defined by its
\code{RunModel} function (i.e.: \code{RunModel_GR4J}, \code{RunModel_GR5HCemaneige}...)
\item \code{RunModel_Reservoir} for simulating a reservoir (See: \link{RunModel_Reservoir})
\item \code{Ungauged} for an ungauged node. The sub-basin inherits hydrological model and
parameters from a "donor" sub-basin. If not defined by the user in the column \code{donor},
the donor is automatically set to the first gauged node at downstream.
This set of sub-basins with the same donor downstream then forms an ungauged
node cluster that will be calibrated at once.
\item \code{NA} for injecting (or abstracting) a flow time series at the location of the node
(direct flow injection)
\item \code{Diversion} for abstracting a flow time series from an existing node transfer it
to another node. As a \code{Diversion} is attached to an existing node, this node is
then described with 2 lines: one for the hydrological model and another one for the
diversion
}
}
}
\examples{
library(airGRiwrm)

#########################################
# Network of 2 nodes distant of 150 km: #
#########################################
# - an upstream reservoir modeled as a direct flow injection (no model)
# - a gauging station downstream a catchment of 360 km² modeled with GR4J
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
nodes_ungauged$model[nodes_ungauged$gauge_id \%in\% c("54029", "54001")] <- "Ungauged"
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
  header = "\%\%{init: {'flowchart': {'nodeSpacing': 30, 'rankSpacing': 30, 'curve': 'linear'}}}\%\%"
)
}
}
