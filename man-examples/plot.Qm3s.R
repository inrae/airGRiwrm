###################################################################
# Run the Severn example provided with this package               #
# A natural catchment composed with 6 gauging stations            #
###################################################################

data(Severn)
nodes <- Severn$BasinsInfo
nodes$model <- "RunModel_GR4J"
# Mismatch column names are renamed to stick with GRiwrm requirements
rename_columns <- list(id = "gauge_id",
                       down = "downstream_id",
                       length = "distance_downstream")
g_severn <- CreateGRiwrm(nodes, rename_columns)

# Network diagram with upstream basin nodes in blue, intermediate sub-basin in green
plot(g_severn)

# Format CAMEL-GB meteorological dataset for airGRiwrm inputs
BasinsObs <- Severn$BasinsObs
DatesR <- BasinsObs[[1]]$DatesR
PrecipTot <- cbind(sapply(BasinsObs, function(x) {x$precipitation}))
PotEvapTot <- cbind(sapply(BasinsObs, function(x) {x$peti}))

# Precipitation and Potential Evaporation are related to the whole catchment
# at each gauging station. We need to compute them for intermediate catchments
# for use in a semi-distributed model
Precip <- ConvertMeteoSD(g_severn, PrecipTot)
PotEvap <- ConvertMeteoSD(g_severn, PotEvapTot)

# CreateInputsModel object
IM_severn <- CreateInputsModel(g_severn, DatesR, Precip, PotEvap)

# GRiwrmRunOptions object
IndPeriod_Run <- seq(
  which(IM_severn[[1]]$DatesR == (IM_severn[[1]]$DatesR[1] + 365*24*60*60)), # Set aside warm-up period
  length(IM_severn[[1]]$DatesR) # Until the end of the time series
)
IndPeriod_WarmUp <- seq(1, IndPeriod_Run[1] - 1)

RO_severn <- CreateRunOptions(
  IM_severn,
  IndPeriod_WarmUp = IndPeriod_WarmUp,
  IndPeriod_Run = IndPeriod_Run
)

# Load parameters of the model from Calibration in vignette V02
P_severn <- readRDS(system.file("vignettes", "ParamV02.RDS", package = "airGRiwrm"))

# Run the simulation
OM_severn <- RunModel(IM_severn,
                          RunOptions = RO_severn,
                          Param = P_severn)

# Plot results of simulated flows in m3/s
Qm3s <- attr(OM_severn, "Qm3s")
plot(Qm3s[1:150, ])
