# Loading gauging station attributes
BasinsInfo <- read.csv("data-raw/CAMELS_GB_Severn_attributes.csv")
BasinsInfo$gauge_id <- as.character(BasinsInfo$gauge_id)
BasinsInfo$downstream_id <- as.character(BasinsInfo$downstream_id)
BasinsInfo$bankfull_flow <- as.double(BasinsInfo$bankfull_flow)
BasinsInfo$distance_downstream <- as.double(BasinsInfo$distance_downstream)

# Loading gauging station time series
stations <- paste0("540", sprintf("%02d", c(1, 2, 29, 32, 57, 95)))
files <- file.path(
  "data-raw",
  paste0("CAMELS_GB_hydromet_timeseries_", stations, "_19701001-20150930.csv")
)

read_CAMELS_ts <- function(file, cut) {
  if(!file.exists(file)) {
    stop("File not found: ", file, "\n",
         "Please download it from:\n",
         "https://catalogue.ceh.ac.uk/documents/8344e4f3-d2ea-44f5-8afa-86d2987543a9")
  }
  df <- read.csv(file)
  iCut <- which(df$date == cut)
  df <- df[iCut:nrow(df),]
  df$DatesR <- as.POSIXct(df$date)

  return(df[,c("DatesR", "precipitation", "peti", "discharge_spec")])
}

BasinsObs <- lapply(files, read_CAMELS_ts, cut = max(BasinsInfo$flow_period_start))
names(BasinsObs) <- stations

Severn <- list(BasinsInfo = BasinsInfo, BasinsObs = BasinsObs)

usethis::use_data(Severn, overwrite = TRUE)
