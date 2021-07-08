#' Conversion of meteorological data from basin scale to sub-basin scale
#'
#' @param x either a `GRiwrm` network description (See [CreateGRiwrm]), a [character] id of a node, or a [matrix] containing meteorological data
#' @param ... Parameters passed to the methods
#'
#' @return [matrix] a matrix containing the converted meteorological data
#' @export
#' @rdname ConvertMeteoSD
#'
ConvertMeteoSD <- function(x, ...) {
  UseMethod("ConvertMeteoSD")
}

#' @param meteo [matrix] or [data.frame] containing meteorological data. Its [colnames] should be equal to the ID of the basins
#' @export
#' @rdname ConvertMeteoSD
ConvertMeteoSD.GRiwrm <- function(x, meteo, ...) {
  meteo <- as.matrix(meteo)
  output <- lapply(colnames(meteo), ConvertMeteoSD , griwrm = x, meteo = meteo)
  meteoOut <- do.call(cbind,output)
  dimnames(meteoOut)[[2]] <- colnames(meteo)
  return(meteoOut)
}

#' @param griwrm `GRiwrm` object describing the semi-distributed network (See [CreateGRiwrm])
#' @export
#' @rdname ConvertMeteoSD
ConvertMeteoSD.character <- function(x, griwrm, meteo, ...) {
  upperBasins <- !is.na(griwrm$down) & griwrm$down == x
  if(all(!upperBasins)) {
    return(meteo[,x])
  }
  upperIDs <- griwrm$id[upperBasins]
  areas <- griwrm$area[match(c(x, upperIDs), griwrm$id)]
  output <- ConvertMeteoSD(
    meteo[,c(x, upperIDs), drop = FALSE],
    areas = areas
  )
  return(output)
}

#' @param areas [numeric] vector with the total area of the basin followed by the areas of the upstream basins in km2
#' @param temperature [logical] `TRUE` if the meteorological data contain air temperature. If `FALSE` minimum output values are bounded to zero
#' @export
#' @rdname ConvertMeteoSD
ConvertMeteoSD.matrix <- function(x, areas, temperature = FALSE, ...) {
  # Check arguments
  if(nrow(x) < 2) {
    stop("Meteorological data matrix should contain more than one row")
  }
  if(length(areas) != ncol(x)) {
    stop("'areas' length and meteo data matrix number of columns should be equal")
  }
  if(areas[1] <= sum(areas[-1])) {
    stop("Basin area 'areas[1]' should be greater than the sum of the upstream sub-basin areas")
  }
  if(ncol(x) == 1) {
    return(x)
  }
  # Convert mm to 1E3 m3
  V <- x * rep(areas, rep(nrow(x), length(areas)))
  # Sum upstream data
  if(ncol(x) > 2) {
    Vup <- rowSums(V[,-1])
  } else {
    Vup <- V[,2]
  }
  # Remove to basin to get downstream data
  Vdown <- V[,1] - Vup
  if(!temperature) Vdown[Vdown < 0] <- 0
  # Convert to mm
  meteoDown <- Vdown / (areas[1] - sum(areas[-1]))
  return(as.matrix(meteoDown, ncol = 1))
}

