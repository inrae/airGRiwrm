#' Convert meteorological data from basin scale to sub-basin scale
#'
#' @param x either a [GRiwrm] network description, a [character] id of a node, a [matrix] containing meteorological data
#' @param ... Parameters passed to the methods
#'
#' @return Either a [matrix] containing the converted meteorological data
#' @export
#' @rdname ConvertMeteoSD
#'
#' @examples
ConvertMeteoSD <- function(x, ...) {
  UseMethod("ConvertMeteoSD")
}

#' @export
#' @rdname ConvertMeteoSD
ConvertMeteoSD.GRiwrm <- function(x, meteo, ...) {
  meteo <- as.matrix(meteo)
  output <- lapply(colnames(meteo), ConvertMeteoSD , griwrm = x, meteo = meteo)
  meteoOut <- do.call(cbind,output)
  dimnames(meteoOut)[[2]] <- colnames(meteo)
  return(meteoOut)
}

#' @export
#' @rdname ConvertMeteoSD
ConvertMeteoSD.character <- function(x, griwrm, meteo) {
  upperBasins <- !is.na(griwrm$down) & griwrm$down == x
  if(all(!upperBasins)) {
    return(meteo[,x])
  }
  upperIDs <- griwrm$id[upperBasins]
  areas <- griwrm$area[match(c(x, upperIDs), griwrm$id)]
  areas[1] <- sum(areas)
  output <- ConvertMeteoSD(
    meteo[,c(x, upperIDs), drop = FALSE],
    areas = areas
  )
  return(output)
}

#' @export
#' @rdname ConvertMeteoSD
ConvertMeteoSD.matrix <- function(x, areas, temperature = FALSE) {
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

