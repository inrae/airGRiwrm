% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ConvertMeteoSD.R
\name{ConvertMeteoSD}
\alias{ConvertMeteoSD}
\alias{ConvertMeteoSD.GRiwrm}
\alias{ConvertMeteoSD.character}
\alias{ConvertMeteoSD.matrix}
\title{Convert meteorological data from basin scale to sub-basin scale}
\usage{
ConvertMeteoSD(x, ...)

\method{ConvertMeteoSD}{GRiwrm}(x, meteo, ...)

\method{ConvertMeteoSD}{character}(x, griwrm, meteo, ...)

\method{ConvertMeteoSD}{matrix}(x, areas, temperature = FALSE, ...)
}
\arguments{
\item{x}{either a \link{GRiwrm} network description, a \link{character} id of a node, a \link{matrix} containing meteorological data}

\item{...}{Parameters passed to the methods}

\item{meteo}{\link{matrix} or \link{data.frame} containing meteorological data. Its \link{colnames} should be equal to the IDof the basins}

\item{griwrm}{\link{GRiwrm} object describing the semi-distributive network}

\item{areas}{\link{numeric} vector with the total area of the basin followed by the areas of the upstream basins in km^2^}

\item{temperature}{\link{logical} \code{TRUE} if the meteorological data is temperature. if \code{FALSE} minimum output values are bounded to zero}
}
\value{
Either a \link{matrix} containing the converted meteorological data
}
\description{
Convert meteorological data from basin scale to sub-basin scale
}