#' Plot simulated reservoir volume, inflows and released flows time series on a reservoir node
#'
#' @param x Object returned by [RunModel_Reservoir]
#' @param ... Further arguments passed to [plot.Qm3s]
#'
#' @return Function used for side effect.
#' @export
#'
#' @example man-examples/RunModel_Reservoir.R
plot.OutputsModelReservoir <- function(x, ...) {
  oldpar <- par(mfrow=c(2,1),
                mar = c(2,3.3,1.2,0.5),
                mgp = c(2,1,0))
  df <- data.frame(DatesR = x$DatesR,
                   Inflows = x$Qinflows_m3 / x$RunOptions$TimeStep,
                   Releases = x$Qsim_m3 / x$RunOptions$TimeStep)
  plot.Qm3s(df, ...)

  Vres <- data.frame(DatesR = x$DatesR,
                     Storage = x$Vsim / 1E6)
  plot.Qm3s(Vres,
            main = "Simulated reservoir storage",
            ylab = expression("Storage (Mm"^"3" * ")"), ...)
  par(oldpar)
}
