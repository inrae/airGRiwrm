#' Function which creates screen plots giving an overview of the model outputs in the GRiwrm network
#'
#' @details
#' For examples of use see topics [RunModel.GRiwrmInputsModel], [RunModel_Reservoir],
#' and [RunModel.Supervisor].
#'
#' @param x \[object of class *GRiwrmOutputsModel*\] see [RunModel.GRiwrmInputsModel] for details
#' @param Qobs (optional) [matrix] time series of observed flows
#'        (for the same time steps than simulated) (mm/time step) with one column
#'        by hydrological model output named with the node ID (See [CreateGRiwrm] for details)
#' @param unit (optional) [character] flows unit ("m3/s" or "mm")
#' @param ... Further arguments for [airGR::plot.OutputsModel] and [plot]
#'
#' @return [list] of plots.
#'
#' @importFrom graphics plot par title
#' @export
#'
#'
plot.GRiwrmOutputsModel <- function(x, Qobs = NULL, unit = "m3/s", ...) {

  # Arguments checks
  stopifnot(is.null(Qobs) || is.matrix(Qobs) || is.data.frame(Qobs),
            is.character(unit),
            unit %in% c("mm", "m3/s"))

  griwrm <- attr(x, "GRiwrm")
  ## define outer margins and a title inside it
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(oma = c(0, 0, 3, 0))

  lapply(
    names(x),
    function(id) {
      Qobs_id <- NULL
      if (!is.null(Qobs)) {
        if (id %in% colnames(Qobs)) {
          Qobs_id <- Qobs[,id]
        } else {
          warning("Column \"", id, "\" not found in Qobs")
        }
      }

      BasinArea <- griwrm$area[griwrm$id == id & !is.na(griwrm$model) & griwrm$model != "Diversion"]
      if (unit == "m3/s" && length(BasinArea) == 1 && !is.na(BasinArea)) {
        plot(x[[id]], Qobs = Qobs_id, BasinArea = BasinArea, ...)
      } else {
        plot(x[[id]], Qobs = Qobs_id, ...)
      }
      title(main = id, outer = TRUE, line = 1.2, cex.main = 1.4)
    }
  )
  invisible(NULL)
}
