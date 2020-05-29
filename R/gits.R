#' Title
#'
#' @param id string of the id of the node
#' @param ts numeric matrix or data frame containing 3 columns for precipitation, evaporation, and observed flow
#' @param cols named list or vector used for matching the columns of ts with the required columns names which are "Precip", "PotEvap", and "Qobs".
#'
#' @return \emph{Gits} class object which is a list containing a `date` element (Vector of PosiXlt timestamps) and an element named the id of the node containing a dataframe with observed data.
#' @export
Gits <- function(id, ts,
                 cols = list(date = "date", Precip = "Precip", PotEvap = "PotEvap", Qobs = "Qobs")) {

  cols <- as.list(cols)
  ts <- dplyr::rename(ts, unlist(cols))

  if(any(is.na(ts$Qobs))) {
    stop("Qobs should not contain any NA")
  }
  if(any(ts$Qobs < 0)) {
    stop("Qobs should be strictly positive")
  }

  gitsOut <- list(date = ts$date)
  cols$date <- NULL
  gitsOut[[id]] <- dplyr::select(ts, names(cols))
  class(gitsOut) <- append(class(gitsOut), "Gits")
  gitsOut
}

#' Merge two gits objects with identical date time series.
#'
#' @param x Gits object to merge (See [Gits]).
#' @param y Gits object to merge (See [Gits]).
#' @param ... For merge generic function compatibility.
#'
#' @return Gits object merged with one item `Date` and Items corresponding to each node.
#' @export
merge.Gits <- function(x, y, ...) {
  if(!is(y, "Gits")) {
    stop("A Gits class object can only be merged with a Gits class object")
  }
  if(! identical(x$date, y$date)) {
    stop("Time series dates are not identical")
  }
  y$date <- NULL
  x <- utils::modifyList(x, y)
}
