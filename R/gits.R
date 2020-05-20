#' Title
#'
#' @param id
#' @param ts
#' @param cols
#'
#' @return
#' @export
#'
#' @examples
Gits <- function(id, ts,
                 cols = list(date = "date", Precip = "Precip", PotEvap = "PotEvap", Qobs = "Qobs")) {

  cols <- as.list(cols)
  ts <- dplyr::rename(ts, unlist(cols))
  gitsOut <- list(date = ts$date)
  cols$date <- NULL
  gitsOut[[id]] <- dplyr::select(ts, names(cols))
  class(gitsOut) <- append(class(gitsOut), "Gits")
  gitsOut
}

#' Title
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
merge.Gits <- function(x, y) {
  browser()
  if(!is(y, "Gits")) {
    stop("A Gits class object can only be merged with a Gits class object")
  }
  if(! identical(x$date, y$date)) {
    stop("Time series dates are not identical")
  }
  y$date <- NULL
  x <- utils::modifyList(x, y)
}
