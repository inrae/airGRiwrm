#' Generate the list of run-off models and their parameters
#'
#' @param db data frame containing at least the id the area and the model of the sub-basin.
#' @param cols named list or vector for matching columns of `db` parameter. By default, mandatory columns names are: `id`, `area`, `model`. But other names can be handled with a named list or vector containing items defined as `"required name" = "column name in db"`.
#' @param keep_all Keep all column of `db` or keep only columns defined in `cols`
#'
#' @return \emph{Girop} object.
#' @export
Girop <- function(db, cols = c(id = "id", area = "area", model = "model", params = "params"), keep_all = FALSE) {
  colsDefault <- list(id = "id", area = "area", model = "model", params = "params")
  cols <- utils::modifyList(colsDefault, as.list(cols))
  if(!any(names(db) == cols$params)) {
    # Add missing params column in the database
    db[[cols$params]] = NA
  }
  db <- dplyr::rename(db, unlist(cols))
  if(!keep_all) {
    db <- dplyr::select(db, names(cols))
  }
  class(db) <- append(class(db), "Girop")
  db
}
