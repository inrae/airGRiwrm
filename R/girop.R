#' Generate the list of run-off models and their parameters
#'
#' @param db dataframe containing at least the id and the area of the sub-basin
#' @param cols
#' @param keep_all Keep all column of `db` or keep only columns defined in `cols`
#'
#' @return
#' @export
#'
#' @examples
girop <- function(db, cols = c(id = "id", area = "area", model = "model", params = "params"), keep_all = FALSE) {
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
  class(db) <- append(class(db), "girop")
  db
}
