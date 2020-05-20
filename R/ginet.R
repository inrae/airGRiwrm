#' Generate a network description containing all hydraulic nodes and the description
#' of their connections
#'
#' @param db A tibble or a dataframe containing at least the id and the description of the connections
#' @param cols
#' @param keep_all Keep all column of `db` or keep only columns defined in `cols`
#'
#' @return
#' @export
#'
#' @examples
Ginet <- function(db, cols = list(id = "id", down = "down", length = "length", runoff = "runoff"), keep_all = FALSE) {
  colsDefault <- list(id = "id", down = "down", length = "length", runoff = "runoff")
  cols <- utils::modifyList(colsDefault, as.list(cols))
  db <- dplyr::rename(db, unlist(cols))
  if(!keep_all) {
    db <- dplyr::select(db, names(cols))
  }
  class(db) <- append(class(db), "Ginet")
  db
}
