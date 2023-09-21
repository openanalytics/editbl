#' rows_update implementation for data.frame backends.
#' @inheritParams e_rows_update
#' @inherit e_rows_update return details
#' @author Jasper Schelfhout
#' @export
e_rows_update.data.frame <- function(x, y, by = NULL, match = NULL,..., copy = FALSE, in_place = FALSE){
  if(in_place){
    stop("Can not edit in place")
  }
  
  if(is.null(by)){
    by <- colnames(x)[1]
  }
  
  if(is.null(match)){
    match <- list(x = y[by], y = y[by])
  }
  
  idx <- vctrs::vec_match(match$x, x[by])
  
  bad <- which(is.na(idx))
  if (length(bad)) {
    stop("Attempting to update missing rows.")
  }
  
  x[idx, names(y)] <- y
  
  x
}
