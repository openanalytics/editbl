#' rows_insert implementation for data.table backends.
#' 
#' @inheritParams dplyr::rows_insert
#' @inherit dplyr::rows_insert return details
#' 
#' @author Jasper Schelfhout
#' @export
rows_insert.dtplyr_step <- function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE){  
  x_dt <- data.table::copy(data.table::as.data.table(x))
  y_dt <- data.table::as.data.table(y)
  
  if(is.null(by)){
    by <- colnames(x)[1]
  }
  
  if(in_place){
    stop("Adding rows by reference to a data.table is not supported yet.")
  } else {
    result <- dtplyr::lazy_dt(rbind(x_dt,y_dt))
  }
  
  return(result)
}

#' rows_delete implementation for data.table backends.
#' @inheritParams dplyr::rows_delete
#' @inherit dplyr::rows_delete return details
#' @export
#' @author Jasper Schelfhout
rows_delete.dtplyr_step <- function(x, y, by = NULL, ..., unmatched, copy = FALSE, in_place = FALSE){  
  x_dt <- data.table::copy(data.table::as.data.table(x))
  y_dt <- data.table::as.data.table(y)
  
  if(!nrow(y_dt)){
    return(x)
  }
  
  if(is.null(by)){
    by <- colnames(x_dt)[1]
  }
  
  if(in_place){
    stop("In place deletes for data.tables are not supported yet.
 See issue: https://github.com/Rdatatable/data.table/issues/635")
  }
  
  matches <- unlist(lapply(seq_len(nrow(x_dt)), function(i){
        nrow(merge(x_dt[i,], y_dt, by = by)) > 0
      }))
  
  x_dt <- x_dt[!matches,]
  result <- dtplyr::lazy_dt(x_dt)
  
  result
}



#' rows_update implementation for data.table backends.
#' @inheritParams dplyr::rows_update
#' @param match named list consisting out of two equal length \code{data.frame}'s with columns defined in \code{by}.
#' This allows for updates of columns defined in by.
#' @inherit dplyr::rows_update return details
#' @author Jasper Schelfhout
#' @export
rows_update.dtplyr_step <- function(x, y, by = NULL, match = NULL,..., copy = FALSE, in_place = FALSE){
  args <- c(as.list(environment()), list(...))
  
  x_dt <- data.table::as.data.table(x)
  y_dt <- data.table::as.data.table(y)
  
  if(!nrow(y_dt)){
    return(x)
  }
  
  if(is.null(by)){
    by <- colnames(x_dt)[1]
  }
  
  if(is.null(args$match)){ # Be explicit about argument since otherwhise base::match will used.
    match <- list(x = y_dt[,by, with = FALSE, drop = FALSE],
        y = y_dt[,by, with = FALSE, drop = FALSE])
  }
  
  for (i in seq_len(nrow(match$y))){
    yMatch <-  match$y[i,, drop = FALSE]
    values <- merge(yMatch, y, by = by)[,colnames(y_dt)]
    xMatch <-  match$x[i,, drop = FALSE]
    
    xRows <- which(unlist(lapply(seq_len(nrow(x_dt)), function(i){
                  nrow(merge(x_dt[i,], xMatch, by = by)) > 0
                })))
    
    x_dt[xRows,] <- values
  }
  
  result <- dtplyr::lazy_dt(x_dt)
  return(invisible(result))  
}
