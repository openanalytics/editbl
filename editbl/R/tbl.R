# General methods being experimental dplyr methods
# Custom implementations introduce new 'match' argument in rows_update() not
# supported by dplyr (yet).
# Once as similar argument is introduced, full dplyr compatibility 
# is aimed for.

#' @importFrom dplyr rows_update rows_insert rows_delete
#' @export
NULL

#' Start a transaction for a tibble
#' @param tbl `tbl`
#' 
#' @author Jasper Schelfhout
beginTransaction <- function(tbl){
  if(inherits(tbl, "tbl_dbi")){
    DBI::dbBegin(tbl$src$con)
  }
}

#' Start a transaction for a tibble
#' @param tbl `tbl`
#' 
#' @author Jasper Schelfhout
commitTransaction <- function(tbl){
  if(inherits(tbl, "tbl_dbi")){
    DBI::dbCommit(tbl$src$con)
  }
}

#' Start a transaction for a tibble
#' @param tbl `tbl`
#' 
#' @author Jasper Schelfhout
rollbackTransaction <- function(tbl){
  if(inherits(tbl, "tbl_dbi")){
    DBI::dbRollback(tbl$src$con)
  }
}