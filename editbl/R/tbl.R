#' @importFrom dplyr rows_delete
NULL

#' Insert rows into a tibble
#' @details Mainly a wrapper around \code{\link[dplyr]{rows_insert}}.
#' Allows for specific implementations should the behavior differ from what's needed by `editbl`.
#' Reason for separate method is to avoid conflicts on package loading.
#' 
#' @inheritParams dplyr::rows_insert
#' @inherit dplyr::rows_insert return details
#' @export
e_rows_insert <- function(
    x,
    y,
    by = NULL,
    ...,
    conflict = c("error", "ignore"),
    copy = FALSE,
    in_place = FALSE){
  UseMethod("e_rows_insert")
}

#' @inherit e_rows_insert
#' @export
#' @importFrom dplyr rows_insert
e_rows_insert.default <- function(
    x,    
    y,
    by = NULL,
    ...,
    conflict = c("error", "ignore"),
    copy = FALSE,
    in_place = FALSE){
  dplyr::rows_insert(
      x = x,
      y = y,
      by = by,
      ... ,
      conflict = conflict,
      copy = copy,
      in_place = in_place)
}

#' Update rows of a tibble
#' @details Mainly a wrapper around \code{\link[dplyr]{rows_update}}.
#' Allows for specific implementations should the behavior differ from what's needed by `editbl`.
#' Reason for separate method is to avoid conflicts on package loading.
#' @param match named `list` consisting out of two equal length `data.frame`'s with columns defined in `by`.
#' This allows for updates of columns defined in `by`.
#' @inheritParams dplyr::rows_update
#' @inherit dplyr::rows_update return details
#' @export
e_rows_update <- function(
    x,
    y,
    by = NULL,
    ...,
    match,
    unmatched = c("error", "ignore"),
    copy = FALSE,
    in_place = FALSE){
  UseMethod("e_rows_update")
}

#' @inherit e_rows_update
#' @importFrom dplyr rows_update
#' @export
e_rows_update.default <- function(
    x,
    y,
    by = NULL,
    ...,
    match = match,
    unmatched = c("error", "ignore"),
    copy = FALSE,
    in_place = FALSE){
  dplyr::rows_update(
      x = x,
      y = y,
      by = by,
      ... ,
      match = match,
      unmatched = unmatched,
      copy = copy,
      in_place = in_place
  )
}

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