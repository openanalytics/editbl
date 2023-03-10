#' rows_insert implementation for DBI backends.
#' 
#' @examples
#' \dontrun{
#' # insert a new row
#' conn <- connectDB()
#' artists <- dplyr::tbl(conn, "Artists")
#' rows_insert.tbl_dbi(artists, data.frame(ArtistId = 999, Name = "testArtist"), in_place = TRUE)
#' }
#' 
#' @inheritParams dplyr::rows_insert
#' @inherit dplyr::rows_insert return details
#' 
#' @author Jasper Schelfhout
#' @export
rows_insert.tbl_dbi <- function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE){
  if(!in_place){
    stop("Can only edit in place")
  }
  
  if(copy){
    stop("copy TRUE not supported yet.")
  }
  
  if(is.null(by)){
    by <- colnames(x)[1]
  }
  
  if(in_place){
    lapply(seq_len(nrow(y)), function(i){
          rowInsert(
              conn = x$src$con,
              table =  get_db_table_name(x),
              values = as.list(y[i,])
          )
        })
    return(invisible(x))
  }
  return(x)
}


#' rows_update implementation for DBI backends.
#' @inheritParams dplyr::rows_update
#' @param match named list consisting out of two equal length \code{data.frame}'s with columns defined in \code{by}.
#' This allows for updates of columns defined in by.
#' @inherit dplyr::rows_update return details
#' @examples
#' \dontrun{
#' # update rows.
#' conn <- connectDB()
#' artists <- dplyr::tbl(conn, "Artists")
#' y <- data.frame(ArtistId = 1, Name = "DC/AC")
#'  rows_update.tbl_dbi(
#'      x = artists,
#'      y = y,
#'      by = "ArtistId",
#'      in_place = TRUE)
#' 
#' # update key values of rows.
#' conn <- connectDB()
#' artists <- dplyr::tbl(conn, "Artists")
#' y <- data.frame(ArtistId = 999, Name = "DC/AC")
#' match <- list(
#'    x = data.frame("ArtistId" = 1),
#'    y = data.frame("ArtistId" = 999)
#' )
#' rows_update.tbl_dbi(
#'     x = artists,
#'     y = y,
#'     match = match,
#'     by = "ArtistId",
#'     in_place = TRUE)
#' }
#' @author Jasper Schelfhout
#' @export
rows_update.tbl_dbi <- function(x, y, by = NULL, match = NULL,..., copy = FALSE, in_place = FALSE){
  if(!in_place){
    stop("Can only edit in place")
  }
  
  if(copy){
    stop("copy TRUE not supported yet.")
  }
  
  if(is.null(by)){
    by <- colnames(x)[1]
  }
  
  if(is.null(match)){
    match <- list(x = y[by], y = y[by])
  }
  
  lapply(seq_len(nrow(match$y)), function(i){
        yMatch <-  match$y[i,, drop = FALSE]
        values <- merge(yMatch, y, by = by)   
        xMatch <-  match$x[i,, drop = FALSE]
        
        rowUpdate(
            conn = x$src$con,
            table =  get_db_table_name(x),
            values = as.list(values),
            where = as.list(xMatch)
        )
      })
  return(invisible(x))  
}


#' rows_delete implementation for DBI backends.
#' @inheritParams dplyr::rows_delete
#' @inherit dplyr::rows_delete return details
#' @examples
#' \dontrun{
#' # delete rows
#' conn <- connectDB()
#' artists <- dplyr::tbl(conn, "Artists")
#' y <- data.frame(ArtistId = 999)
#'  rows_delete.tbl_dbi(
#'      x = artists,
#'      y = y,
#'      by = "ArtistId",
#'      in_place = TRUE)
#' }
#' 
#' @author Jasper Schelfhout
#' @export
rows_delete.tbl_dbi <- function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE){
  if(!in_place){
    stop("Can only edit in place")
  }
  
  if(copy){
    stop("copy TRUE not supported yet.")
  }
  
  if(is.null(by)){
    by <- colnames(x)[1]
  }
  
  if(in_place){
    lapply(seq_len(nrow(y)), function(i){
          row <- y[i,,drop = FALSE]
          rowDelete(
              conn = x$src$con,
              table = get_db_table_name(x),
              where = as.list(row[by])
          )
        })
    return(invisible(x))
  }
  return(x)
}

#' get name of the tbl in the database
#' @param x tbl with DBI connection
#' @return SQL
get_db_table_name <- function(x){
  # FIXME: figure out what the perfect way is of retrieving the table identity
  # form a tibble
  name <- coalesce(
      tryCatch({x$lazy_query$x$x}, error = function(e){NULL}), # tbl can apparently get more nested
      tryCatch({x$lazy_query$x}, error = function(e){NULL}), # normal place
      tryCatch({x$ops$x}, error = function(e){NULL}) # old tbl compatibility
  )
        
  if(is.null(name)){
    stop("Can not find in-database table name based on the tibble object.")
  }
  name <- DBI::SQL(name)
  return(name)
}

#' Add a row to a table in the database.
#' 
#' @param conn database connection object as given by \code{\link[DBI]{dbConnect}}.
#' @param table character
#' @param values named list, row to add. Names are database column names. Unspecified columns will get database defaults.
#' @return integer number of affected rows.
rowInsert <- function (
    conn,
    table,
    values){

  for(i in seq_along(values)){
      values[[i]] <- castToSQLSupportedType(values[[i]])
  }
  
  # Treat missing values as NULL and use database default.
  # This might need to be a flag in the function.
  # E.g. there is a difference between wanting to insert NA or let a database generate a default.
  values <- values[!is.na(values)]

  query <- glue::glue_sql(
      .con = conn,
      "INSERT INTO {`table`} ({`columns`*}) VALUES ({values*})",
      table = table,
      columns = names(values),
      values = values
  )
  
  DBI::dbExecute(
      conn,
      query)
}


#' Update rows in the database.
#' 
#' @param conn database connection object as given by \code{\link[DBI]{dbConnect}}.
#' @param table character
#' @param values named list, values to be set. Names are database column names.
#' @param where named list, values to filter on. Names are database column names. If NULL no filter is applied.
#' @return integer number of affected rows.
rowUpdate <- function (
    conn,
    table,
    values,
    where){
  values <- lapply(values, function(x){
        castToSQLSupportedType(x)
      })
  
  query <- glue::glue_sql(
      .con =conn,
      "UPDATE {`table`} SET ({`columns`*}) = ({values*})",
      table = table,
      columns = names(values),
      values = values
  )
  
  if(length(where)){
    whereSQL  <- lapply(seq_along(where), function(i){
          whereSQL(
              conn = conn,
              table =  table,
              column = names(where)[i],
              operator = "in",
              values = where[[i]]
          )   
        })
    whereSQL <- paste(whereSQL, collapse = " AND ")
    query <- paste(query, "WHERE", whereSQL)
  }
  
  DBI::dbExecute(
      conn,
      query)
}

#' Delete rows from a table in the db
#' 
#' @param conn database connection object as given by \code{\link[DBI]{dbConnect}}.
#' @param table character
#' @param where named list, values to filter on. Names are database column names. If NULL no filter is applied and all rows are deleted.
#' @return integer number of affected rows.
rowDelete <- function (
    conn,
    table,
    where){
  query <- glue::glue_sql(
      .con =conn,
      "DELETE FROM {`table`}",
      table = table
  )
  
  if(length(where)){
    whereSQL  <- lapply(seq_along(where), function(i){
          whereSQL(
              conn = conn,
              table =  table,
              column = names(where)[i],
              operator = "in",
              values = where[[i]]
          )   
        })
    whereSQL <- paste(whereSQL, collapse = " AND ")
    query <- paste(query, "WHERE", whereSQL)
  }
  
  DBI::dbExecute(
      conn,
      query)
}

#' Cast the data type to something supported by SQL.
#' @param x single value or vector of values
#' @return x, possibly cast to different type
#' 
#' @author Jasper Schelfhout
castToSQLSupportedType <- function(x){
  if(!inherits(x, c("numeric", "character"))){
    x <- as.character(x)
  }
  x
}

#' Generate where sql
#' 
#' @param conn database connection object as given by \code{\link[DBI]{dbConnect}}.
#' @param table character table name (or alias used in query)
#' @param column character column of table
#' @param values character vector of values
#' @param operator character
#' @return character sql
#' 
#' @author Jasper Schelfhout
whereSQL <- function(
    conn,
    table,
    column,
    operator = 'in',
    values = NULL){
  
   values <- castToSQLSupportedType(values)
 
  if(length(values) > 0){  
    
    singleOperators <- c("like", "not like", "=", "!=", ">", "<", ">=", "<=", "<>")
    multiOperators <- c("in", "not in")
    if(length(values) == 1){
      operators <- c(singleOperators, multiOperators)
    } else {
      operators <- multiOperators
    } 
    
    if(!operator %in% operators){
      stop(sprintf("Should use on of following operators: %s",
              paste(operators, collapse = ", ")))
    }
    
    sql <- sprintf("{`table`}.{`column`} %s ({values*})",
        operator)
    sql <-  glue::glue_sql(
        .con = conn,
        sql,
        table = table,
        column = column,
        operator = operator,
        values = values
    )
  } else{
    sql = "TRUE"
  }
  sql
}
