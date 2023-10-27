#' rows_insert implementation for DBI backends.
#' 
#' @examples
#' library(dplyr)
#' 
#' # Set up a test table
#' conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' artists_df <- data.frame(
#'          ArtistId = c(1,2),
#'          Name = c("AC/DC", "The Offspring")
#' )
#' DBI::dbWriteTable(conn, "Artist", artists_df)     
#'  
#' # Insert new row
#' artists <- tbl(conn, "Artist")
#' DBI::dbBegin(conn)
#' e_rows_insert(artists,
#'  data.frame(ArtistId = 999, Name = "testArtist"),
#'  in_place = TRUE)
#' 
#' DBI::dbRollback(conn)
#' DBI::dbDisconnect(conn)
#' 
#' @inheritParams e_rows_insert
#' @inherit e_rows_insert return details
#' 
#' @author Jasper Schelfhout
#' @export
e_rows_insert.tbl_dbi <- function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE){
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
#' @inheritParams e_rows_update
#' @inherit e_rows_update return details
#' @examples
#' library(dplyr)
#' 
#' # Set up a test table
#' conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' artists_df <- data.frame(
#'          ArtistId = c(1,2),
#'          Name = c("AC/DC", "The Offspring")
#' )
#' DBI::dbWriteTable(conn, "Artist", artists_df)     
#' 
#' # Update rows without changing the key.
#' artists <- tbl(conn, "Artist")
#' DBI::dbBegin(conn)
#' y <- data.frame(ArtistId = 1, Name = "DC/AC")
#' e_rows_update(
#'      x = artists,
#'      y = y,
#'      by = "ArtistId",
#'      in_place = TRUE)
#' DBI::dbRollback(conn)
#' 
#' # Update key values of rows.
#' DBI::dbBegin(conn)
#' y <- data.frame(ArtistId = 999, Name = "DC/AC")
#' match <- list(
#'    x = data.frame("ArtistId" = 1),
#'    y = data.frame("ArtistId" = 999)
#' )
#' e_rows_update(
#'     x = artists,
#'     y = y,
#'     match = match,
#'     by = "ArtistId",
#'     in_place = TRUE)
#' DBI::dbRollback(conn)
#' DBI::dbDisconnect(conn)
#' 
#' @author Jasper Schelfhout
#' @export
e_rows_update.tbl_dbi <- function(x, y, by = NULL, match = NULL,..., copy = FALSE, in_place = FALSE){
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

#' Get name of the tbl in the database
#' @param x `tbl_dbi`
#' @return SQL, the table name as used in the database
get_db_table_name <- function(x){
  name <- coalesce(
      tryCatch({dbplyr::remote_table(x)}, error = function(e){NULL}), # Since dbplyr 2.4.0
      tryCatch({x$lazy_query$x$x}, error = function(e){NULL}), # tbl can apparently get more nested
      tryCatch({x$lazy_query$x}, error = function(e){NULL}), # normal place
      tryCatch({x$ops$x}, error = function(e){NULL}) # old tbl compatibility
  )
        
  if(is.null(name)){
    stop("Can not find in-database table name based on the tibble object.")
  }
  
  if(inherits(name,'dbplyr_table_ident')){
    attributes <- unclass(name)
    id <- list()
    if(!is.na(attributes$table)){
      id$table <- attributes$table
    }
    if(!is.na(attributes$schema)){
      id$schema <- attributes$schema
    }
    if(!is.na(attributes$catalog)){
      id$catalog <- attributes$catalog
    }
    name <- DBI::dbQuoteIdentifier(
        conn = x$src$con,
        x = do.call(DBI::Id, id)
    )
  } else {
    name <- DBI::SQL(name)
  }
    
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
