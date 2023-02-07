#' @importFrom tibble as_tibble
testImports <- NULL

#' cast columns to the type of the template
#' 
#' @param template data.frame
#' @param x data.frame
#' 
#' @details only affects columns in both the template and x
#' 
#' @importFrom DT coerceValue
coerceColumns <- function(template, x){
  for(col in intersect(names(x), names(template))){
    x[[col]] <-   DT::coerceValue(x[[col]], template[[col]])
  }
  x
}

#' DT coerceValue with better POSIXct support
#' @inheritParams DT::coerceValue
#' @importFrom DT coerceValue
#' 
#' @author Jasper Schelfhout
coerceValue <- function(val,old){
  if (inherits(old, c('POSIXlt', 'POSIXct'))) {  
    
    # Try a bunch of formats supported by ISO
    newVal <- strptime(val, '%Y-%m-%dT%H:%M:%SZ', tz = 'UTC')
    if(is.na(newVal)){
      newVal <- strptime(val, '%Y-%m-%d %H:%M:%SZ', tz = 'UTC')
    }
    if(is.na(newVal)){
      newVal <- strptime(val, '%Y-%m-%d %H:%M:%S', tz = 'UTC')
    }
    if(is.na(newVal)){
      newVal <- strptime(val, '%Y-%m-%dT%H:%M:%S', tz = 'UTC')
    }
    
    if (inherits(old, 'POSIXlt')) return(newVal)
    return(as.POSIXct(newVal))
  } else {
    return(DT::coerceValue(val,old))
  }
}

#' Connect to your database.
#'  
#' @param dbname \code{character(0)}
#' @param drv database driver
#' @param ... arguments passed to `DBI::dbConnect`
#' @importFrom utils packageName
#' @return database connection
connectDB <- function(
    dbname = system.file("extdata", "chinook.db", package = utils::packageName()),
    drv = RSQLite::SQLite(),
    ...
    ){
  DBI::dbConnect(
      dbname = dbname,
      drv = drv,
      ...
      )
}

#' Return first non null argument
#' @param ... set of arguments
#' 
#' @author Jasper Schelfhout
coalesce <- function(...){
  args <- list(...)
  result <- NULL
  for(arg in args){
    if(!is.null(arg)){
      return(arg)
    }
  }
  return(result)
}

#' Cast columns in data.frame to editable types in datatable
#' @param data data.frame
#' @param cols character columns perform casting on.
#' @return data.frame with some columns cast to another type
#' 
#' @author Jasper Schelfhout
castForDisplay <- function(data, cols = colnames(data)){
  stopifnot(cols %in% colnames(data))
  
  for(col in cols){
    if(!inherits(data[[col]], c("integer", "character"))){
      data[[col]] <- as.character(data[[col]])
    }
  }
  data
}

#' Cast data to tbl
#' @param data object
#' @return tbl
#' 
#' @importFrom dplyr as_tibble is.tbl
#' 
#' @author Jasper Schelfhout
castToTbl <- function(data){
  if(dplyr::is.tbl(data)){
    result <- data
  } else if(inherits(data, 'data.table')){
    result <- dtplyr::lazy_dt(data)
  } else {
    result <- dplyr::as_tibble(data)
  }
  result
}

#' Cast data from tbl to class of template
#' @param tbl tbl
#' @param template tabular object 
#' @return tbl cast to the type of template
#' @importFrom dplyr is.tbl
#' 
#' @author Jasper Schelfhout
castFromTbl <- function(tbl, template){
  if(dplyr::is.tbl(template)){
    result <- tbl
  } else if (inherits(template, "data.table")){
    result <- data.table::as.data.table(tbl)
  } else if (inherits(template, "data.frame")){
    result <- as.data.frame(tbl)
  } else {
    stop(sprintf("Can not convert tbl to class '%s'.",class(template)))
  }
  result
}

#' Standardize to colnames argument to the format of named character vector
#' @param x colnames argument to eDT
#' @param data data argument to eDT
#' @importFrom dplyr tbl_vars
#' @return named character vector
#' 
#' @author Jasper Schelfhout
standardizeColnamesArgument <- function(x, data){
  if(is.null(x)){
    result <- as.character(dplyr::tbl_vars(data))
    names(result) <- result
  } else if (is.numeric(x)) {
    result <- dplyr::tbl_vars(data)[x]
    names(result) = names(x)
  } else if (is.character(x)){
    if(!is.null(names(x))){
     result <- x
    } else {
     result <- dplyr::tbl_vars(data)[seq_len(length(x))] 
     names(result) <- x
    }
  }
  result
}


#' Replace instances of integer64 with actual NA values instead of weird default 9218868437227407266
#' 
#' @details https://github.com/Rdatatable/data.table/issues/4561
#' 
#' @param x data.frame
#' @return x with integer64 columns set to bit64::as.integer64(NA)
#' 
#' @author Jasper Schelfhout
fixInteger64 <- function(x){
  for(column in dplyr::tbl_vars(x)){
    if(inherits(x[[column]], "integer64")){
      x[[column]] <- rep(bit64::as.integer64(NA), nrow(x))
    }
  }
  x
}

#' Get types of columns in a tbl
#' @param tbl tbl
#' @return named list with types of the colums
#' 
#' @importFrom dplyr type_sum collect
#' @importFrom utils head
#' 
#' @author Jasper Schelfhout
getColumnTypeSums <- function(tbl){
  tbl %>% 
      head %>% 
      collect %>% 
      lapply(dplyr::type_sum)
}
