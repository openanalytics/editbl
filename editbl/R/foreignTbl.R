#' Create a foreign tibble
#' 
#' @details this is a tibble that can be passed onto eDT as a reference.
#' 
#' It is the equivalent of a database table to which the core tbl of eDT has a foreign key.
#' 
#' It will be merged with the core tibble allowing to provide restrictions
#' for certain columns.
#' 
#' Note that it assumes that both the columns from `by` and `naturalKey` are unique in the foreignTbl.
#' This assumption will not be checked since it is an expensive operation on big datasets.
#' 
#' @param x  `tbl`. The child table.
#' @param y `tbl`. The referenced table.
#' @param by `character`. Column names to match on.
#' Note that you should rename and/or typecast the columns in y should they not exactly match the columns in x.
#' @param naturalKey `character`. The columns that form the natural key in y.
#' These are the only ones that can actually get modified, other columns will be deducted from these.
#' @return list
#' 
#' @importFrom dplyr tbl_vars all_of select
#' 
#' @author Jasper Schelfhout
#' @export
foreignTbl <- function(
    x,
    y,
    by = intersect(dplyr::tbl_vars(x), dplyr::tbl_vars(y)),
    naturalKey = dplyr::tbl_vars(y)
){
  stopifnot(all(naturalKey %in% colnames(y)))
  
  if(is.null(names(by))){
    names(by) <- by
  }
  
  clashingNames <- intersect(
      setdiff(dplyr::tbl_vars(y), by),
      dplyr::tbl_vars(x)
  )
  if(length(clashingNames)){
    stop(sprintf("Name clashes on: %s. Please rename.", paste(clashingNames, collapse = ", ")))
  }
  
  x_types <- x %>% select(all_of(by)) %>% getColumnTypeSums %>% unlist()
  y_types <- y %>% select(all_of(by)) %>% getColumnTypeSums %>% unlist()
  typeMisMatches <- names(x_types[x_types != y_types])
  if(length(typeMisMatches)){
    stop(sprintf("%s is not of the same type: %s vs %s. Try casting with dplyr::mutate()",
            typeMisMatches,
            x_types[typeMisMatches],
            y_types[typeMisMatches]))
  }
  
  foreignTbl <- list(
      y = y,
      by = by,
      naturalKey = naturalKey
  )
  
  return(foreignTbl)
}

#' Merge a tbl with it a foreignTbl
#'  
#' @param tbl `tbl`
#' @param foreignTbl `tbl`
#' @param keepNA `boolean` keep rows from tbl with NA keys.
#' @param by named character
#' @param copy boolean
#' @param type character
#' @return tibble `tbl`
#' 
#' @examples 
#' \dontrun{
#' a <- tibble::as_tibble(data.frame(
#'     a = c(1,2,3),
#'     key1 = c(NA,2,4),
#'     key2 = c(NA,2,4)))
#' 
#' b <-  tibble::as_tibble(data.frame(
#'     b = c("a", "b", "c"),
#'     key1 = c(1,2,3),
#'     key2 = c(1,2,3)
#' ))
#' 
#' foreignTbl <- foreignTbl(a,b)
#' 
#' joinForeignTbl(a, foreignTbl)
#' 
#' }
#' 
#' @importFrom dplyr left_join inner_join filter union
#' 
#' @author Jasper Schelfhout
joinForeignTbl <- function(
    tbl,
    foreignTbl,
    keepNA = TRUE,
    by = foreignTbl$by,
    copy = TRUE,
    type = c("inner", "left")[1]){  
  .data <- NULL # for R CMD CHECK
  if(is.null(names(by))){
    names(by) <- by
  }
  
  if(type == "inner"){
    result <- dplyr::inner_join(
        x = tbl,
        y = foreignTbl$y,
        by = by,
        copy = copy)
  } else if(type == "left"){
    result <- dplyr::left_join(
        x = tbl,
        y = foreignTbl$y,
        by = by,
        copy = copy)
  } else {
    stop("only inner and left join supported")
  }

  
  if(keepNA && type == "inner"){
    naTbl <- tbl
    for(key in names(by)){      
      naTbl <- dplyr::filter(naTbl, is.na(.data[[key]]))
    }
    
    naTbl <- dplyr::left_join(
        x = naTbl,
        y = foreignTbl$y,
        by = by,
        copy = copy
        )
    result <- dplyr::union(result, naTbl)
  }
  
  return(result)
}

#' Fill data columns based on foreignTbls
#' 
#' @details
#' When a combination of columns is not found in the foreignTbl,
#' fill the deductedColumns with NA.
#' 
#' @details on foreignTbls suggesting conflicting data,
#' an arbitrary choice is made. It is best to afterwards check with
#' checkForeignTbls to see if a valid result is obtained.
#' 
#' @param tbl `tbl`
#' @param foreignTbls  list of foreign tbls
#' @return tbl
#' 
#' @author Jasper Schelfhout
fillDeductedColumns <- function(tbl, foreignTbls){
  columnOrder <- names(tbl)
  nrow <- nrow(tbl)
  
  # Clear columns that should be deducted
  for (foreignTbl in foreignTbls){
    autoFill <- setdiff(colnames(foreignTbl$y), foreignTbl$naturalKey)
    tbl[,autoFill] <- NULL
  }

  # Fill in columns
  for(foreignTbl in foreignTbls){
    tbl <- joinForeignTbl(tbl, foreignTbl, by = foreignTbl$naturalKey, type = "left")
  }
  
  newNrow <- nrow(tbl)
  
  if(nrow < newNrow){
    stop("One of the given foreign tbls has non unique keys.")
  }
  
  tbl[,columnOrder]
}

#' Check if all rows in tbl fufill foreignTbl constraints
#' @param tbl tibble
#' @param foreignTbls list
#' @return boolean if tbl fufills constraint of all foreign tbls.
#' @importFrom dplyr count pull anti_join
#' 
#' @author Jasper Schelfhout
checkForeignTbls <- function(tbl, foreignTbls){
  n <- NULL # R CMD CHECK fix
  
  for(foreignTbl in foreignTbls){
    nonExisting <- dplyr::anti_join(tbl, foreignTbl$y,
        by = unique(unname(c(foreignTbl$by, foreignTbl$naturalKey))),
        copy = TRUE)
    if(dplyr::pull(dplyr::count(nonExisting), n)){
     stop(sprintf("Invalid %s: %s",
             paste(foreignTbl$naturalKey, collapse = ", "),
             paste(as.character(nonExisting[1,foreignTbl$naturalKey]), collapse = ", ")
             )) 
    }
  }
  TRUE
}

#' Cast all columns that exist in a foreignTbl to factor
#' 
#' @details Can be used to fixate possible options when editing.
#' 
#' @param data data.frame
#' @param foreignTbls list of foreign tbls 
#' @importFrom dplyr distinct select pull tbl_vars
#' @return data.frame
#' 
#' @author Jasper Schelfhout
castToFactor <- function(data, foreignTbls){
  levels <- list()
  for(foreignTbl in foreignTbls){
    for(column in dplyr::tbl_vars(foreignTbl$y)){
      currentLevels <- levels[[column]]
      newRestrictions <- dplyr::pull(dplyr::distinct(dplyr::select(foreignTbl$y, column)))
      if(is.null(currentLevels)){
        levels[[column]] <- newRestrictions
      } else {
        levels[[column]] <- intersect(currentLevels, newRestrictions)
      }
    }
  }
  
  for(column in intersect(dplyr::tbl_vars(data), names(levels))){
    data[[column]] <- factor(data[[column]], levels = levels[[column]])
  }
  data
}

#' Get all columns that are not natural keys
#' @param foreignTbls list
#' @return character
#' 
#' @author Jasper Schelfhout
getNonNaturalKeyCols <- function(foreignTbls){
  result <- c()
  for(foreignTbl in foreignTbls){
    cols <- as.character(dplyr::tbl_vars(foreignTbl$y))
    naturalKey <- foreignTbl$naturalKey
    result <- unique(c(result, setdiff(cols, naturalKey)))
  }
  result
}

