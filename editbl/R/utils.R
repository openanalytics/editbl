#' @importFrom tibble as_tibble
#' @import fontawesome
testImports <- NULL

#' Cast columns to the type of the template
#' 
#' @param template `data.frame`
#' @param x `data.frame`
#' 
#' @details only affects columns in both the template and x
coerceColumns <- function(template, x){
  for(col in intersect(names(x), names(template))){
    x[[col]] <-   coerceValue(x[[col]], template[[col]])
  }
  x
}

#' `DT::coerceValue` with better `POSIXct` support
#' 
#' @details Will assume UTC in case no timezone is specified.
#' 
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
  } else if(inherits(old, "logical")){
    newVal <- as.logical(val)
  } else {
    return(DT::coerceValue(val,old))
  }
}

#' Connect to a database.
#'  
#' @details Connects by default to a test SQLite database originally obtained here:
#' [chinook_git](https://github.com/lerocha/chinook-database/blob/master/ChinookDatabase/DataSources/Chinook_Sqlite.sqlite)
#' 
#' @param dbname `character(0)`
#' @param drv database driver
#' @param ... arguments passed to `DBI::dbConnect`
#' @importFrom utils packageName
#' @examples 
#' 
#' conn <- connectDB()
#' DBI::dbDisconnect(conn)
#' 
#' @return database connection
#' @export
connectDB <- function(
    dbname = system.file("extdata", "chinook.sqlite", package = utils::packageName()),
    drv = RSQLite::SQLite(),
    ...
    ){
  DBI::dbConnect(
      dbname = dbname,
      drv = drv,
      ...
      )
}

#' Return first non `NULL` argument
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

#' Cast columns in `data.frame` to editable types in datatable
#' @param data `data.frame`
#' @param cols `character` columns to perform casting on.
#' @return `data.frame` with some columns cast to another type
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

#' Cast `tbl` or `data.frame` x to the types of the template
#' 
#' @details If template is a `tbl` with database source, convert to an in-memory tibble with same data types instead.
#' @details Rownames might differ or get lost.
#' 
#' @param x `data.frame`, `tbl` or `data.table`
#' @param template `data.frame`, `tbl` or `data.table`
#' @return object containing data of x in the class and structure of the template.
#' 
#' @author Jasper Schelfhout
castToTemplate <- function(x, template){
  if(!all(base::colnames(x) == base::colnames(template)))
    stop("Template and casted tbl should have exactly the same colums")
  
  rowNames <- attr(x, 'row.names')
  
  result <- rbind(
      dplyr::collect(dplyr::filter(template, dplyr::row_number()==1)),
      x
  )[-1,]
  
  # Tbl doesn't properly support row names
  if(!inherits(template, 'tbl')){
    try({rownames(result) <- rowNames}, silent = TRUE)
  }
 
  result
}

#' Cast tbl to class of template
#' @param tbl `tbl`
#' @param template tabular object like `data.frame` or `data.table` or `tbl`.
#' @return tbl cast to the type of template
#' @importFrom dplyr is.tbl
#' 
#' @author Jasper Schelfhout
castFromTbl <- function(tbl, template){
  if(dplyr::is.tbl(template)){
    result <- tbl
  } else {
    result <- castToTemplate(tbl,template)
  }
  result
}

#' Standardize colnames argument to the format of named character vector
#' @inheritParams eDT
#' @importFrom dplyr tbl_vars
#' @return named character vector
#' 
#' @author Jasper Schelfhout
standardizeArgument_colnames <- function(colnames, data){
  if(is.null(colnames)){
    result <- as.character(dplyr::tbl_vars(data))
    names(result) <- result
  } else if (is.numeric(colnames)) {
    result <- dplyr::tbl_vars(data)[colnames]
    names(result) = names(colnames)
  } else if (is.character(colnames)){
    if(!is.null(names(colnames))){
     result <- colnames
    } else {
     result <- dplyr::tbl_vars(data)[seq_len(length(colnames))] 
     names(result) <- colnames
    }
  }
  result
}

#' Standardized editable argument to be in the form of a list
#' @inheritParams eDT
#' @return list of the form `list(target = foo, ...)`
#' 
#' @author Jasper Schelfhout
standardizeArgument_editable <- function(
    editable,
    data
    ){
  if(is.logical(editable)){
    if(editable){
      return(list(target = "cell"))
    } else {
      return(list(target = "cell", disable = list(columns = seq_len(ncol(data))))) 
    }
  }

  if(is.character(editable)){
      return(list(target = editable))
  }
  
  if(is.list(editable)){
    return(editable)
  }
  
  stop("editable is not in a standard format.")
}

#' Replace instances of integer64 with actual NA values instead of weird default 9218868437227407266
#' 
#' @details [github issue](https://github.com/Rdatatable/data.table/issues/4561)
#' 
#' @param x `data.frame`
#' @return x with `integer64` columns set to `bit64::as.integer64(NA)`
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
#' @param tbl `tbl`
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

#' Generate a custom button for \code{\link{eDT}}
#' 
#' @details
#' Combines elements of `shiny::actionButton` and [datatable options](https://datatables.net/reference/option/)
#' 
#' @param id `character(1)`, namespaced id
#' @param label `character(1)`
#' @param icon `shiny::icon`
#' @param disabled `logical`. Whether or not the button should start in a disabled state.
#' @return list to be used in `eDT(options = list(buttons = xxx))`
#' 
#' @examples
#' if(interactive()){
#' 
#'   ui <- eDTOutput("data")
#'   server <- function(input,output,session){
#'     b <- customButton('print', label = 'print')
#'     eDT_result <- eDT(id = "data", mtcars, options = list(buttons = list("save", b)))
#'     observeEvent(input$print,{
#'           print(eDT_result$state())
#'     })
#'   }
#'   shinyApp(ui,server)
#' }
#' 
#' @author Jasper Schelfhout
#' @export
customButton <- function(id, label, icon = "", disabled = FALSE){
      list(
          attr = list(
              id = id,
              class = "btn btn-default action-button shiny-bound-input",
              disabled = disabled
              ),
          extend = "",
          text = paste(as.character(icon), label, sep = " "),
          action = DT::JS(sprintf("function (e, dt, node, config ) {
                      Shiny.setInputValue('%s', true, {priority: 'event'});
                      }", id))
      )
}
