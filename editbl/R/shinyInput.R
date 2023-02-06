#' An input UI for a data.frame
#'  
#' @param id character module id
#' @param  ... arguments passed onto methods
#' @return HTML. A set of input fields corresponding to the given row.
#' 
#' @author Jasper Schelfhout
#' @export
inputUI <- function(id, ...){
  UseMethod("inputUI")
}

#' An input server for a data.frame
#' @param id character module id
#' @param data single row data.frame
#' @return modified version of data
#' 
#' @author Jasper Schelfhout
inputServer <- function(id, data){
  UseMethod("inputServer")
}

#' UI part for modal with input fields for editing
#' 
#' @details The UI elements that have an id identical to a column name are used for updating the data.
#' 
#' @param id character module id
#' @param data single row data.frame
#' @param colnames named character
#' @param ... for compatibility with method
#' @return HTML. A set of input fields corresponding to the given row.
#' 
#' @author Jasper Schelfhout
#' @export
inputUI.default <- function(id, data, colnames, ...){
  ns <- NS(id)
  
  inputs <- lapply(colnames(data), function(x){
        if(x %in% colnames){
          label = names(colnames)[which(colnames == x)]
        } else {
          label = x
        }
        
        shinyInput(
            x = data[[x]],
            inputId = ns(x),
            label = label, 
            selected = data[,x]
        )
      })
  do.call(tagList,inputs)
}

#' An input server for a data.frame
#' 
#' @details Reads all inputs ids that are identical to column names of the data
#' and updates the data.
#' 
#' @param id character module id
#' @param data single row data.frame
#' @return reactive modified version of data
#' 
#' @author Jasper Schelfhout
inputServer.default <- function(id, data){
  moduleServer(
      id,
      function(input, output, session) {
        newData <- reactive({
              for(col in intersect(colnames(data), names(input))){
                data[,col] = coerceValue(input[[col]], data[,col])
              }
              data
            })
        newData
      }
  )
}

#' Get a shiny input for a column of a tbl
#' @param x column
#' @param inputId shiny input Id
#' @param label character
#' @param selected object of class of x
#' @importFrom shiny checkboxInput numericInput dateInput selectInput textInput
#' @return shiny input
#' 
#' @author Jasper Schelfhout
shinyInput <- function(x, inputId, label, selected){
  if(inherits(x, "logical")){
    shiny::checkboxInput(inputId = inputId, label = label, value = selected)
  }
  else if(inherits(x, "numeric")){
    numericInput(inputId = inputId, label = label, value = selected)
  }
  else if(inherits(x, "Date")){
    dateInput(inputId = inputId, label = label, value = selected)
  }
  else if(inherits(x, "factor")){
    selectInput(inputId = inputId, label = label, choices = sort(levels(x)), selected = selected)
  } else {
    textInput(inputId = inputId, label = label, value = as.character(selected))
  }
}