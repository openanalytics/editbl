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
#' @param ... further arguments for methods
#' @return modified version of data
#' 
#' @author Jasper Schelfhout
inputServer <- function(id, data, ...){
  UseMethod("inputServer")
}

#' UI part for modal with input fields for editing
#' 
#' @details The UI elements that have an id identical to a column name are used for updating the data.
#' 
#' @param id character module id
#' @param ... for compatibility with method
#' @return HTML. A set of input fields corresponding to the given row.
#' 
#' @author Jasper Schelfhout
inputUI.default <- function(id, ...){
  ns <- NS(id)
  uiOutput(ns("inputUI"))
}

#' An input server for a data.frame
#' 
#' @details Reads all inputs ids that are identical to column names of the data
#' and updates the data.
#' 
#' @param id character module id
#' @param data single row data.frame
#' @param notEditable character columns that should not be edited
#' @param  colnames named character
#' @param foreignTbls list of foreignTbls
#' @param ... for compatibility with other methods
#' @return reactive modified version of data
#' 
#' @author Jasper Schelfhout
inputServer.default <- function(id, data, colnames, notEditable, foreignTbls, ...){  
  moduleServer(
      id,
      function(input, output, session) {
        ns <- session$ns
        rv <- reactiveValues(inputDTData = list())
        
        # Make arguments reactive
        # Need to be explicit about environement. Otherwhise they overwrite themselves.
        # This way users can pass on both reactive an non reactive arguments
        argEnv <- parent.frame(3)
        args <- c(as.list(argEnv))
        args$id <- NULL
        for(arg in names(args)){
          if(!shiny::is.reactive(args[[arg]])){
            eval(parse(text = sprintf("assign(arg, shiny::reactive(%s, env = argEnv))", arg)))
          }
        }
        
        inputDTs <- reactive({
              inputDTs <- lapply(foreignTbls(), function(x){
                    if(length(x$naturalKey) > 1){
                      id <- gsub("-", "_", force(uuid::UUIDgenerate()))
                      list(
                          id = id,
                          choices = dplyr::collect(
                              dplyr::select(
                                  x$y,
                                  dplyr::all_of(x$naturalKey)
                              )
                          ),
                          selected = data()[,x$naturalKey]
                      )
                    }
                  })
              if(length(inputDTs)){
                inputDTs[sapply(inputDTs, is.null)] <- NULL
              }
              inputDTs
            })
        
        observe({
              inputDTData <- list()
              for(inputDT in inputDTs()){
                inputDTData[[inputDT$id]] <- do.call(selectInputDT_Server, inputDT)
              }
              rv$inputDTData <- inputDTData
            })
        
        inputDTCols <- reactive({
              unique(unlist(lapply(inputDTs(), function(x){
                            names(x$choices)
                          })))
            })
        
        normalInputs <- reactive({
              uiData <- data()[,setdiff(base::colnames(data()), notEditable())]
              uiData <- castToFactor(uiData, foreignTbls())        
              inputNormalCols <- setdiff(base::colnames(uiData), inputDTCols())
              
              inputs <- lapply(inputNormalCols, function(x){
                    if(x %in% colnames()){
                      label = names(colnames())[which(colnames() == x)]
                    } else {
                      label = x
                    }
                    
                    shinyInput(
                        x = uiData[[x]],
                        inputId = ns(x),
                        label = label, 
                        selected = uiData[,x]
                    )
                  })
              inputs
            })
        
        DTInputs <- reactive({
              lapply(inputDTs(), function(inputDT){
                    selectInputDT_UI(id = ns(inputDT$id))                 
                  })
            })
        
        output$inputUI <- renderUI({
              do.call(tagList,c(normalInputs(), DTInputs()))
            })
        
        newData <- reactive({
              data <- data()
              
              # Table inputs
              for(DTinput in rv$inputDTData){
                newData <- DTinput()
                data[,base::colnames(newData)] <- newData
              }
              
              # Normal inputs
              for(col in intersect(base::colnames(data), names(input))){
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
#' @importFrom shiny numericInput dateInput selectInput textInput
#' @return shiny input
#' 
#' @author Jasper Schelfhout
shinyInput <- function(x, inputId, label, selected){
  if(inherits(x, "logical")){
    selectInput(inputId = inputId, label = label, choices = c(" " = NA, "TRUE" = TRUE, "FALSE" = FALSE), selected = selected)
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

