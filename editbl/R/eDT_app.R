#' Open interactive app to explore and modify data
#' @param ... arguments past to eDT
#' @importFrom shiny shinyApp
#' @return data (or a modified version thereof)
eDT_app <- function(...){
  args <- list(...)
  ui <- eDT_app_ui(eDTId = args$id)
  server <- function(input, output, session) {
    do.call(eDT_app_server, args)
  }
  shiny::runApp(list(ui = ui, server = server))  
}

#' UI of eDT_app
#' @param moduleId character
#' @param eDTId character
#' @importFrom shiny NS tagList actionButton
#' @return HTML
#' 
#' @author Jasper Schelfhout
eDT_app_ui <- function(moduleId = "nevergonnagiveyouup", eDTId = "nevergonnaletyoudown") {
  ns <- NS(moduleId)
  tagList(
      actionButton(inputId = ns("close"), "label" = "close"),
      eDTOutput(id = ns(eDTId)),
  )
}

#' Server of eDT_app
#' @param moduleId character
#' @param ... arguments passed to \link{eDT}
#' @importFrom shiny reactive moduleServer observeEvent stopApp reactiveValues
#' @importFrom dplyr tbl
#' @return moduleServer which on application stop returns version of x with made changes
#' 
#' @author Jasper Schelfhout
eDT_app_server <- function(moduleId =  "nevergonnagiveyouup",...) {
  args <- list(...)
  moduleServer(
      moduleId,
      function(input, output, session) {  
        modifiedData <- do.call(eDTServer, args)
        
        observeEvent(input$close, {
              shiny::stopApp(modifiedData())
            })
      }
  )
}
