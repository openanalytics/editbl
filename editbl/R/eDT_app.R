#' Open interactive app to explore and modify data
#' 
#' @details When \code{\link{eDT}} is not used within the server of a shiny app, it will
#' call this function to start up a shiny app itself. Just as `DT::datatable()` displays a table
#' in the browser when called upon interactively.
#' 
#' @param ... arguments past to \code{\link{eDT}}
#' @importFrom shiny shinyApp
#' @return data (or a modified version thereof) once you click 'close'
eDT_app <- function(...){
  args <- list(...)
  ui <- eDT_app_ui(eDTId = args$id)
  server <- function(input, output, session) {
    do.call(eDT_app_server, args)
  }
  shiny::runApp(list(ui = ui, server = server))  
}

#' UI of eDT_app
#' @param moduleId `character(1)` id to connect with eDT_app_server
#' @param eDTId `character(1)` id to connect \code{\link{eDTOutput}} to \code{\link{eDT}} within the module.
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
#' @param moduleId `character(1)` id to connect with eDT_app_server
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
        modifiedData <- do.call(eDTServer, args)$result
        
        observeEvent(input$close, {
              shiny::stopApp(modifiedData())
            })
      }
  )
}
