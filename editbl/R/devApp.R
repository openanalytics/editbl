#' Run a development app
#' @details This app prints some of the server objects and has a button to interactively browse the code.
#' This is useful for debugging and experimenting with new features.
#' 
#' @importFrom shiny shinyApp
#' @inherit shiny::shinyApp return
runDevApp <- function(){
  conn <- connectDB()
  ui <- devUI(id = "app", conn = conn)
  server <- function(input, output, session) {
    devServer(id = "app", conn = conn)
  }
  shinyApp(ui, server)  
}

#' UI of the development app
#' @param id `character(1)`
#' @param conn database connection object as given by \code{\link[DBI]{dbConnect}}.
#' @importFrom shiny NS tagList selectInput verbatimTextOutput actionButton
#' @return HTML
#' 
#' @author Jasper Schelfhout
devUI <- function(id, conn) {
  ns <- NS(id)
  tagList(
      actionButton(ns("browse"), label = "browse parent"),
      selectInput(
          inputId = ns("table"),
          label = "table",
          choices = DBI::dbListTables(conn)),
      eDTOutput(id = ns("editbl")),
      verbatimTextOutput(ns("modifiedData"))
  )
}

#' Server of the development app
#' @param id `character(1)`
#' @param conn database connection object as given by \code{\link[DBI]{dbConnect}}.
#' @importFrom shiny reactive moduleServer renderPrint
#' @importFrom dplyr tbl
#' @return NULL, just executes the module server.
#' @author Jasper Schelfhout
devServer <- function(id, conn) {
  moduleServer(
      id,
      function(input, output, session) {
        data <- reactive({
              dplyr::tbl(conn, input$table)
            })
        
        modifiedData <- eDT(
            id = "editbl",
            data = data,
            options = list(
                columnDefs = list(list(
                        visible = TRUE,
                        targets = "_all"))),
            in_place = TRUE,
            filter = "top"
        )$result
        
        
        observeEvent(input$browse,{
              browser()
            })
        
        output$modifiedData <- renderPrint({
              str(modifiedData())
            })
        invisible()
      }
  )
}