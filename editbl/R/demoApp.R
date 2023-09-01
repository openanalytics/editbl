#' Run a demo app
#' @param app demoApp to run. Options: database / mtcars / custom
#' 
#' @details These apps are for illustrative purposes.
#' 
#' @param ... arguments passed onto the demoApp
#' @examples 
#' ## Only run this example in interactive R sessions
#' if(interactive()){
#'  # Database
#'  conn = connectDB()
#'  runDemoApp(app = "database", conn = conn)
#'  DBI::dbDisconnect(conn)
#' 
#'  # mtcars
#'  runDemoApp(app = "mtcars")
#'  
#'  # Any tibble of your liking
#'  runDemoApp(app = "custom", dplyr::tibble(iris))
#' }
#' @inherit shiny::shinyApp return
#' @export 
runDemoApp <- function(app = "database", ...){
  fn <- switch(app,
      "database" = runDemoApp_DB,
      "mtcars" = runDemoApp_mtcars,
      "custom" = runDemoApp_custom,
      stop("demoApp not found")
      )
  do.call(fn, list(...))
}

#' Run a demo app
#' @param conn database connection as provided by  \code{\link[DBI]{dbConnect}}
#' @importFrom shiny shinyApp
#' @inherit shiny::shinyApp return
runDemoApp_DB <- function(conn = connectDB()){
  ui <- demoUI_DB(id = "app", conn = conn)
  server <- function(input, output, session) {
    demoServer_DB(id = "app", conn = conn)
  }
  shinyApp(ui, server)  
}

#' Run a demo app
#' @importFrom shiny shinyApp
#' @inherit shiny::shinyApp return
runDemoApp_mtcars <- function(){
  ui <- demoUI_mtcars(id = "app")
  server <- function(input, output, session) {
    demoServer_mtcars(id = "app")
  }
  shinyApp(ui, server)  
}

#' Run a custom demo app
#' @param x `tbl`
#' @importFrom shiny shinyApp
#' @inherit shiny::shinyApp return
runDemoApp_custom <- function(x){
  ui <- demoUI_custom(id = "app")
  server <- function(input, output, session) {
    demoServer_custom(id = "app", x= x)
  }
  shinyApp(ui, server)  
}

#' UI of the DB demo app
#' @param id `character(1)`
#' @param conn database connection object as given by \code{\link[DBI]{dbConnect}}.
#' @importFrom shiny NS tagList selectInput verbatimTextOutput
#' @return HTML
#' 
#' @author Jasper Schelfhout
demoUI_DB <- function(id, conn) {
  ns <- NS(id)
  tagList(
      selectInput(
          inputId = ns("table"),
          label = "table",
          choices = DBI::dbListTables(conn)),
      eDTOutput(id = ns("editbl"))
  )
}

#' Server of the DB demo app
#' @param id `character(1)`
#' @param conn database connection object as given by \code{\link[DBI]{dbConnect}}.
#' @importFrom shiny reactive moduleServer renderPrint
#' @importFrom dplyr tbl
#' @return NULL, just executes the module server.
#' @author Jasper Schelfhout
demoServer_DB <- function(id, conn) {
  moduleServer(
      id,
      function(input, output, session) {
        data <- reactive({
              dplyr::tbl(conn, input$table)
            })
        
        modifiedData <- eDT(
            id = "editbl",
            data = data,
            in_place = TRUE 
            )

        invisible()
      }
  )
}

#' UI of the demo mtcars app
#' @param id `character(1)`
#' @importFrom shiny NS tagList
#' @return HTML
#' 
#' @author Jasper Schelfhout
demoUI_mtcars <- function(id) {
  demoUI_custom(id)
}

#' Server of the mtcars demo app
#' @param id `character(1)`
#' @importFrom dplyr tibble
#' @inherit demoServer_custom return
#' @author Jasper Schelfhout
demoServer_mtcars <- function(id) {
  demoServer_custom(id, dplyr::tibble(datasets::mtcars))
}

#' UI of the demo mtcars app
#' @param id `character(1)`
#' @importFrom shiny NS tagList
#' @return HTML
#' 
#' @author Jasper Schelfhout
demoUI_custom <- function(id) {
  ns <- NS(id)
  tagList(
      eDTOutput(id = ns("editbl")),
  )
}

#' Server of the mtcars demo app
#' @param id `character(1)`
#' @param x `tbl`
#' @return NULL, just executes the module server.
#' @author Jasper Schelfhout
demoServer_custom <- function(id, x) {
  moduleServer(
      id,
      function(input, output, session) {        
        modifiedData <- eDT(
            id = "editbl",
            data = x,
            in_place = FALSE 
        )$result
        
        invisible()
      }
  )
}
