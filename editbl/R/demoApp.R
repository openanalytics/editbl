#' Run a demo app
#' @param app demoApp to run. Options: database / mtcars / custom
#' @param ... arguments passed onto demoApp
#' @examples 
#' \dontrun{
#'  runDemoApp(app = "database")
#'  runDemoApp(app = "mtcars")
#'  runDemoApp(app = "custom", dplyr::tibble(iris))
#' 
#' }
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
#' @importFrom shiny shinyApp
runDemoApp_DB <- function(){
  conn <- connectDB()
  ui <- demoUI_DB(id = "app", conn = conn)
  server <- function(input, output, session) {
    demoServer_DB(id = "app", conn = conn)
  }
  shinyApp(ui, server)  
}

#' Run a demo app
#' @importFrom shiny shinyApp
runDemoApp_mtcars <- function(){
  ui <- demoUI_mtcars(id = "app")
  server <- function(input, output, session) {
    demoServer_mtcars(id = "app")
  }
  shinyApp(ui, server)  
}

#' Run a custom demo app
#' @param x tbl
#' @importFrom shiny shinyApp
runDemoApp_custom <- function(x){
  ui <- demoUI_custom(id = "app")
  server <- function(input, output, session) {
    demoServer_custom(id = "app", x= x)
  }
  shinyApp(ui, server)  
}

#' UI of the DB demo app
#' @param id character
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
#' @param id character
#' @param conn database connection object as given by \code{\link[DBI]{dbConnect}}.
#' @importFrom shiny reactive moduleServer renderPrint
#' @importFrom dplyr tbl
#' 
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
            in_place = reactive(TRUE) 
            )

        invisible()
      }
  )
}

#' UI of the demo mtcars app
#' @param id character
#' @importFrom shiny NS tagList
#' @return HTML
#' 
#' @author Jasper Schelfhout
demoUI_mtcars <- function(id) {
  demoUI_custom(id)
}

#' Server of the mtcars demo app
#' @param id character
#' @importFrom dplyr tbl
#' 
#' @author Jasper Schelfhout
demoServer_mtcars <- function(id) {
  demoServer_custom(id, dplyr::tibble(datasets::mtcars))
}

#' UI of the demo mtcars app
#' @param id character
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
#' @param id character
#' @param x tbl
#' @importFrom shiny reactive moduleServer renderPrint
#' @importFrom dplyr tbl
#' 
#' @author Jasper Schelfhout
demoServer_custom <- function(id, x) {
  moduleServer(
      id,
      function(input, output, session) {
        data <- reactive({x})
        
        modifiedData <- eDT(
            id = "editbl",
            data = data,
            in_place = reactive(FALSE) 
        )
        
        invisible()
      }
  )
}
