#' UI part of a DT select input
#' @param id `character(1)` same one as used in \code{\link{selectInputDT_Server}}
#' @importFrom shiny NS uiOutput
#' @return HTML
#' @inherit selectInputDT_Server examples
#' @export
#' @author Jasper Schelfhout
selectInputDT_UI <- function(id){
  ns <- NS(id)
  uiOutput(ns("DT_UI"))
}

#' Server part to use a \code{\link[DT]{datatable}} as select input
#' 
#' @seealso `shiny::selectInput`. This function can be more convenient for selecting rows
#' with multiple columns.
#' 
#' @param id `character(1)` same one as used in \code{\link{selectInputDT_UI}}
#' @param choices `data.frame`
#' @param label `character(1)`
#' @param selected `data.frame` with rows available in `choices`.
#' @param multiple `logical`. Whether or not multiple row selection is allowed
#' 
#' @examples
#' ## Only run this example in interactive R sessions
#' if(interactive()){
#'   ui <- selectInputDT_UI('id')
#'   data <- data.frame(id = 1:3, name = letters[1:3])
#'   server <- function(input,output, session){
#'     selected = selectInputDT_Server('id', choices = data, selected = data[1,] )
#'     observe({print(selected())})
#'   }
#'   shiny::shinyApp(ui, server)
#' 
#' }
#' @importFrom shiny is.reactive reactive renderUI moduleServer
#' @importFrom DT renderDataTable datatable
#' @return A selection of rows from the `data.frame` provided under choices.
#' @export 
#' @author Jasper Schelfhout
selectInputDT_Server <- function(id,
    label = "",
    choices,
    selected = NULL,
    multiple = FALSE){
  moduleServer(
      id,
      function(input,output,session){
        # Make arguments reactive
        # Need to be explicit about environement. Otherwhise they overwrite themselves.
        # This way users can pass on both reactive an non reactive arguments
        argEnv <- parent.frame(3)
        
        if(!shiny::is.reactive(label)){
          label <- shiny::reactive(label, env = argEnv)
        }
        
        if(!shiny::is.reactive(choices)){
          choices <- shiny::reactive(choices, env = argEnv)
        }
        
        if(!shiny::is.reactive(selected)){
          selected <- shiny::reactive(selected, env = argEnv)
        }
        
        if(!shiny::is.reactive(multiple)){
          multiple <- shiny::reactive(multiple, env = argEnv)
        }
        
        observe({
              if(!multiple() && nrow(selected()) > 1){
                stop("Can not have more than 1 row selected.")
              }
            })
        
        ns <- session$ns
        
        hasSelection <- reactive({
              if(is.null(selected())){
                FALSE
              } else if ({ all(is.na(selected()))}){
                FALSE
              } else {
                TRUE
              }
            })
        
        data_selection_first <- reactive({
              if(hasSelection()){
                dt <- unique(rbind(selected(), choices()))
              } else {
                dt <- choices()
              }
              dt
            })
        
        output$DT_UI <- renderUI({
              tagList(
                  label(),
                  DT::DTOutput(ns("DT"))
              )
            })
        
        mode <- reactive({
              if(multiple()){
                mode = "multiple"
              } else {
                mode = "single"
              }
              mode
            })
        
        rowNrs <- reactive({
              if (hasSelection()){
                rowNrs = seq_len(nrow(selected()))
              } else {
                c()
              }
              rowNrs
            })
        
        output$DT <- DT::renderDataTable({
             data <- data_selection_first()             
              DT::datatable(
                  data,
                  rownames = TRUE,
                  options = list(
                      scrollX = TRUE,
                      columnDefs = list(
                          list(
                              visible = FALSE,
                              targets = c(0)) # hide row names https://github.com/rstudio/DT/issues/945
                      )
                  ),
                  filter = "top",
                  selection = list(
                      mode = mode(),
                      selected = rowNrs(),
                      target = 'row'))
            })
        
        reactive({
              data_selection_first()[input$DT_rows_selected,]
            })
      })
}
