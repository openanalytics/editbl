#' Create a DT select input
#' @param id character
#' 
#' @author Jasper Schelfhout
selectInputDT_UI <- function(id){
  ns <- NS(id)
  uiOutput(ns("DT_UI"))
}

#' Server to use a datatable as select input
#' @param id character
#' @param choices data.frame
#' @param label character
#' @param selected data.frame single row
#' @param multiple logical
#' @return data.frame single row, new selection
#' 
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
        args <- c(as.list(argEnv))
        args$id <- NULL
        for(arg in names(args)){
          if(!shiny::is.reactive(args[[arg]])){
            eval(parse(text = sprintf("assign(arg, shiny::reactive(%s, env = argEnv))", arg)))
          }
        }
        
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
              DT::datatable(
                  data_selection_first(),
                  rownames = TRUE, # otherwhise buggy selection: https://github.com/rstudio/DT/issues/945
                  options = list(scrollX = TRUE),
                  filter = "top",
                  selection = list(
                      mode = mode(),
                      selected = rowNrs(),
                      target = 'row'))
            })
        
        reactive({
              choices()[input$DT_rows_selected,]
            })
      })
}