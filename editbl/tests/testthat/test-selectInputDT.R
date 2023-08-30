test_that("selectInputDT works", {  
      # Try running the app manually
      df <- data.frame(
          id = 1:10,
          name = letters[1:10]
          )
      
      ui <- selectInputDT_UI("app")
      
      server <- function(input,output,session){
        selectInputDT_Server(id = "app", label = "test", choices = df, selected = df[4,])
      }
      shiny::shinyApp(ui, server)
      
      # Actual test
      shiny::testServer(
          app = selectInputDT_Server,
          args = list(id = "app", label = "test", choices = df, selected = df[4,]),
          expr = {
            session$setInputs(DT_rows_selected = 2)                   
            expect_equal(data_selection_first()[input$DT_rows_selected,]$name, "a") # Not 'b' because selection always comes first
          })
    })
