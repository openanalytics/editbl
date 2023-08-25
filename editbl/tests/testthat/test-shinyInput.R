test_that("Input generation for single column data.frame works", {
      data <- data.frame(x = as.numeric(1))
      
      # Try running the app manually
      ui <- inputUI.default(id = "app")
      server <- function(input,output,session){
        inputServer.default(id = "app", data = reactive(data))
      }
      shiny::shinyApp(ui, server)
      
      # Actual test
      shiny::testServer(
          app = inputServer.default,
          args = list(data = data),
          expr = {
            session$flushReact()
            session$setInputs(x = 2)
            expect_equal(newData(), data.frame(x = 2))
          })
    })
