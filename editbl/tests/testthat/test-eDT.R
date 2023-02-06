test_that("passing on an empty tibble works", {
      data <- dplyr::as_tibble(sleep)
      data <- data[NULL,]
      
      # Try running the app manually
      ui <- eDTOutput("app")
      server <- function(input,output,session){
        eDTServer(id = "app", data = reactive(data))
      }
      shiny::shinyApp(ui, server)
      
      # Actual test
      shiny::testServer(
          app = eDTServer,
          args = list(data = data),
          expr = {
            session$setInputs(addRow = 1)       
            session$setInputs(commit = 1)
            session$setInputs(confirmCommit = 1)
            expect_true(nrow(rv$committedData) == 1)
          })
})
