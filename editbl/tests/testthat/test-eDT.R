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


test_that("working with selectInputDT works.", {
      songs <- dplyr::as_tibble(data.frame(
          song = c("Never gonna give you up", "Self Esteem"),
          artist_id = c(1,2)
          ))
          
      artists <- dplyr::as_tibble(data.frame(
         artist_id = c(1,2),
         first_name = c("Rick", "Dexter"),
         last_name = c('Astley', "Holland")
      ))
      
      # Try running the app manually
      ui <- eDTOutput("app")
      server <- function(input,output,session){
        eDTServer(id = "app",
            data = songs,
            foreignTbls = list(
                foreignTbl(songs, artists, by = "artist_id", naturalKey = c("first_name", "last_name"))
                ))
      }
      
      # Test if using edit a second time still works
      # Reactivity problem.
      # TODO: write proper tests
      
      expect_true(TRUE)
      
      shiny::shinyApp(ui, server)
    })
