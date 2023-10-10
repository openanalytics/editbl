test_that("passing on an empty tibble works", {
      data <- dplyr::as_tibble(sleep)
      data <- data[0,]
      
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
            session$setInputs(add = 1)       
            session$setInputs(save = 1)
            session$setInputs(confirmCommit = 1)
            expect_true(nrow(rv$committedData) == 1)
          })
})

test_that("Editing a single table column works", {
      data <- dplyr::tibble(one_column = c(1,2))
      
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
            session$setInputs(edit_row_1 = 1)       
            session$setInputs(confirmCommit = 1)
          })
      expect_true(TRUE)
    })

test_that("Deletion of a row works", {
      data <- dplyr::tibble(id = 1:2, name = letters[1:2])
      
      shiny::testServer(
          app = eDTServer,
          args = list(data = data),
          expr = {
            session$setInputs(current_id = 'delete_row_1')
            session$setInputs(delete = 1)
            session$flushReact()
            session$setInputs(confirmCommit = 1)
            expect_equal(nrow(result()),1)
          }
      )
    })

test_that("working with selectInputDT works.", {
      songs <- tibble::tibble(
          song = c("Never gonna give you up", "Self Esteem"),
          artist_id = c(1,2)
          )
          
      artists <- dplyr::tibble(
         artist_id = c(1,2),
         first_name = c("Rick", "Dexter"),
         last_name = c('Astley', "Holland")
      )
      
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
