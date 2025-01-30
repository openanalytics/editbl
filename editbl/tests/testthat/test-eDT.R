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
            session$flushReact()
            test_id = rv$modifiedData[1,"_editbl_identity"] # generated uuid
            session$setInputs(current_id = paste0('delete_row_', test_id))
            session$setInputs(delete = 1)
            session$flushReact()
            session$setInputs(confirmCommit = 1)
            expect_equal(nrow(result()),1)
          }
      )
    })

test_that("Can not delete row when canDeleteRow blocks it", {
      data <- dplyr::tibble(id = 1:2, name = letters[1:2])
      
      shiny::testServer(
          app = eDTServer,
          args = list(data = data, canDeleteRow = function(...){FALSE}),
          expr = {
            session$flushReact()
            test_id = rv$modifiedData[1,"_editbl_identity"] # generated uuid
            session$setInputs(current_id = paste0('delete_row_', test_id))
            session$setInputs(delete = 1)
            session$flushReact()
            session$setInputs(confirmCommit = 1)
            expect_equal(nrow(result()),2)
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

test_that("Row dragging works when filter is on", {
      ui <- eDTOutput("app")
      server <- function(input,output,session){
        eDTServer(id = "app",
            data = mtcars,
            filter = "bottom")
      }

      # Test
      # 1. add on a filter for a column
      # 2. drag a cell using making use of the autofill extension
      # 3. remove the filter
      # 4. check if the same cell is still modified 
      # TODO: try to encode this
      expect_true(TRUE)
      
      shiny::shinyApp(ui, server)
    })

test_that("Can support all data types",{
      df = tibble(
          integer = 1L,
          double = 0.5,
          time = as.POSIXct('2020-01-01 01:02:03'),
          date = as.Date('2020-01-01')
          )
          ui <- eDTOutput("app")
          server <- function(input,output,session){
            eDTServer(id = "app",data = df)
          }
          
          # This just ensures the app doesn't crash on these different
          # data types
          expect_true(TRUE)
          
          shiny::shinyApp(ui, server)
    })
