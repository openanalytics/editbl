test_that("passing on a foreign tibble works", {
      a <- tibble::tibble(
              a = c(1,2,3),
              key1 = c(NA,2,4),
              key2 = c(NA,2,4))
      
      b <-  foreignTbl(
          a,
          tibble::tibble(
                  b = c("a", "b", "c"),
                  key1 = c(1,2,3),
                  key2 = c(1,2,3)
              ),
          naturalKey = "b"
      )
      
      # Try running the app manually
      ui <- eDTOutput("app")
      server <- function(input,output,session){
        eDT(id = "app",
            data = reactive(a),
            foreignTbls = list(b),
            options = list(columnDefs = list(list(visible=FALSE, targets="key2"))))
      }
      shiny::shinyApp(ui, server)
      
      # Actual test
      shiny::testServer(
          app = eDTServer,
          args = list(data = reactive(a), foreignTbls = list(b)),
          expr = {
            expect_true(TRUE)
          })
})

test_that("Can use foreign tibbles to fill in non natural key values.",{
      tbl <- tibble::tibble(
              a = c(1,2),
              key1 = as.double(c(NA,NA)),
              key2 = as.double(c(NA,NA))
          )
      
      merged_tbl <- cbind(tbl, tibble(
              b = c("b1", "b2"), 
              c = c("c1", "c2")))

      
      b <-  foreignTbl(
          tbl,
          tibble::tibble(
                  b = c("b1", "b2", "b3"),
                  key1 = c(1,2,3)
              ),
          by = "key1",
          naturalKey = "b"
      )
      
      c <-  foreignTbl(
          tbl,
          tibble::tibble(
                  c = c("c1", "c2", "c3"),
                  key2 = c(1,2,3)
              ),
          by = "key2",
          naturalKey = "c"
      )
      
      foreignTbls = list(b, c)
      
      result <- fillDeductedColumns(merged_tbl, foreignTbls)
      
      expect_equal(result$key1, c(1,2))
      expect_equal(result$key2, c(1,2))
    })

test_that("Contradicting foreign tibbles give error when filling in data.",{
      tbl <- tibble::tibble(
              a = c(1,2),
              key = as.double(c(NA,NA))
          )
      
      merged_tbl <- tibble::tibble(cbind(tbl,
              tibble::tibble(color = c("blue", "orange"))
          ))
      
      b <-  foreignTbl(
          tbl,
          tibble::tibble(
                  color = c("blue", "orange"),
                  key = c(1,2)
              ),
          by = "key",
          naturalKey = "color"
      )
      
      c <-  foreignTbl(
          tbl,
          tibble::tibble(
                  color = c("blue", "purple"),
                  key = c(2,1)
              ),
          by = "key",
          naturalKey = "color"
      )
            
      expect_error({fillDeductedColumns(tbl, foreignTbls = list(b,c))})
      expect_error({fillDeductedColumns(tbl, foreignTbls = list(c,b))})
      
    })

test_that("Empty foreign tibbles list returns given tibble",{
      tbl <- tibble::tibble(
              a = c(1,2),
              key = as.double(c(NA,NA)),
              color = c("blue", "orange")
          )
          
      result <- fillDeductedColumns(tbl, foreignTbls = list())
      expect_equal(result, tbl)
    })

test_that("checkForeignTbls throws error",{
      
      tbl <-  tibble::tibble(
              a = c(1,2, 3),
              key = c(1,2, 3)
          )
      
      merged_tbl <- cbind(tbl,
              tibble::tibble(color = c("blue", "orange", "purple"))
          )
      
      b <-  foreignTbl(
          tbl,
          tibble::tibble(
                  color = c("blue", "orange"),
                  key = c(1,2)
              ),
          by = "key",
          naturalKey = "color"
      )
      
      expect_error(checkForeignTbls(tbl = merged_tbl,foreignTbls = list(b)))
    })

test_that("joinForeignTbl() type 'inner' works.",{
      a <- tibble::tibble(
           a = c(1,2,3),
           key1 = c(NA,2,4),
           key2 = c(NA,2,4))
       
       b <-  tibble::tibble(
           b = c("a", "b", "c"),
           key1 = c(1,2,3),
           key2 = c(1,2,3)
       )
       
       foreignTbl <- foreignTbl(a,b)
       
       result <- joinForeignTbl(a, foreignTbl, type = 'inner')
       
       # NA keys in 'a' are expected to stay.
       expect_equal(colnames(result), c("a", "key1", "key2", "b"))
       expect_true(nrow(result) == 2)
    })

test_that("joinForeignTbl() type 'left' works.",{
      a <- tibble::tibble(
          a = c(1,2,3),
          key1 = c(NA,2,4),
          key2 = c(NA,2,4))
      
      b <-  tibble::tibble(
          b = c("a", "b", "c"),
          key1 = c(1,2,3),
          key2 = c(1,2,3)
      )
      
      foreignTbl <- foreignTbl(a,b)
      
      result <- joinForeignTbl(a, foreignTbl, type = 'left')
      
      expect_equal(colnames(result), c("a", "key1", "key2", "b"))
      expect_true(nrow(result) == 3)
    })

