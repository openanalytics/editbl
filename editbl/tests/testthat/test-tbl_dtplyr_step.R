test_that("rows_insert add row to data.table", {
      x <- dtplyr::lazy_dt(data.table::data.table(mtcars))
      y <- mtcars[1,]
      result <- rows_insert(x,y)
      expect_equal(nrow(x) + 1, nrow(result))
    })

test_that("rows_update can update a row in a data.table", {
      dt <- data.table::data.table(mtcars)
      dt$id <- seq_len(nrow(dt))
      x <- dtplyr::lazy_dt(dt)
      y <- dt[1,]
      y$mpg <- 1000
      
      result <- rows_update(x = x, y = y, by = "id")
      expect_equal(data.table::as.data.table(result)[1,"mpg"][[1]], 1000)
    })

test_that("rows_delete can delete a row in a data.table", {
      dt <- data.table::data.table(mtcars)
      dt$id <- seq_len(nrow(dt))
      x <- dtplyr::lazy_dt(dt)
      y <- dt[1,]
      y$mpg <- 1000
      
      result <- rows_delete(x = x, y = y, by = "id")
      expect_equal(nrow(result), nrow(dt) - 1)
    })

