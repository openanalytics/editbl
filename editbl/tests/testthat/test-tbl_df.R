test_that("rows_update can update a row in a data.frame", {
      x <- mtcars
      x$id <- seq_len(nrow(x))
      y <- x[1,]
      y$mpg <- 1000
      
      result <- rows_update(x = x, y = y, by = "id")
      expect_equal(result[1,"mpg"][[1]], 1000)
    })
