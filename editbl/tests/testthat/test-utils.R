test_that("standardizeColnamesArgument returns named character for named integer",{
     result <- standardizeColnamesArgument(c("test" = 1), dplyr::as_tibble(mtcars))
      expect_equal(result, c("test" = "mpg"))
    })

test_that("standardizeColnamesArgument returns named character for unnamed character",{
      result <- standardizeColnamesArgument(c("test"), dplyr::as_tibble(mtcars))
      expect_equal(result, c("test" = "mpg"))
    })

test_that("standardizeColnamesArgument returns named character for named character",{
      result <- standardizeColnamesArgument(c("test" = "mpg"), dplyr::as_tibble(mtcars))
      expect_equal(result, c("test" = "mpg"))
    })

test_that("castFromTbl can cast to data.frame",{
      result <- castFromTbl(tibble::as_tibble(mtcars), mtcars)
      expect_true(is.data.frame(result))
      expect_equal(colnames(result), colnames(mtcars))
    })

test_that("castFromTbl can cast to data.table",{
      result <- castFromTbl(tibble::as_tibble(mtcars), data.table::as.data.table(mtcars))
      expect_true(data.table::is.data.table(result))
      expect_equal(colnames(result), colnames(mtcars))
    })
