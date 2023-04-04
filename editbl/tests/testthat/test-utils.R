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

test_that("castToTemplate can cast from data.frame to data.table",{
      result <- castToTemplate(x = iris, template = data.table::data.table(iris))
      expect_equal(result, data.table::as.data.table(iris))
    })

test_that("castToTemplate can cast from data.frame to tbl",{
      result <- castToTemplate(x = iris,  template = dplyr::as_tibble(iris))
      expect_equal(result, dplyr::as_tibble(iris))
    })

test_that("castToTemplate can cast from tbl to data.frame",{
      result <- castToTemplate(x = dplyr::as_tibble(iris), template = iris)
      expect_equal(result, iris)
    })

test_that("castToTemplate can cast from data.table to data.frame",{
      result <- castToTemplate(x = data.table::as.data.table(iris), template = iris)
      expect_equal(result, iris)
    })

test_that("castToTemplate can cast from data.table to tbl",{
      result <- castToTemplate(x = data.table::as.data.table(iris), template = dplyr::as_tibble(iris))
      expect_equal(result, dplyr::as_tibble(iris))
    })

test_that("castToTemplate can cast from tbl to data.table",{
      result <- castToTemplate(x = dplyr::as_tibble(iris), template = data.table::as.data.table(iris))
      expect_equal(result, data.table::as.data.table(iris))
    })

test_that("standardizeArgument_colnames returns named character for named integer",{
      result <- standardizeArgument_colnames(c("test" = 1), dplyr::as_tibble(mtcars))
      expect_equal(result, c("test" = "mpg"))
    })

test_that("standardizeArgument_colnames returns named character for unnamed character",{
      result <- standardizeArgument_colnames(c("test"), dplyr::as_tibble(mtcars))
      expect_equal(result, c("test" = "mpg"))
    })

test_that("standardizeArgument_colnames returns named character for named character",{
      result <- standardizeArgument_colnames(c("test" = "mpg"), dplyr::as_tibble(mtcars))
      expect_equal(result, c("test" = "mpg"))
    })

test_that("standardizeArgument_editable TRUE",{
      result <- standardizeArgument_editable(TRUE)
      expect_equal(result, list(target = "cell"))
    })

test_that("standardizeArgument_editable FALSE",{
      result <- standardizeArgument_editable(FALSE, mtcars)
      expect_equal(result, list(target = "cell", disable = list(columns = 1:11)))
    })

test_that("standardizeArgument_editable 'row'",{
      result <- standardizeArgument_editable('row')
      expect_equal(result, list(target = "row"))
    })

test_that("standardizeArgument_editable list",{
      result <- standardizeArgument_editable(list(target = "cell", numeric = 2))
      expect_equal(result, list(target = "cell", numeric = 2))
    })

test_that("standardizeArgument_editable Error",{
      expect_error(standardizeArgument_editable(1))
    })

