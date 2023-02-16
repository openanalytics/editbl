test_that("Starting demo app db works", {
      runDemoApp (app = "database")
      expect_true(TRUE)
    })

test_that("Starting demo app mtcars works", {
      runDemoApp (app = "mtcars")
      expect_true(TRUE)
    })

test_that("Starting demo app custom works", {
      runDemoApp(app = "custom", dplyr::tibble(iris))
      expect_true(TRUE)
    })