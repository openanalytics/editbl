test_that("Starting demo app db works", {
      runDemoApp (app = "database")
    })

test_that("Starting demo app mtcars works", {
      runDemoApp (app = "mtcars")
    })

test_that("Starting demo app custom works", {
      runDemoApp(app = "custom", dplyr::tibble(iris))
    })