conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
test_df <- data.frame(
    id = 1:3,
    name = letters[1:3]
    )
DBI::dbCreateTable(conn, "test", test_df)
DBI::dbAppendTable(conn, "test", test_df)

test_tbl <- dplyr::tbl(conn, "test")

test_that("rows_insert.tbl_dbi works",{
      rows_insert.tbl_dbi(test_tbl, data.frame(id = 4, name = "insert"), in_place = TRUE)
      result <- DBI::dbGetQuery(conn, "select * from test where id = 4")$name
      expect_equal(result,'insert')
    })

test_that("rows_update.tbl_dbi works",{
      rows_update.tbl_dbi(test_tbl, data.frame(id = 1, name = "update"), by = "id", in_place = TRUE)
      result <- DBI::dbGetQuery(conn, "select * from test where id = 1")$name
      expect_equal(result,'update')
    })

test_that("rows_delete.tbl_dbi works",{
      rows_delete.tbl_dbi(test_tbl, data.frame(id = 2), in_place = TRUE)
      result <- DBI::dbGetQuery(conn, "select * from test where id = 2")
      expect_true(nrow(result) == 0)
    })

DBI::dbDisconnect(conn)
