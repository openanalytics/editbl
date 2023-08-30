conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
test_df <- data.frame(
    id = 1:3,
    name = letters[1:3]
)
DBI::dbCreateTable(conn, "test", test_df)
DBI::dbAppendTable(conn, "test", test_df)

test_tbl <- dplyr::tbl(conn, "test")

test_that("beginTransaction and commitTransaction works for tbl_dbi",{
      beginTransaction(test_tbl)
      rows_delete.tbl_dbi(test_tbl, data.frame(id = 2), in_place = TRUE)
      commitTransaction(test_tbl)
      result <- DBI::dbGetQuery(conn, "select * from test")
      expect_true(nrow(result) == 2)
    })

test_that("beginTransaction and rollbackTransaction works for tbl_dbi",{
      beginTransaction(test_tbl)
      rows_delete.tbl_dbi(test_tbl, data.frame(id = 1), in_place = TRUE)
      rollbackTransaction(test_tbl)
      result <- DBI::dbGetQuery(conn, "select * from test")
      expect_true(nrow(result) == 2)
    })

DBI::dbDisconnect(conn)
