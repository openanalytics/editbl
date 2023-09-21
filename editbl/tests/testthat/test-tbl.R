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
      
      # rows_delete will start nested transaction if 'y' is not of same source as 'x'
      dbplyr::db_copy_to(conn, table = 'y1_temp', values = data.frame(id = 2), in_transaction = FALSE)
      y <- tbl(conn, 'y1_temp')
      
      rows_delete(x = test_tbl, y = y, in_place = TRUE, copy = TRUE, unmatched = "ignore")
      commitTransaction(test_tbl)
      result <- DBI::dbGetQuery(conn, "select * from test")
      expect_true(nrow(result) == 2)
    })

test_that("beginTransaction and rollbackTransaction works for tbl_dbi",{
      beginTransaction(test_tbl)
      
      # rows_delete will start nested transaction if 'y' is not of same source as 'x'
      dbplyr::db_copy_to(conn, table = 'y2_temp', values = data.frame(id = 1), in_transaction = FALSE)
      y <- tbl(conn, 'y2_temp')
      rows_delete(x = test_tbl, y = y, in_place = TRUE, copy = TRUE, unmatched = "ignore")
      
      rollbackTransaction(test_tbl)
      result <- DBI::dbGetQuery(conn, "select * from test")
      expect_true(nrow(result) == 2)
    })

DBI::dbDisconnect(conn)
