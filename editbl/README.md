# {editbl}: DT extension for CRUD <img style="height: 139px;" src='https://github.com/openanalytics/editbl/blob/main/editbl_logo.svg?raw=true' align="right" />

[![CRAN status](https://www.r-pkg.org/badges/version/editbl)](https://cran.r-project.org/package=editbl)
[![R-CMD-check](https://github.com/openanalytics/editbl/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/openanalytics/editbl/actions/workflows/check-standard.yaml)
[![codecov](https://codecov.io/gh/openanalytics/editbl/branch/main/graph/badge.svg)](https://app.codecov.io/gh/openanalytics/editbl)
![CRAN downloads](https://cranlogs.r-pkg.org/badges/editbl)

`editbl` ('*edit [tibble](https://cran.r-project.org/package=tibble)*') allows you to modify tables in a spreadsheet-like fashion. Not just in-memory `data.frame` objects, but also
data living in a database.

## Installation

* From CRAN:

```
install.packages('editbl')
```

* Latest development version:

```
remotes::install_github("https://github.com/openanalytics/editbl", ref = "main", subdir = "editbl")
```

## Get started

Choose a dataset of your liking and use `eDT` to interactively explore and modify it!

```
modifiedData <- editbl::eDT(mtcars)
print(modifiedData)
```


Run some demo apps

```
editbl::runDemoApp()
```
![](https://github.com/openanalytics/editbl/blob/main/editbl.gif?raw=true)

More introductory examples can be found [here](https://github.com/openanalytics/editbl/blob/main/editbl/R/demoApp.R).
Advanced examples can be found in the [vignettes](https://github.com/openanalytics/editbl/tree/main/editbl/vignettes).


## Features

### Overview of main features

* Supporting multiple backends and in-place editing
* Customizable (lightweight [DT](https://CRAN.R-project.org/package=DT) wrapper)
* Easy integration in [shiny](https://cran.r-project.org/package=shiny) apps
* Undo/redo button
* Copy rows
* Drag cells
* No need to have all data in-memory
* Tackles challenges such as enforcing foreign keys and hiding of surrogate keys
* Transactional commits (currently for `tbl_dbi` class and non in-place editing)
* Default values for new rows (UUID's, 'current date', 'inserted by', ...)
* Possible to set row level security

### Constraints and normalized tables

Sometimes you want to restrict certain columns of your table to only contain specific values.
Many of these restrictions would be implemented at database level through the use of foreign keys to other tables.

`editbl` allows you to specify similar rules through the use of `foreignTbls` as an argument to `eDT()`.
Note that you can additionally hide surrogate keys by the use of `naturalKey` and `columnDefs` if you wish to.

```
a <- tibble::tibble(
    first_name = c("Albert","Donald","Mickey"),
    last_name_id = c(1,2,2)
  )

b <-  foreignTbl(
  a,
  tibble::tibble(
      last_name = c("Einstein", "Duck", "Mouse"),
      last_name_id = c(1,2,3)
    ),
  by = "last_name_id",
  naturalKey = "last_name"
)

eDT(a,
  foreignTbls = list(b),
  options = list(columnDefs = list(list(visible=FALSE, targets="last_name_id")))
)
```

### Support for different backends

`dplyr` code is used for all needed data manipulations and it is recommended to pass on your data as a `tbl`.
This allows editbl to support multiple backends through the usage of other packages like `dtplyr`, `dbplyr` etc.

In case you pass on other tabular objects like `data.frame` or `data.table` the function will internally automatically
cast back and forth to `tbl`. Small side effects may occur because of this (like loosing rownames), so it might be better
to cast yourself to `tbl` explicitly first.

```
# tibble support
modifiedData <- editbl::eDT(tibble::as_tibble(mtcars))

# data.frame support
modifiedData <- editbl::eDT(mtcars)

# data.table support
modifiedData <- editbl::eDT(data.table::data.table(mtcars))

# database support
tmpFile <- tempfile(fileext = ".sqlite")
file.copy(system.file("extdata", "chinook.sqlite", package = 'editbl'), tmpFile)
conn <- editbl::connectDB(dbname = tmpFile)
modifiedData <- editbl::eDT(dplyr::tbl(conn, "Artist"), in_place = TRUE)
DBI::dbDisconnect(conn)
unlink(tmpFile)

```

Note that there are some custom methods in the package itself for `rows_update` / `rows_delete` / `rows_insert`. The goal
would be to fully rely on `dplyr` once these functions are not experimental anymore and support all needed requirements.
These functions also explain the high amount of 'suggested' packages, while the core functionality of `editbl` has few
dependencies.

## Switching from DT

Let's say you already use `DT::datatable()` to display your data, but want to switch to `editbl::eDT()` to be able to edit it. Would this be a lot of effort? No!
In fact, `eDT()` accepts the exact same arguments. So it is almost as easy as replacing the functions and you are done.
Should you run into problems take a look [here](https://github.com/openanalytics/editbl/blob/main/editbl/vignettes/howto_switch_from_DT.rmd) for some pointers to look out for.

## Notes

* https://github.com/tidyverse/dtplyr/issues/260 might cause errors / warnings when using `eDT` with `dtplyr`. If possible convert to normal tibble first.
* `editbl` assumes that **all rows in your table are unique**. This assumption is the key (ba dum tss) to allow for only having the data partially in memory.
* `editbl` does not attempt to detect/give notifications on concurrent updates by other users to the same data, nor does it 'lock' the rows you are updating.
It just sends its updates to the backend by matching on the keys of a row. If other users have in the meantime made conflicting adjustments,
the changes you made might not be executed correctly or errors might be thrown.

## General future goals for this package

* Full `dplyr` compatibility so support for different backends is easily facilitated. Now there are 2 methods (`e_rows_update`, `e_rows_insert`) that need to be implemented to support a new backend.
* Full `DT` compatibility, including all extensions.
* Better editing / display options for time values. E.g. control over timezone and format of display / storage + nicer input forms.
* Any addition that supports the concept of editing data as flexible/easy as possible while respecting backend schema's and constraints.

## References

### Alternatives 

These are other popular CRUD packages in R.
Depending on your needs, they might be better alternatives.

[**DataEditR**](https://cran.r-project.org/package=DataEditR)

* Rstudio plugin
* Really flexible excel-like feeling
* Can only edit in-memory tables. Harder to support databases etc.

[**editData**](https://cran.r-project.org/package=editData)

* Rstudio plugin
* Nice features in terms of editing (pop-ups, more buttons,...)
* Can only edit in-memory tables. Harder to support databases etc.

[**Editor**](https://editor.datatables.net/)

*  Premium datatable extension allowing for editing data.

[**DT-Editor**](https://github.com/jienagu/DT-Editor)

* data.table focused

[**DTedit**](https://github.com/jbryer/DTedit)

* `DT` extension
* Very customizable (own callbacks)
* Few dependencies

### Additional links:
[CRAN `DT`](https://cran.r-project.org/package=DT)

[CRAN `tibble`](https://cran.r-project.org/package=tibble)

[Blogpost spreadsheets vs robust backends](https://www.openanalytics.eu/blog/2023/02/12/spreadsheets-backends-love/)

[Blogpost buttons in DT](https://thatdatatho.com/adding-action-buttons-in-rows-of-dt-data-table-in-r-shiny/)

[Blogpost shiny vs excel](https://appsilon.com/forget-about-excel-use-r-shiny-packages-instead/)

[Generic CRUD application](https://github.com/garrylachman/ElectroCRUD)

[Example SQLite database](https://www.sqlitetutorial.net/sqlite-sample-database/)
