% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/foreignTbl.R
\name{joinForeignTbl}
\alias{joinForeignTbl}
\title{Merge a tbl with it a foreignTbl}
\usage{
joinForeignTbl(
  tbl,
  foreignTbl,
  keepNA = TRUE,
  by = foreignTbl$by,
  copy = TRUE,
  type = c("inner", "left")[1]
)
}
\arguments{
\item{tbl}{`tbl`}

\item{foreignTbl}{`tbl`}

\item{keepNA}{`boolean` keep rows from tbl with NA keys.}

\item{by}{named character}

\item{copy}{boolean}

\item{type}{character}
}
\value{
tibble `tbl`
}
\description{
Merge a tbl with it a foreignTbl
}
\examples{
\dontrun{
a <- tibble::as_tibble(data.frame(
    a = c(1,2,3),
    key1 = c(NA,2,4),
    key2 = c(NA,2,4)))

b <-  tibble::as_tibble(data.frame(
    b = c("a", "b", "c"),
    key1 = c(1,2,3),
    key2 = c(1,2,3)
))

foreignTbl <- foreignTbl(a,b)

joinForeignTbl(a, foreignTbl)

}

}
\author{
Jasper Schelfhout
}
