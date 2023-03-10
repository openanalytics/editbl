% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/foreignTbl.R
\name{foreignTbl}
\alias{foreignTbl}
\title{Create a foreign tibble}
\usage{
foreignTbl(
  x,
  y,
  by = intersect(dplyr::tbl_vars(x), dplyr::tbl_vars(y)),
  naturalKey = dplyr::tbl_vars(y)
)
}
\arguments{
\item{x}{`tbl`. The child table.}

\item{y}{`tbl`. The referenced table.}

\item{by}{`character`. Column names to match on.
Note that you should rename and/or typecast the columns in y should they not exactly match the columns in x.}

\item{naturalKey}{`character`. The columns that form the natural key in y.
These are the only ones that can actually get modified, other columns will be deducted from these.}
}
\value{
list
}
\description{
Create a foreign tibble
}
\details{
this is a tibble that can be passed onto eDT as a reference.

It is the equivalent of a database table to which the core tbl of eDT has a foreign key.

It will be merged with the core tibble allowing to provide restrictions
for certain columns.

Note that it assumes that both the columns from `by` and `naturalKey` are unique in the foreignTbl.
This assumption will not be checked since it is an expensive operation on big datasets.
}
\author{
Jasper Schelfhout
}
