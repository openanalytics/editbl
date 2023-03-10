% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eDT.R
\name{initData}
\alias{initData}
\title{Add some extra columns to data to allow for / keep track of modifications}
\usage{
initData(
  data,
  ns,
  buttonCol = "buttons",
  statusCol = "status",
  deleteCol = "deleted",
  iCol = "i"
)
}
\arguments{
\item{data}{data.frame}

\item{ns}{namespace function}

\item{buttonCol}{character name of column with buttons}

\item{statusCol}{character name of column with general status (e.g. modified or not).}

\item{deleteCol}{character name of the column with deletion status.}

\item{iCol}{name of column with i}
}
\value{
data with extra columns buttons, status, i.
}
\description{
Add some extra columns to data to allow for / keep track of modifications
}
\author{
Jasper Schelfhout
}
