\name{.implausible_values}
\alias{.implausible_values}
\title{Check Variables for Implausible Values}
\usage{
.implausible_values(dataset, participantID, unexpected, enrollment)
}
\arguments{
\item{dataset}{A \code{data.frame} of clinical trial data.}
\item{participantID}{Column name of column storing participant IDs.}
\item{unexpected}{List of elements specifying implausible values. Names of list are column names}
\item{enrollment}{Column name of column storing enrollment dates.}
}
\description{
Internal function documentation for developers. Each column is checked for violations.
}
\value{
A \code{data.frame} with one row for each violation or one row with Pass if no rows violated the check.
}
\examples{
library(readxl)
examplePath <- system.file("extdata", "dataset.xlsx", package = "integrity")
dataset <- read_excel(examplePath)
library(yaml)
infoPath <- system.file("extdata", "variables.yaml", package = "integrity")
info <- read_yaml(infoPath)
integrity:::.implausible_values(dataset, info$participantID, info$unexpected, info$enrollment)
}