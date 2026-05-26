\name{.unexpectedly_uncorrelated}
\alias{.unexpectedly_uncorrelated}
\title{Check Pairs of Variables Expected to be Correlated}
\usage{
.unexpectedly_uncorrelated(dataset_subset, pairs, alpha)
}
\arguments{
\item{dataset_subset}{A \code{data.frame} of clinical trial data subset to numeric columns.}
\item{pairs}{List of elements, each of length two. The elements are column names.}
\item{alpha}{p-value signficance threshold.}
}
\description{
Internal function documentation for developers. Essentially, \code{cor.test}.
}
\value{
A list of length two. \code{check_table}: One-row \code{data.frame} with a Pass or Fail indicator for each variable pair. \code{images}: Scatter plots.
}
\examples{
library(readxl)
examplePath <- system.file("extdata", "dataset.xlsx", package = "integrity")
dataset <- read_excel(examplePath)
library(yaml)
infoPath <- system.file("extdata", "variables.yaml", package = "integrity")
info <- read_yaml(infoPath)
integrity:::.unexpectedly_uncorrelated(dataset, info$correlated, 0.05)
}