\name{.terminal_digits}
\alias{.terminal_digits}
\title{Check Terminal Digits of Numerical Variables for Non-uniformity}
\usage{
.terminal_digits(dataset_subset)
}
\arguments{
\item{dataset_subset}{A \code{data.frame} of clinical trial data subset to only numeric columns.}
}
\description{
Internal function documentation for developers. Creates a distribution plot of terminal digits
}
\value{
A \code{ggplot2} plot.
}
\examples{
library(readxl)
examplePath <- system.file("extdata", "dataset.xlsx", package = "integrity")
dataset <- read_excel(examplePath)
library(yaml)
infoPath <- system.file("extdata", "variables.yaml", package = "integrity")
info <- read_yaml(infoPath)
numeric_columns <- info$baseline$numeric
dataset_subset <- dataset[, unlist(info$baseline)]
integrity:::.terminal_digits(dataset_subset)
}