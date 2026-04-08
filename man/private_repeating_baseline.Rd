\name{.repeating_baseline}
\alias{.repeating_baseline}
\title{Check Baseline Variables for Repetition}
\usage{
.repeating_baseline(dataset_subset, type = c("across", "within", "across_rare"))
}
\arguments{
\item{dataset_subset}{A \code{data.frame} of clinical trial data subset to only the baseline variables.}
\item{type}{If \code{"across"}, across all baseline variables. If \code{"within"}, within each baseline variable. If \code{"across_rare"}, across the baseline variables but only for participants who had a rare outcome.}
}
\description{
Internal function documentation for developers. Essentially a wrapper around \code{get_dupes} of \href{https://cran.r-project.org/package=janitor}{janitor}.
}
\value{
A \code{data.frame} with one row for each repetition or just one row reporting Pass status for the check.
}
\examples{
library(readxl)
examplePath <- system.file("extdata", "dataset.xlsx", package = "integrity")
dataset <- read_excel(examplePath)
library(yaml)
infoPath <- system.file("extdata", "variables.yaml", package = "integrity")
info <- read_yaml(infoPath)
dataset_subset <- dataset[, unlist(info$baseline)]
integrity:::.repeating_baseline(dataset_subset)
}